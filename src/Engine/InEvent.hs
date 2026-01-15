module Engine.InEvent
where

import PreludeL
import Control.Monad (when, filterM, foldM_)
import Control.Monad.State (modify, gets, forM_)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (sort, find, deleteBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Data.Maze
import Data.World
import Data.Primitive
import Engine.GameAuto
import Engine.Utils
import qualified Data.GameEvent as Ev
import qualified Data.Characters as Chara
import qualified Data.Spells as Spell

import Control.CUI (translate)

-- =======================================================================

doEvent :: Maybe CharacterID
        -> Ev.Define
        -> (Bool -> GameMachine)                        -- ^ when Escape Event
        -> (Bool -> GameMachine)                        -- ^ when End Event
        -> (Spell.Define -> GameMachine -> GameMachine) -- ^ GameMachine for spelling
        -> GameMachine
doEvent cidRep edef whenEscape whenEnd spelling = GameAuto $ do
    cid <- characterIDInPartyAt F1
    run $ doEventInner True (fromMaybe cid cidRep) edef whenEscape whenEnd spelling
    

doEventInner :: Bool
             -> CharacterID
             -> Ev.Define
             -> (Bool -> GameMachine)                        -- ^ when Escape Event
             -> (Bool -> GameMachine)                        -- ^ when End Event
             -> (Spell.Define -> GameMachine -> GameMachine) -- ^ GameMachine for spelling
             -> GameMachine
doEventInner isHidden cidRep edef whenEscape whenEnd spelling = doEvent' edef whenEscape
  where
    candidates :: [(String, Ev.Define)] -> [(Input, GameMachine)]
    candidates = concatMap (\(m, edef) -> [(Key x, doEventInner False cidRep edef whenEscape whenEnd spelling)
                                          | x <- if m == "" || m == "\n" then [m] else if m == "\r" then ["\n"] else lines m])

    doEvent' :: Ev.Define -> (Bool -> GameMachine) -> GameMachine
    -- moving
    doEvent' Ev.ReturnCastle _ = GameAuto $ do
        toCastle <- home
        returnToCastle >> run toCastle
    doEvent' (Ev.MoveTo (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        when (z' /= z p) resetRoomBattle
        movePlace (InMaze p') >> run (next isHidden)
    doEvent' (Ev.StairsToUpper (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        when (z' /= z p) resetRoomBattle
        run $ events' (updownEffect p' True) (next False)
    doEvent' (Ev.StairsToLower (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        when (z' /= z p) resetRoomBattle
        run $ events' (updownEffect p' False) (next False)

    -- interactive
    doEvent' (Ev.Message     msg picID     ) next = events [messagePic msg picID] (next False)
    doEvent' (Ev.MessageTime msg picID t   ) next = events [messageTime t msg picID] (next False)
    doEvent' (Ev.Select      msg picID ways) next = select (messagePic msg picID) (candidates ways)
    doEvent' (Ev.Ask         msg picID ways) next = select (ask msg picID) (candidates ways)

    doEvent' (Ev.MessageT     dt msg picID     ) next = talk msg dt picID (next False)
    doEvent' (Ev.MessageTimeT dt msg picID t   ) next = talkSelect msg dt picID $ const (events [messageTime t msg picID] (next False))
    doEvent' (Ev.SelectT      dt msg picID ways) next = talkSelect msg dt picID (`select` candidates ways)
    doEvent' (Ev.AskT         dt msg picID ways) next = talkSelect msg dt picID $ const (select (ask msg picID) (candidates ways))

    doEvent' (Ev.FlashMessage     msg)   next = events [flashMessageInf msg] (next False)
    doEvent' (Ev.FlashMessageTime msg t) next = events [flashMessage (-1500) msg] (next False)

    -- happens
    doEvent' (Ev.Switch []) next = next isHidden
    doEvent' (Ev.Switch (c:cs)) next = GameAuto $ do
        match <- matchCondition cidRep (fst c)
        run $ if match then doEvent' (snd c)        next
                       else doEvent' (Ev.Switch cs) next

    doEvent' (Ev.GetItem targetType itemIdF isDetermined ns) next = 
        let next1 = if null ns then next isHidden else doEvent' (head ns) next
            next2 = if length ns <= 1 then next1 else doEvent' (ns !! 1) next
        in doEventToCharacterAny targetType next1 next2 $ \cid -> do
            c   <- characterByID cid
            map <- formulaMapC c
            iid <- ItemID <$> evalWith map itemIdF
            item <- itemByID iid
            if Chara.hasMaxCountItem c then return False
            else updateCharacterWith cid (\c -> c { Chara.items = Chara.items c ++ [ItemInf iid isDetermined] })
                 >> return True
    doEvent' (Ev.LostItem targetType itemIdF ns) next =
        let next1 = if null ns then next isHidden else doEvent' (head ns) next
            next2 = if length ns <= 1 then next1 else doEvent' (ns !! 1) next
        in doEventToCharacterAny targetType next1 next2 $ \cid -> do
            c   <- characterByID cid
            map <- formulaMapC c
            iid <- ItemID <$> evalWith map itemIdF
            let p (ItemInf iid' _) = iid' == iid
            if not (any p (Chara.items c)) then return False
            else do
                updateCharacterWith cid (\c' -> c' {
                    Chara.items = deleteBy (\(ItemInf iid1 _) (ItemInf iid2 _) -> iid1 == iid2) (ItemInf iid True) (Chara.items c')
                })
                return True
    doEvent' (Ev.GetGold targetType valF) next = doEventToCharacter targetType (next isHidden) $ \cid -> do
        c <- characterByID cid
        n <- flip evalWith valF =<< formulaMapC c
        updateCharacterWith cid (Chara.getGold n)
    doEvent' (Ev.LostGold Ev.Leader valF ns) next =
        let next1 = if null ns then next isHidden else doEvent' (head ns) next
            next2 = if length ns <= 1 then next1 else doEvent' (ns !! 1) next
        in doEventToCharacterAny Ev.Leader next1 next2 $ \cid -> do
            c <- characterByID cid
            n <- flip evalWith valF =<< formulaMapC c
            let g = Chara.gold c
            if g < n then return False
            else updateCharacterWith cid (\c' -> c' { Chara.gold = g - n }) >> return True
    doEvent' (Ev.LostGold Ev.All valF ns) next =
        let next1 = if null ns then next isHidden else doEvent' (head ns) next
            next2 = if length ns <= 1 then next1 else doEvent' (ns !! 1) next
        in GameAuto $ do
            c    <- characterByID cidRep
            n    <- flip evalWith valF =<< formulaMapC c
            cids <- party <$> world
            gs   <- fmap Chara.gold <$> mapM characterByID cids
            let g = sum gs
            if g < n then run next2
                     else foldM_ (\n' cid -> do
                            gc <- Chara.gold <$> characterByID cid
                            if gc >= n' then spentGold cid n' >> return 0
                                        else spentGold cid gc >> return (n' - gc)
                          ) g cids >> run next1
    doEvent' (Ev.ChangeHP targetType valF) next = doEventToCharacter targetType (next isHidden) $ \cid -> do
        c   <- characterByID cid
        map <- formulaMapC c
        n   <- evalWith map valF
        updateCharacterWith cid (setHp (Chara.hp c + n))
    doEvent' (Ev.ChangeMP targetType kind lvs valF) next = doEventToCharacter targetType (next isHidden) $ \cid -> do
        c   <- characterByID cid
        map <- formulaMapC c
        n   <- evalWith map valF
        updateCharacterWith cid (\c' ->
            let (mageMps, priestMps) = Chara.mp c'
                (maxMageMps, maxPriestMps) = Chara.maxmp c'

                updateAt :: Int -> (a -> a) -> [a] -> [a]
                updateAt idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

                updateLevels :: [Int] -> [Int] -> [Int] -> [Int]
                updateLevels targetLvs currentLvMps maxLvMps = foldl (\mps lv ->
                    let current = mps !! (lv - 1)
                        maxi    = maxLvMps !! (lv - 1)
                        new     = max 0 (min maxi (current + n))
                    in updateAt (lv - 1) (const new) mps
                    ) currentLvMps targetLvs

                newMps = case kind of
                    Spell.M -> (updateLevels lvs mageMps maxMageMps, priestMps)
                    Spell.P -> (mageMps, updateLevels lvs priestMps maxPriestMps)
            in c' { Chara.mp = newMps })
    doEvent' (Ev.ChangeJob targetType jobName) next = doEventToCharacter targetType (next isHidden) $ \cid -> do
        j <- asks $ find ((== jobName) . Chara.jobName) . jobs
        case j of Just j' -> updateCharacterWith cid (\c -> c { Chara.equips = [], Chara.job = j' })
                  Nothing -> return ()
    doEvent' (Ev.LearningSpell targetType spellIdF) next = doEventToCharacter targetType (next isHidden) $ \cid -> do
        c   <- characterByID cid
        map <- formulaMapC c
        sid <- SpellID <$> evalWith map spellIdF
        s   <- spellByID sid
        case s of
            Nothing -> return ()
            Just spell -> do
                let (mageMpFormula, priestMpFormula) = Chara.mpFormula (Chara.job c)
                    canLearn = case Spell.kind spell of
                        Spell.M -> not $ null mageMpFormula
                        Spell.P -> not $ null priestMpFormula
                    hasLearned = sid `elem` Chara.spells c
                when (canLearn && not hasLearned) $ do
                    updateCharacterWith cid (\c' -> c' { Chara.spells = sort (sid : Chara.spells c') })

    doEvent' (Ev.ChangeEventFlag idx f) next = GameAuto $ do
        efs <- eventFlags <$> world
        ps  <- party <$> world
        os  <- mapM characterByID ps
        map <- formulaMapP
        n   <- evalWith map f
        modify $ \w -> w { eventFlags = take idx efs ++ [n] ++ drop (idx + 1) efs }
        run $ next isHidden

    doEvent' (Ev.ChangeLeader pos) next = next isHidden --MEMO:this event has no mean in this timing.

    doEvent' (Ev.PlaySoundEffect s) next = addEff (withSE s) (next isHidden)
    doEvent' (Ev.PlayBGM Ambient) next   = addEff (withBGM TurnOff) (events [Resume (changeWaitTime 1)] $ addEff (withBGM Ambient) (next isHidden))
    doEvent' (Ev.PlayBGM s) next         = addEff (withBGM s) (next isHidden)

    -- others
    doEvent' (Ev.AsSpell sid) next = GameAuto $ do
        s' <- spellByID sid
        run $ case s' of Just s  -> spelling s (next isHidden)
                         Nothing -> next isHidden

    doEvent' (Ev.Reference eid) next = GameAuto $ do
        evDB  <- asks mazeEvents
        case Map.lookup eid evDB of Nothing   -> run $ next isHidden
                                    Just edef -> run $ doEvent' edef next
    doEvent' Ev.End    _ = whenEnd    isHidden
    doEvent' Ev.Escape _ = whenEscape isHidden

    doEvent' (Ev.Events [])         next = next isHidden
    doEvent' (Ev.Events (Ev.End   :_)) _ = whenEnd    isHidden
    doEvent' (Ev.Events (Ev.Escape:_)) _ = whenEscape isHidden
    doEvent' (Ev.Events (Ev.ChangeLeader pos:es)) next = GameAuto $ do
        cid <- characterIDInPartyAtS pos
        run $ doEvent' edef $ \isHidden' -> doEventInner isHidden' (fromMaybe cidRep cid) (Ev.Events es) next whenEnd spelling
    doEvent' (Ev.Events (edef:es)) next = doEvent' edef $ \isHidden' -> doEventInner isHidden' cidRep (Ev.Events es) next whenEnd spelling

    
    doEventToCharacter :: Ev.TargetType -> GameMachine -> (CharacterID -> GameState()) -> GameMachine
    doEventToCharacter Ev.Leader next' e = with [e cidRep] next'
    doEventToCharacter Ev.All    next' e = GameAuto $ do
        cids <- party <$> world
        run $ with (e <$> cids) next'

    doEventToCharacterAny :: Ev.TargetType -> GameMachine -> GameMachine -> (CharacterID -> GameState Bool) -> GameMachine
    doEventToCharacterAny Ev.Leader next1 next2 e = GameAuto $ do
        suc <- e cidRep
        run $ if suc then next1 else next2
    doEventToCharacterAny Ev.All    next1 next2 e = GameAuto $ do
        cids <- party <$> world
        run $ doEventToCharacterAny' next1 next2 e cids
    doEventToCharacterAny' next1 next2 e [] = next2
    doEventToCharacterAny' next1 next2 e (cid:cids) = GameAuto $ do
        suc <- e cid
        run $ if suc then next1 else doEventToCharacterAny' next1 next2 e cids


matchCondition :: CharacterID -> Ev.Condition -> GameState Bool
matchCondition _ (Ev.PartyHasItem iid mustIdentified) = do
    os  <- mapM characterByID . party =<< world
    let is = concatMap Chara.items os
    return $ any (\(ItemInf id identified) -> id == iid && (not mustIdentified || identified)) is
matchCondition _ (Ev.PartyExistAlignment as) = do
    os  <- mapM characterByID . party =<< world
    return $ any (`elem` as) (Chara.alignment <$> os)
matchCondition _ (Ev.PartyNotExistAlignment as) = do
    os  <- mapM characterByID . party =<< world
    return $ all (`notElem` as) (Chara.alignment <$> os)
matchCondition _ (Ev.PartyPositionIs ps) = flip elem ps <$> currentPosition
matchCondition _ (Ev.FormulaCheckParty f) = do
    os  <- mapM characterByID . party =<< world
    map <- formulaMapP
    n   <- evalWith map f
    happens n
matchCondition cidRep (Ev.FormulaCheckLeader f) = do
    c   <- characterByID cidRep
    map <- formulaMapC c
    n   <- evalWith map f
    happens n
matchCondition cidRep (Ev.And cs) = and <$> mapM (matchCondition cidRep) cs
matchCondition cidRep (Ev.Or  cs) = or  <$> mapM (matchCondition cidRep) cs
matchCondition _ Ev.Otherwise = return True
matchCondition cidRep (Ev.LeaderKnowSpell sid) = knowSpell cidRep sid
matchCondition _ (Ev.AnyOneKnowSpell sid) = do
    cids <- party <$> world
    known <- filterM (`knowSpell` sid) cids
    return $ not (null known)
matchCondition cidRep (Ev.LeaderIsJobOf js) = do
    c   <- characterByID cidRep
    return $ Chara.jobName (Chara.job c) `elem` js
matchCondition _ (Ev.AnyOneIsJobOf js) = do
    cids <- party <$> world
    matched <- filterM (\cid -> do
        c <- characterByID cid
        return $ Chara.jobName (Chara.job c) `elem` js
        ) cids
    return $ not (null matched)


updownEffect :: Position -> Bool -> [(GameState (), Event)]
updownEffect p toUp =[(upStep 3, wait  75 Nothing)
                     , (upStep 2, wait  75 Nothing)
                     , (upStep 2, wait  50 Nothing)
                     , (upStep 1, wait  75 Nothing)
                     , (upStep 2, wait  90 Nothing)
                     , (upStep 1, wait 150 Nothing)
                     , (upStep 3, wait  60 Nothing)
                     , (upStep 1, wait  70 Nothing)
                     , (upStep 2, wait  80 Nothing)
                     , (upStep 1, wait 150 Nothing)
                     , (upStep 1, wait  80 Nothing)
                     , (upStep 1, wait  80 Nothing)
                     ] ++ 
                     [(upRest >> movePlace (InMaze p), wait 150 Nothing)] ++ 
                     [
                       (upStep 3, wait  50 Nothing)
                     , (upStep 2, wait  75 Nothing)
                     , (upStep 2, wait  75 Nothing)
                     , (upStep 1, wait  75 Nothing)
                     , (upStep 2, wait  90 Nothing)
                     , (upStep 1, wait 140 Nothing)
                     , (upStep 3, wait  80 Nothing)
                     , (upStep 1, wait 250 Nothing)
                     , (upStep 2, wait  80 Nothing)
                     , (upStep 4, wait  80 Nothing)
                     , (upStep 2, wait  60 Nothing)
                     , (upStep (-3), wait  80 Nothing)
                     ]
  where
    r = if toUp then 1 else -1
    upStep :: Int -> GameState ()
    upStep n = modify (\w -> w { sceneTrans = sceneTrans w . translate (0, n * r) })
    upRest   = modify (\w -> w { sceneTrans = translate (0, -20 * r) })
