module Engine.InEvent
where

import Control.Monad (when, filterM)
import Control.Monad.State (modify, gets, forM_)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (sort, find)
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

    -- happens
    doEvent' (Ev.Switch []) next = next isHidden
    doEvent' (Ev.Switch (c:cs)) next = GameAuto $ do
        match <- matchCondition (fst c)
        run $ if match then doEvent' (snd c)        next
                       else doEvent' (Ev.Switch cs) next

    doEvent' (Ev.GetItem targetType itemIdF isDetermined) next = undefined -- TODO
    doEvent' (Ev.LostItem targetType itemIdF) next = undefined -- TODO
    doEvent' (Ev.GetGold targetType valF) next = undefined -- TODO
    doEvent' (Ev.LostGold targetType valF) next = undefined -- TODO
    doEvent' (Ev.ChangeHP targetType valF) next = undefined -- TODO
    doEvent' (Ev.ChangeMP targetType kind lvs valF) next = undefined -- TODO
    doEvent' (Ev.ChangeJob targetType jobName) next = doEventToCharacter targetType (next isHidden) $ \cid -> do
        j <- asks $ find ((== jobName) . Chara.jobName) . jobs
        case j of Just j' -> updateCharacterWith cid (\c -> c { Chara.equips = [], Chara.job = j' })
                  Nothing -> return ()
    doEvent' (Ev.LearningSpell targetType spellIdF) next = undefined -- TODO

    doEvent' (Ev.ChangeEventFlag idx f) next = GameAuto $ do
        efs <- eventFlags <$> world
        ps  <- party <$> world
        os  <- mapM characterByID ps
        map <- addEvFlagToFormulaMap Map.empty
        n   <- evalWith map f
        modify $ \w -> w { eventFlags = take idx efs ++ [n] ++ drop (idx + 1) efs }
        run $ next isHidden

    doEvent' (Ev.ChangeLeader pos) next = next isHidden


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
    doEvent' (Ev.Events [])        next = next isHidden
    doEvent' (Ev.Events (Ev.ChangeLeader pos:es)) next = GameAuto $ do
        cid <- characterIDInPartyAtS pos
        run $ doEvent' edef $ \isHidden' -> doEventInner isHidden' (fromMaybe cidRep cid) (Ev.Events es) whenEscape whenEnd spelling
    doEvent' (Ev.Events (edef:es)) next = doEvent' edef $ \isHidden' -> doEventInner isHidden' cidRep (Ev.Events es) whenEscape whenEnd spelling

    
    doEventToCharacter :: Ev.TargetType -> GameMachine -> (CharacterID -> GameState()) -> GameMachine
    doEventToCharacter Ev.Leader next' e = with [e cidRep] next'
    doEventToCharacter Ev.All    next' e = GameAuto $ do
        cids <- party <$> world
        run $ with (e <$> cids) next'


matchCondition :: Ev.Condition -> GameState Bool
matchCondition (Ev.PartyHasItem iid mustIdentified) = do
    os  <- mapM characterByID . party =<< world
    let is = concatMap Chara.items os
    return $ any (\(ItemInf id identified) -> id == iid && (not mustIdentified || identified)) is
matchCondition (Ev.PartyExistAlignment as) = do
    os  <- mapM characterByID . party =<< world
    return $ any (`elem` as) (Chara.alignment <$> os)
matchCondition (Ev.PartyNotExistAlignment as) = do
    os  <- mapM characterByID . party =<< world
    return $ all (`notElem` as) (Chara.alignment <$> os)
matchCondition (Ev.PartyPositionIs ps) = flip elem ps <$> currentPosition
matchCondition (Ev.FormulaCheckParty f) = do
    os  <- mapM characterByID . party =<< world
    map <- addEvFlagToFormulaMap Map.empty
    n   <- evalWith map f
    happens n
matchCondition (Ev.FormulaCheckLeader f) = do
    c   <- characterByID . head . party =<< world
    map <- addEvFlagToFormulaMap =<< formulaMapS (Left c)
    n   <- evalWith map f
    happens n
matchCondition (Ev.And cs) = and <$> mapM matchCondition cs
matchCondition (Ev.Or  cs) = or  <$> mapM matchCondition cs
matchCondition Ev.Otherwise = return True


updownEffect :: Position -> Bool -> [(GameState (), Event)]
updownEffect p toUp =[ (upStep 3, wait  75 Nothing)
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
                     [ (upStep 3, wait  50 Nothing)
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


-- | state machine when return to castle.
returnToCastle :: GameState ()
returnToCastle = do
    resetRoomBattle
    setLightValue True  0
    setLightValue False 0

    resetEffectInOnlyBattle

    ps <- party <$> world
    forM_ ps $ \p -> do
      c <- characterByID p
      updateCharacter p c { Chara.paramDelta = [] }
      updateCharacterWith p whenReturnCastle

    -- remove dead/stoned/staned characters etc.
    rcis <- flip filterM ps $ fmap mustGotoTemple . characterByID

    modify $ \w -> w {
        partyParamDelta = []
      , party           = filter (not . (`elem` rcis)) $ party w
      , inTavernMember  = sort $ inTavernMember w ++ rcis
      }


-- | remove effects that is valid in battle only.
resetEffectInOnlyBattle :: GameState ()
resetEffectInOnlyBattle = do
    ps <- party <$> world
    mapM_ (`updateCharacterWith` (\ca -> ca { Chara.paramDelta = filter ((/= OnlyInBattle) . fst) (Chara.paramDelta ca) })) ps
    w <- world
    modify $ \w -> w { partyParamDelta = filter ((/= OnlyInBattle) . fst) (partyParamDelta w) }



resetRoomBattle :: GameState ()
resetRoomBattle = modify $ \w -> w { roomBattled = [] }

setLightValue :: Bool -> Int -> GameState ()
setLightValue super n = modify $ \w -> if super then w { partyLight' = n }
                                                else w { partyLight = n }

setLightValueWith :: Bool -> (Int -> Int) -> GameState ()
setLightValueWith super f = do
    n <- gets (f . if super then partyLight' else partyLight)
    when (n >= 0) $ setLightValue super n

