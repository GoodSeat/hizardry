{-# LANGUAGE TupleSections #-}
module Engine.InBattle (startBattle) where

import PreludeL
import Data.List hiding ((!!))
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import Engine.GameAuto
import Engine.Utils
import Engine.BattleAction
import Engine.CharacterAction (inputSpell, selectItem, useItem, castDamageSpell, readSpell)
import Engine.InTreasureChest (actionForTreasureChest, TreasureCondition (TreasureCondition), getTreasures)
import Data.World
import Data.Primitive
import Data.Formula (parse')
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item

data BattleAction = ByParties CharacterID Action
                  | ByEnemies Int Enemy.Instance Enemy.Action
    deriving (Show)

data Action = Fight EnemyLine
            | Spell String SpellTarget
            | Hide
            | Ambush EnemyLine
--          | Dispell EnemyLine    -- TODO:not implement
            | Run
            | Parry
            | UseItem Chara.ItemPos SpellTarget
            | CantMove
    deriving (Show, Eq)

data Surprise = PartySurprise | EnemySurprise | NoSurprise deriving (Eq)

data Condition = Condition {
      afterWin     :: GameMachine
    , afterRun     :: GameMachine
    , gotExps      :: Int
    , dropGold     :: Int
    , dropItems    :: [Int]
    , traps        :: [Enemy.Trap]
    , defaultOrder :: [CharacterID] -- ^ party order when battle started.
    , isRoomBattle :: Bool
    }

-- ==========================================================================
decideEnemyInstance :: EnemyID -> GameState [[Enemy.Instance]]
decideEnemyInstance e = decideEnemyInstance' 1 e >>= tryDetermineEnemies
  where
    decideEnemyInstance' :: Int -> EnemyID -> GameState [[Enemy.Instance]]
    decideEnemyInstance' l eid = if l > 4 then return [] else do
        def <- enemyDefineByID eid
        n   <- eval $ Enemy.numOfOccurrences def
        withBack <- happens $ Enemy.withBackProb def
        bl  <- if withBack then decideEnemyInstance' (l + 1) . EnemyID =<< eval (Enemy.backEnemyID def)
                           else return []
        el  <- createEnemyInstances n l eid True
        return $ el : bl

createEnemyInstances :: Int     -- ^ num of create instaces.
                     -> Int     -- ^ belong line no.
                     -> EnemyID -- ^ target id of enemy.
                     -> Bool    -- ^ maybe drop item or not.
                     -> GameState [Enemy.Instance]
createEnemyInstances 0 _ _ _              = return []
createEnemyInstances n l eid dropItem = do
    def <- enemyDefineByID eid
    es  <- createEnemyInstances (n - 1) l eid False
    mhp <- eval $ Enemy.hpFormula def
    let e = Enemy.Instance {
        Enemy.id            = eid
      , Enemy.define        = def
      , Enemy.noID          = n + 10 * l
      , Enemy.determined    = False
      , Enemy.hp            = mhp
      , Enemy.maxhp         = mhp
      , Enemy.statusErrors  = []
      , Enemy.maybeDropItem = dropItem
      , Enemy.modParams     = []
    }
    return $ e : es

tryDetermineEnemies :: [[Enemy.Instance]] -> GameState [[Enemy.Instance]]
tryDetermineEnemies = mapM $ \es -> do
    determine <- (<=) <$> randomNext 1 10 <*> (length . party <$> world)
    return $ if determine then fmap (\e -> e { Enemy.determined = True }) es else es

-- ==========================================================================
startBattle :: EnemyID                    -- ^ encounted enemy.
            -> Bool                       -- ^ is room battle or not.
            -> (GameMachine, GameMachine) -- ^ after battle won, run from battle.
            -> GameMachine
startBattle eid isRB gms = startBattle' eid isRB gms 0 []

startBattle' :: EnemyID                    -- ^ encounted enemy.
             -> Bool                       -- ^ is room battle or not.
             -> (GameMachine, GameMachine) -- ^ after battle won, run from battle.
             -> Int                        -- ^ pre gained gold.
             -> [Int]                      -- ^ pre gained items.
             -> GameMachine
startBattle' eid isRB (g1, g2) gold items = GameAuto $ do
    es <- decideEnemyInstance eid
    ps <- party <$> world
    isFriendly <- happens $ (Enemy.friendlyProb . Enemy.define . head . head) es
    surprise   <- if isFriendly then return NoSurprise else do
        r <- randomNext 0 99
        let partySurpriseProb = 32 - length ps * 2
            enemySurpriseProb = 22 - length ps
        return $ if      r < partySurpriseProb                     then PartySurprise
                 else if r < partySurpriseProb + enemySurpriseProb then EnemySurprise
                 else                                                   NoSurprise
    let msg = withBGM Encounter $ flashMessage (-1000) "    Encounter!!    "
        con = Condition {
                afterWin = g1, afterRun = g2, gotExps = 0, dropGold = gold, dropItems = items, traps = [], defaultOrder = ps, isRoomBattle = isRB
              }
    run $ case surprise of
        PartySurprise -> events [msg, flashMessage (-3000) "    The monsters are unaware of you.    "]
                         (with [moveToBattle es] $ selectBattleCommand 1 [] con (Just PartySurprise))
        EnemySurprise -> events [msg, flashMessage (-3000) "    The monsters surprised you!    "]
                         (with [moveToBattle es] $ startProgressBattle [] con (Just EnemySurprise))
        NoSurprise    -> events [msg]
                         (startBattleMaybeFriendly isFriendly es con g1)

startBattleMaybeFriendly :: Bool -> [[Enemy.Instance]] -> Condition -> GameMachine -> GameMachine
startBattleMaybeFriendly isFriendly es con whenLeave = if not isFriendly then core else
    select (message msg) [(Key "a", core), (Key "l", whenLeave)]  -- TODO:may change alignment
  where
    erep = head . head $ es
    msg  = "A friendly group of " ++ nameOf erep ++ ".\nThey hail you in welcome!\n\n^A)ttack!\n^L)eave in Peace"
    core = with [moveToBattle es] $ selectBattleCommand 1 [] con (Just NoSurprise)
  
    

moveToBattle :: [[Enemy.Instance]] -> GameState ()
moveToBattle es = movePlace =<< InBattle <$> currentPosition <*> pure es

selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(CharacterID, Action)]
                    -> Condition
                    -> Maybe Surprise
                    -> GameMachine
selectBattleCommand i cmds con surprise = GameAuto $ do
    if null cmds then resetCommand else updateCommand cmds
    p   <- party <$> world
    if length p < i then
      run $ confirmBattle cmds con surprise
    else do
      let cid = p !! (i - 1)
      c   <- characterByID cid
      wep <- weaponAttrOf c
      ess <- lastEnemies
      let els = take (length ess) $ toEnemyLine <$> [1..]
          fts = filter (`elem` els) $ if i <= 3 then Item.targetF wep else Item.targetB wep
          fts'= filter (`elem` els) $ Item.targetF wep
          cs  = Chara.enableBattleCommands $ Chara.job c
          cs' = filter (if null fts  then (/= Chara.Fight) else const True)
              . filter (if null fts' then (/= Chara.Ambush) else const True)
              . filter (if any (hasStatusError c) cantSpellStatus then (/= Chara.Spell) else const True)
              . filter (if not (hasStatusError c Hidden) then (/= Chara.Ambush) else const True)
              . filter (if hasStatusError c Hidden then (/= Chara.Hide) else const True)
              . filter (if hasStatusError c Found  then (`notElem` [Chara.Hide, Chara.Ambush]) else const True)
              . filter (if surprise == Just PartySurprise then (/= Chara.Spell) else const True)
              $ cs
      let next a = selectBattleCommand (i + 1) ((cid, a) : cmds) con surprise
          cancel = selectBattleCommand i cmds con surprise
      if isCantFight c then
        run $ next CantMove
      else
        let inspect = selectEsc (showStatus cid "^R)ead Spell   ^L)eave `[`E`S`C`]")
                                [(Key "l", selectBattleCommand i cmds con surprise)
                                ,(Key "r", readSpell inspect cid)
                                ]
            cms = [( Key "a"
                   , selectFightTarget c fts' next cancel
                   , Chara.Ambush `elem` cs')
                  ,( Key "f"
                   , selectFightTarget c fts next cancel
                   , Chara.Fight `elem` cs')
                  ,( Key "h"
                   , next Hide
                   , Chara.Hide `elem` cs')
                  ,( Key "p"
                   , next Parry
                   , Chara.Parry `elem` cs')
                  ,( Key "s"
                   , inputSpell c spellCommand battleCommand (\s l -> next $ Spell s l) cancel
                   , Chara.Spell `elem` cs')
                  ,( Key "u"
                   , selectItem battleCommand identified
                       (useItem battleCommand (\i l -> next $ UseItem i l)) c cancel
                   , Chara.UseItem `elem` cs')
                  ,( Key "r"
                   , tryRun c con surprise
                   , Chara.Run `elem` cs')
                  ,( Key "\16128", inspect, True) -- code of "?" ?
                  ,( Key "?", inspect, True)
                  ,( Key "\ESC"
                   , events [None] (selectBattleCommand i cmds con surprise)
                   , True)
                  ,( Key "b"
                   , with [resetCommand] $ selectBattleCommand (i - 1) (drop 1 cmds) con surprise
                   , i > 1)
                  ]
            toMsg cmd = case cmd of Chara.Fight   -> if Chara.Ambush `elem` cs' then "^F)ight\n" else "^F)ight`*\n"
                                    Chara.Spell   -> "^S)pell\n"
                                    Chara.Hide    -> if Chara.Fight `elem` cs' then "^H)ide\n" else"^H)ide`*\n" 
                                    Chara.Ambush  -> "^A)mbush`*\n"
                                    Chara.Dispell -> "^D)ispell\n"
                                    Chara.Run     -> "^R)un\n"
                                    Chara.Parry   -> if Chara.Fight `elem` cs' || Chara.Hide `elem` cs' || Chara.Ambush `elem` cs' then "^P)arry\n" else "^P)arry`*\n"
                                    Chara.UseItem -> "^U)se Item\n"
        in run $ selectWhen1 (battleCommand $ Chara.name c ++ "'s Option\n\n" ++ concatMap toMsg cs' ++ (if i == 1 then "" else "^B)ack\n")) cms

selectFightTarget :: Chara.Character -> [EnemyLine] -> (Action -> GameMachine) -> GameMachine -> GameMachine
selectFightTarget c fts next cancel = GameAuto $ do
    ess   <- lastEnemies
    wattr <- weaponAttrOf c
    let cmds = cmdNumsWhen (length ess) $ \i -> ((next . Fight . toEnemyLine) i, toEnemyLine i `elem` fts)
        minl = enemyLineToNum $ minimum fts
        maxl = enemyLineToNum $ maximum fts
    run $ if length ess == 1 || Item.targetRange wattr == Item.ToAll then next (Fight L1)
          else selectWhen1 (battleCommand $ "Target group?\n(^" ++ show minl ++ "`*~^" ++ show maxl ++ ")\n\n^L)eave `[`E`S`C`]")
                           (cmds ++ [(Key "l", cancel, True), (Key "\ESC", cancel, True)])

tryRun :: Chara.Character -> Condition -> Maybe Surprise -> GameMachine
tryRun c con surprised = GameAuto $ do
    np  <- length . party <$> world
    es  <- lastEnemies
    suc <- if isJust surprised then return True else happens $ 100 - length es * 3 - (np - 1) * 4
    let bm = Chara.name c ++ " flees."
    if suc then run $ events [message bm] (afterRun con)
           else run $ events [message bm, message $ bm ++ "\n\nBut failed!!"] (startProgressBattle [] con surprised)

resetCommand :: GameState ()
resetCommand = do
    ps <- party <$> world
    forM_ ps $ flip updateCharacterWith (removeStatusError (Command ""))

updateCommand :: [(CharacterID, Action)] -> GameState ()
updateCommand [] = return ()
updateCommand ((cid, act):cs) = do
    let s = toS act
    c <- characterByID cid
    unless (null s) $ updateCharacterWith cid (addStatusError (Command s))
    updateCommand cs
  where
    toS (Fight _)     = "Fight"
    toS (Spell s _)   = s
    toS Hide          = "Hide"
    toS (Ambush _)    = "Ambush"
    toS Parry         = "Parry"
    toS (UseItem _ _) = "Item"
    toS _             = ""


confirmBattle :: [(CharacterID, Action)]
              -> Condition
              -> Maybe Surprise
              -> GameMachine
confirmBattle cmds con surprise = select1 (battleCommand "Are you OK?\n\n^F)ight`*\n^T)ake Back")
                                 [(Key "f", startProgressBattle cmds con surprise)
                                 ,(Key "t", selectBattleCommand 1 [] con surprise)
                                 ]

-- ==========================================================================

nextTurn :: Condition -> GameMachine
nextTurn con = GameAuto $ do
    w <- world
    modify $ \w -> w { globalTime = globalTime w + 1 }

    -- update enemy condition.
    ess1 <- lastEnemies
    forM_ ess1 $ \es -> forM_ es $ \e -> do
      r     <- randomNext 0 100
      param <- paramOf (Right e)
      when (Enemy.hp e > 0) $ updateEnemy e (damageHp (-(Enemy.healPerTurn $ Enemy.define e)) . whenToNextTurn r 0 param)

    con' <- updateCondition con
    ess  <- execState (do
                modify $ fmap $ filter (\e -> Enemy.hp e > 0) -- remove dead or fleed enemy.
                modify $ filter (not . null)                  -- remove null line.
            ) <$> lastEnemies
    ess' <- tryDetermineEnemies ess

    ps <- party <$> world
    fcs <- flip filterM ps $ \cid -> do
      c     <- characterByID cid
      r     <- randomNext 1 100
      param <- paramOf (Left c)
      updateCharacter cid $ whenToNextTurn r (sum $ length <$> ess') param c
      c'    <- characterByID cid
      return $ hasStatusError c Found /= hasStatusError c' Found

    msgs <- forM fcs $ \cid -> do
      c <- characterByID cid
      return $ message (nameOf c ++ " was found by enemies.")

    sortPartyAutoWith (defaultOrder con)
    -- TODO!:if all character dead, move to gameover.

    moveToBattle ess'
    run $ if null ess' then wonBattle con' else events msgs (selectBattleCommand 1 [] con' Nothing)

updateCondition :: Condition -> GameState Condition
updateCondition con = do
    ess   <- lastEnemies
    drops <- forM (concat ess) (\e -> do
        let edef = Enemy.define e
        if notElem Dead $ Enemy.statusErrors e then return (0, 0, [], [])
        else (,,,) <$> eval (Enemy.dropGold edef) <*> pure (Enemy.exp edef) <*> drops edef <*> pure (Enemy.trapCandidate edef))
    let (g, exp, is, ts) = foldl' (\(g1, e1, is1, ts1) (g2, e2, is2, ts2) -> (g1 + g2, e1 + e2, is1 ++ is2, ts1 ++ ts2)) (0, 0, [], []) drops
    return $ con { dropGold = dropGold con + g, gotExps = gotExps con + exp, dropItems = dropItems con ++ is, traps = traps con ++ ts }
  where
    drops edef = concat <$> forM (Enemy.dropItem edef) (\(prob, f) -> do
      dropped <- happens prob
      itemID  <- eval f
      return [itemID | dropped && isRoomBattle con]
      )


wonBattle :: Condition -> GameMachine
wonBattle con = GameAuto $ do
    ps <- party <$> world
    let e  = gotExps con `div` length ps
        ft = isRoomBattle con && (dropGold con > 0 || not (null $ dropItems con))
    forM_ ps $ flip updateCharacterWith (Chara.getExp e)
    run $ events [withBGM WinBattle $ message $ "Each survivor got " ++ show e ++ " E.P."]
                 (if ft then findTreasureChest con else findTreasures con)

findTreasureChest :: Condition -> GameMachine
findTreasureChest con = GameAuto $ do
    trap <- randomIn $ [Enemy.DropDirectly | null $ traps con] ++ traps con
    movePlace =<< FindTreasureChest <$> currentPosition <*> pure False
    let whenAlarm eid = startBattle' eid False (afterWin con, afterRun con) (dropGold con) (dropItems con)
    run $ actionForTreasureChest (TreasureCondition (afterWin con) (dropGold con) (dropItems con) trap whenAlarm) []

findTreasures :: Condition -> GameMachine
findTreasures con = getTreasures $ TreasureCondition (afterWin con) (dropGold con) (dropItems con) undefined undefined



-- ==========================================================================
startProgressBattle :: [(CharacterID, Action)]
                    -> Condition
                    -> Maybe Surprise
                    -> GameMachine
startProgressBattle cmds con surprise = GameAuto $ run =<< nextProgressBattle <$> determineActions cmds surprise <*> pure con


nextProgressBattle :: [BattleAction]
                   -> Condition
                   -> GameMachine
nextProgressBattle as con = foldr act (nextTurn con) as


act :: BattleAction -> GameMachine -> GameMachine
act (ByParties id a) next = GameAuto $ do
    cantFight <- isCantFight <$> characterByID id
    run $ if cantFight then next else case a of
        Fight l     -> fightOfCharacter id l next
        Spell s l   -> spell s (Left id) l next
        Parry       -> with [updateCharacterWith id $ \c -> c { Chara.paramDelta = Spell.applyChangeParam (TillPastTime 1) parryParamChange (Chara.paramDelta c) }] next
        Run         -> next
        CantMove    -> next
        UseItem i l -> useItemInBattle i (Left id) l next
        Hide        -> hideOfCharacter id next
        Ambush l    -> ambushOfCharacter id l next
act (ByEnemies l e a) next = GameAuto $ do
    e_ <- currentEnemyByNo $ Enemy.noID e
    case e_ of
      Nothing -> run next
      Just e' -> do
        if isCantFight e' then run next
        else case a of
          Enemy.Fight n d t effs -> run $ fightOfEnemy e' n d t effs next
          Enemy.Spelling f       -> do
              s' <- spellByID . SpellID =<< eval f
              np <- length . party <$> world
              cp <- randomIn $ toPartyPos <$> [1..np]
              case s' of
                Just sdef -> do
                   let tt = Spell.target sdef
                       s  = Spell.name sdef
                   if tt == Spell.OpponentSingle ||
                      tt == Spell.OpponentGroup  ||
                      tt == Spell.OpponentAll then
                      run $ spell s (Right e') (Left cp) next
                   else if tt == Spell.AllySingle ||
                           tt == Spell.AllyGroup then do
                      ess <- lastEnemies
                      el  <- randomIn $ toEnemyLine <$> [1..length ess]
                      run $ spell s (Right e') (Right el) next
                   else
                      run $ spell s (Right e') (Right L1) next
                Nothing -> run $ asSpell castUnknown "?" (Right e') (Left cp) next

          Enemy.Breath f attrs   -> do
              ps <- party <$> world
              ts <- castDamageSpell f attrs (Right e') (Left $ toPartyPos <$> [1..length ps])
              let acc (_, t, d, _) = let msg = (nameOf e ++ " spit out a breath.\n") ++ t
                                     in if d then toEffect True msg else events [message msg] 
              run $ foldr acc (with (fst4 <$> ts) next) ((undefined, "", False, False) : ts)
          Enemy.Run              -> do
              updateEnemy e' $ const e' { Enemy.hp = 0 }
              run $ events [message $ nameOf e' ++ " flees."] next


-- ==========================================================================
determineActions :: [(CharacterID, Action)]
                 -> Maybe Surprise
                 -> GameState [BattleAction]
determineActions cmds surprise = do
    pcs <- case surprise of
        Just EnemySurprise -> return []
        _                  -> mapM toCharacterAction cmds
    ecs <- case surprise of
        Just PartySurprise -> return []
        _                  -> do
            elss <- zip [1..] <$> lastEnemies
            concat <$> mapM (toEnemyAction surprise) (concatMap (\(l, es) -> map (l,) es) elss)
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toCharacterAction :: (CharacterID, Action) -> GameState (Int, BattleAction)
    toCharacterAction (id, act) = do
        c   <- characterByID id
        key <- agiBonus . agility =<< paramOf (Left c)
        return (if act == Parry then -9999 else key, ByParties id act)
    toEnemyAction :: Maybe Surprise -> (Int, Enemy.Instance) -> GameState [(Int, BattleAction)]
    toEnemyAction surprise (l, ei) = do
        key <- agiBonus . agility =<< paramOf (Right ei)
        let possibleActions = Enemy.actions (Enemy.define ei)
            actions' = case surprise of
                Just EnemySurprise -> filter (not . isSpellAction) possibleActions
                _                  -> possibleActions
        if null actions' then return []
        else do
            act <- randomIn actions'
            return [(key, ByEnemies l ei act)]
    isSpellAction :: Enemy.Action -> Bool
    isSpellAction (Enemy.Spelling _) = True
    isSpellAction _                  = False

agiBonus :: Int -> GameState Int
agiBonus agi = do
    b <- (+) <$> randomNext 1 10 <*> pure (bonus agi)
    return $ max 2 (min 10 b)
  where
    bonus agi | agi <=  3 =  3
              | agi <=  5 =  2
              | agi <=  7 =  1
              | agi <= 14 =  0
              | agi <= 15 = -1
              | agi <= 16 = -2
              | agi <= 17 = -3
              | agi <= 18 = -4
              | agi <= 19 = -5
              | agi <= 20 = -6
              | otherwise = -7

-- ==========================================================================

