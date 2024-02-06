module Engine.InBattle (startBattle) where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import Engine.GameAuto
import Engine.Utils
import Engine.BattleAction
import Engine.CharacterAction (inputSpell, selectItem, selectUseTarget, castDamageSpell)
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
            | Ambush Int
            | Run
            | Parry
            | UseItem Chara.ItemPos SpellTarget
            | CantMove
    deriving (Show, Eq)

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
      , Enemy.modParam      = emptyParamChange
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
    -- TODO:maybe enemies (or parties) suprised you.
    -- TODO:maybe friendly enemy.
    -- A friendly group of ****.
    -- They hail you in welcome!
    --   ^A)ttack!  ^L)eave in Peace
    let con = Condition {
      afterWin = g1, afterRun = g2, gotExps = 0, dropGold = gold, dropItems = items, traps = [], defaultOrder = ps, isRoomBattle = isRB
    }
    run $ events [MessageTime (-1000) "\nEncounter!\n" Nothing]
          (GameAuto $ moveToBattle es >> run (selectBattleCommand 1 [] con))

moveToBattle :: [[Enemy.Instance]] -> GameState ()
moveToBattle es = movePlace =<< InBattle <$> currentPosition <*> pure es


selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(CharacterID, Action)]
                    -> Condition
                    -> GameMachine
selectBattleCommand i cmds con = GameAuto $ do
    p   <- party <$> world
    if length p < i then
      run $ confirmBattle cmds con
    else do
      let cid = p !! (i - 1)
      c   <- characterByID cid
      wep <- weaponAttrOf c
      ess <- lastEnemies
      let els = take (length ess) $ toEnemyLine <$> [1..]
          fts = filter (`elem` els) $ if i <= 3 then Item.targetF wep else Item.targetB wep
          cs  = Chara.enableBattleCommands $ Chara.job c
          cs' = filter (if null fts then (/= Chara.Fight) else const True)
              . filter (if any (`elem` cantSpellStatus) (statusErrorsOf c) then (/= Chara.Spell) else const True)
              $ cs
      let next a = selectBattleCommand (i + 1) ((cid, a) : cmds) con
          cancel = selectBattleCommand i cmds con
      if isCantFight c then
        run $ next CantMove
      else
        let inspect = selectEsc (ShowStatus cid "^R)ead Spell   ^L)eave `[`E`S`C`]" SingleKey)
                                [(Key "l", selectBattleCommand i cmds con)
-- TODO                         ,(Key "r", readSpell cid inspect)
                                ]
            cms = [( Key "f"
                   , selectFightTarget fts next
                   , Chara.Fight `elem` cs')
                  ,( Key "p"
                   , next Parry
                   , Chara.Parry `elem` cs')
                  ,( Key "s"
                   , inputSpell c SpellCommand BattleCommand (\s l -> next $ Spell s l) cancel
                   , Chara.Spell `elem` cs')
                  ,( Key "u"
                   , selectItem BattleCommand identified
                       (selectUseTarget BattleCommand (\i l -> next $ UseItem i l)) c cancel
                   , Chara.UseItem `elem` cs')
                  ,( Key "r"
                   , events [Message $ Chara.name c ++ " flees."] (afterRun con) -- TODO:implement possible of fail to run.
                   , Chara.Run `elem` cs')
                  ,( Key "\16128", inspect, True) -- code of "?" ?
                  ,( Key "?", inspect, True)
                  ,( Key "\ESC"
                   , events [None] (selectBattleCommand i cmds con)
                   , True)
                  ]
            toMsg cmd = case cmd of Chara.Fight   -> "^F)ight`*\n"
                                    Chara.Spell   -> "^S)pell\n"
                                    Chara.Hide    -> "^H)ide\n"
                                    Chara.Ambush  -> "^A)mbush\n"
                                    Chara.Run     -> "^R)un\n"
                                    Chara.Parry   -> if Chara.Fight `elem` cs' then "^P)arry\n" else "^P)arry`*\n"
                                    Chara.UseItem -> "^U)se Item\n"
            snd' (_, s, _) = s
        in run $ selectWhen1 (BattleCommand $ Chara.name c ++ "'s Option\n\n" ++ concatMap toMsg cs') cms

selectFightTarget :: [EnemyLine] -> (Action -> GameMachine) -> GameMachine
selectFightTarget fts next = GameAuto $ do
    ess <- lastEnemies
    let cmds = cmdNumsWhen (length ess) $ \i -> ((next . Fight . toEnemyLine) i, toEnemyLine i `elem` fts)
        minl = enemyLineToNum $ minimum fts
        maxl = enemyLineToNum $ maximum fts
    run $ if length ess == 1 then next (Fight L1)
          else selectWhen1 (BattleCommand $ "Target group?\n(^" ++ show minl ++ "`*~^" ++ show maxl ++ ")") cmds


confirmBattle :: [(CharacterID, Action)]
              -> Condition
              -> GameMachine
confirmBattle cmds con = select1 (BattleCommand "Are you OK?\n\n^F)ight`*\n^T)ake Back")
                                 [(Key "f", startProgressBattle cmds con)
                                 ,(Key "t", selectBattleCommand 1 [] con)
                                 ]

-- ==========================================================================

nextTurn :: Condition -> GameMachine
nextTurn con = GameAuto $ do
    ps <- party <$> world
    forM_ ps $ \cid -> do
      c <- characterByID cid
      r <- randomNext 0 100
      updateCharacter cid $ whenToNextTurn r c
    sortPartyAutoWith (defaultOrder con)
    -- TODO!:if all character dead, move to gameover.

    -- update enemy condition.
    ess1 <- lastEnemies
    forM_ ess1 $ \es -> forM_ es $ \e -> do
      r <- randomNext 0 100
      updateEnemy e (damageHp (-(Enemy.healPerTurn $ Enemy.define e)) . whenToNextTurn r)

    con' <- updateCondition con
    ess  <- execState (do
                modify $ fmap $ filter (\e -> Enemy.hp e > 0) -- remove dead or fleed enemy.
                modify $ filter (not . null)                  -- remove null line.
            ) <$> lastEnemies
    ess' <- tryDetermineEnemies ess
    moveToBattle ess'
    run $ (if null ess' then wonBattle else selectBattleCommand 1 []) con'

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
    run $ events [Message $ "Each survivor got " ++ show e ++ " E.P."]
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
                    -> GameMachine
startProgressBattle cmds con = GameAuto $ run =<< nextProgressBattle <$> determineActions cmds <*> pure con


nextProgressBattle :: [BattleAction]
                   -> Condition
                   -> GameMachine
nextProgressBattle as con = foldr act (nextTurn con) as

act :: BattleAction -> GameMachine -> GameMachine
act (ByParties id a) next = GameAuto $ do
    cantFight <- isCantFight <$> characterByID id
    if cantFight then run next
    else case a of
        Fight l     -> run $ fightOfCharacter id l next
        Spell s l   -> run $ spell s (Left id) l next
        Parry       -> run next
        Run         -> run next
        CantMove    -> run next
        UseItem i l -> run $ events [Message "sorry, using item is not implemented."] next -- TODO!
        _           -> undefined
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
              case s' of Just s  -> do
                           let tt = Spell.target s
                           if tt == Spell.OpponentSingle ||
                              tt == Spell.OpponentGroup  ||
                              tt == Spell.OpponentAll then
                              run $ spell' s (Right e') (Left cp) next
                           else if tt == Spell.AllySingle ||
                                   tt == Spell.AllyGroup then do
                              ess <- lastEnemies
                              el  <- randomIn $ toEnemyLine <$> [1..length ess]
                              run $ spell' s (Right e') (Right el) next
                           else
                              run $ spell' s (Right e') (Right L1) next
                         Nothing -> run $ spellUnknown "?" (Right e') (Left cp) next

          Enemy.Breath f attrs   -> do
              ps <- party <$> world
              ts <- castDamageSpell f (Right e') (Left $ toPartyPos <$> [1..length ps])
              let toMsg t = Message $ (nameOf e ++ " spit out a breath.\n") ++ t
              run $ events (toMsg <$> "" : (snd <$> ts)) (with (fst <$> ts) next)
          Enemy.Run              -> do
              en   <- enemyNameOf e'
              updateEnemy e' $ const e' { Enemy.hp = 0 }
              run $ events [Message $ en ++ " flees."] next

-- "*** hidden away"
--  vs = ["tries to ambush"]

-- ==========================================================================
determineActions :: [(CharacterID, Action)]
                 -> GameState [BattleAction]
determineActions cmds = do
    pcs  <- mapM toPair cmds
    elss <- zip [1..] <$> lastEnemies
    ecs  <- mapM toEnemyAction $ concatMap (\(l, es) -> zip (repeat l) es) elss
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toPair :: (CharacterID, Action) -> GameState (Int, BattleAction)
    toPair (id, act) = do
        c   <- characterByID id
        key <- agiBonus . agility =<< paramOf (Left c)
        return (key, ByParties id act)


toEnemyAction :: (Int, Enemy.Instance) -> GameState (Int, BattleAction)
toEnemyAction (l, ei) = do
    key <- agiBonus . agility =<< paramOf (Right ei)
    act <- randomIn $ Enemy.actions (Enemy.define ei)
    return (key, ByEnemies l ei act)


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
              | otherwise = -4

-- ==========================================================================
