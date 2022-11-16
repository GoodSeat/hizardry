module Engine.InBattle
where

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import Engine.GameAuto
import Engine.Utils
import Engine.BattleAction
import Engine.CharacterAction (inputSpell, selectItem, selectUseTarget)
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
        def <- enemyOf eid
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
    def <- enemyOf eid
    es  <- createEnemyInstances (n - 1) l eid False
    mhp <- eval $ Enemy.hpFormula def
    let e = Enemy.Instance {
        Enemy.id            = eid
      , Enemy.noID          = n + 10 * l
      , Enemy.determined    = False
      , Enemy.hp            = mhp
      , Enemy.maxhp         = mhp
      , Enemy.statusErrors  = []
      , Enemy.maybeDropItem = dropItem
      , Enemy.modAc         = 0
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
startBattle eid isRB (g1, g2) = GameAuto $ do
    es <- decideEnemyInstance eid
    ps <- party <$> world
    -- TODO:maybe enemies (or parties) ambush.
    -- TODO:maybe friendly enemy.
    let con = Condition {
      afterWin = g1, afterRun = g2, gotExps = 0, dropGold = 0, dropItems = [], traps = [], defaultOrder = ps, isRoomBattle = isRB
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
    p <- party <$> world
    if length p < i then
      run $ confirmBattle cmds con
    else do
      let cid = p !! (i - 1)
      c <- characterOf cid
      let cs     = Chara.enableBattleCommands $ Chara.job c
          next a = selectBattleCommand (i + 1) ((cid, a) : cmds) con
          cancel = selectBattleCommand i cmds con
      if isCantFight c then
        run $ next CantMove
      else 
        run $ selectWhen (BattleCommand $ Chara.name c ++ "'s Option\n\n" ++ concatMap toMsg cs)
                         [( Key "f"
                          , selectFightTarget next
                          , Chara.Fight `elem` cs && i <= 3)
                         ,( Key "p"
                          , next Parry
                          , Chara.Parry `elem` cs)
                         ,( Key "s"
                          , inputSpell c SpellCommand BattleCommand (\s l -> next $ Spell s l) cancel
                          , Chara.Spell `elem` cs)
                         ,( Key "u"
                          , selectItem BattleCommand identified
                              (selectUseTarget BattleCommand (\i l -> next $ UseItem i l)) c cancel
                          , Chara.Spell `elem` cs)
                         ,( Key "r"
                          , events [Message $ Chara.name c ++ " flees."] (afterRun con) -- TODO:implement possible of fail to run.
                          , Chara.Run `elem` cs)
                         ,( Key " "
                          , events [None] (selectBattleCommand i cmds con)
                          , True)
                          ]
  where
    toMsg cmd = case cmd of Chara.Fight   -> "F)ight\n"
                            Chara.Spell   -> "S)pell\n"
                            Chara.Hide    -> "H)ide\n"
                            Chara.Ambush  -> "A)mbush\n"
                            Chara.Run     -> "R)un\n"
                            Chara.Parry   -> "P)arry\n"
                            Chara.UseItem -> "U)se Item\n"

selectFightTarget :: (Action -> GameMachine) -> GameMachine
selectFightTarget next = GameAuto $ do
    ess <- lastEnemies
    if length ess == 1 then run $ next (Fight L1)
    else run $ selectWhen (BattleCommand "Target group?")
                          [(Key "1", next (Fight L1), not (null ess))
                          ,(Key "2", next (Fight L2), length ess > 1)
                          ,(Key "3", next (Fight L3), length ess > 2)
                          ,(Key "4", next (Fight L4), length ess > 3)]


confirmBattle :: [(CharacterID, Action)]
              -> Condition
              -> GameMachine
confirmBattle cmds con = select (BattleCommand "Are you OK?\n\nF)ight\nT)ake Back")
                                [(Key "f", startProgressBattle cmds con)
                                ,(Key "t", selectBattleCommand 1 [] con)
                                ]

-- ==========================================================================

nextTurn :: Condition -> GameMachine
nextTurn con = GameAuto $ do
    ps <- party <$> world
    rs <- replicateM (length ps) (randomNext 0 100)
    forM_ (zip ps rs) $ \(p, r) -> do
      c <- characterOf p
      updateCharacter p $ foldl (&) c (whenToNextTurn r <$> statusErrorsOf c)
    sortPartyAutoWith (defaultOrder con)
    -- TODO!:if all character dead, move to gameover.

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
        edef <- enemyOf $ Enemy.id e
        if notElem Dead $ Enemy.statusErrors e then return (0, 0, [], [])
        else (,,,) <$> eval (Enemy.dropGold edef) <*> pure (Enemy.exp edef) <*> drops edef <*> pure (Enemy.trapCandidate edef))
    let (g, exp, is, ts) = foldl' (\(g1, e1, is1, ts1) (g2, e2, is2, ts2) -> (g1 + g2, e1 + e2, is1 ++ is2, ts1 ++ ts2)) (0, 0, [], []) drops
    return $ con { dropGold = dropGold con + g, gotExps = gotExps con + exp, dropItems = dropItems con ++ is, traps = traps con ++ ts }
  where
    drops edef = concat <$> forM (Enemy.dropItem edef) (\(prob, f) -> do
      dropped <- happens prob
      itemID  <- eval f
      return [itemID | dropped]
      )
      

wonBattle :: Condition -> GameMachine
wonBattle con = GameAuto $ do
    ps <- party <$> world
    let e  = gotExps con `div` length ps
        ft = isRoomBattle con && dropGold con > 0 && not (null $ dropItems con)
    forM_ ps $ flip updateCharacterWith (Chara.getExp e)
    run $ events [Message $ "Each survivor got " ++ show e ++ " E.P."]
                 (if ft then findTreasureChest con else getDrops con [])

getDrops :: Condition -> [Int] -> GameMachine
getDrops con is = GameAuto $ do
    ps <- party <$> world
    let g = dropGold con `div` length ps
        es = [ if g > 0 then Just (Message $ "Each survivor got " ++ show g ++ " G.P.") else Nothing
             --TODO: if not (null items) then ...
             ]
    forM_ ps $ flip updateCharacterWith (Chara.getGold g)
    run $ events (catMaybes es) (afterWin con)
  where
    items = (\i -> ItemInf (ItemID i) False) <$> is

findTreasureChest :: Condition -> GameMachine
findTreasureChest con = GameAuto $ do
    trap <- randomIn $ [Enemy.DropDirectly | null $ traps con] ++ traps con
    movePlace =<< FindTreasureChest <$> currentPosition <*> pure trap <*> pure (dropGold con) <*> pure (dropItems con)
    run $ actionForTreasureChest con

actionForTreasureChest :: Condition -> GameMachine
actionForTreasureChest con = selectWhen (BattleCommand "I)nspect\nD)isarm Trap\nO)pen\nL)eave")
                             [( Key "l"
                              , afterWin con
                              , True
                              )
                             ]

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
    cantFight <- isCantFight <$> characterOf id
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
        edef <- enemyOf $ Enemy.id e'
        if isCantFight (e', edef) then run next
        else case a of
          Enemy.Fight n d t effs -> run $ fightOfEnemy e' n d t effs next
          Enemy.Spelling f       -> do
              s' <- spellByID . SpellID =<< eval f
              np <- length . party <$> world
              cp <- randomIn $ toPartyPos <$> [1..np]
              case s' of Just s  -> run $ spell' s (Right e') (Left cp) next
                         Nothing -> run $ spellUnknown "?" (Right e') (Left cp) next
          Enemy.Run              -> do
              en   <- enemyNameOf e'
              updateEnemy e' $ const e' { Enemy.hp = 0 }
              run $ events [Message $ en ++ " flees."] next
          _                      -> undefined

-- "*** hidden away"
--  vs = ["tries to ambush"]

-- ==========================================================================
determineActions :: [(CharacterID, Action)]
                 -> GameState [BattleAction]
determineActions cmds = do 
    pcs  <- mapM toPair cmds
    elss <- zip [1..] <$> lastEnemies
    let els = concatMap (\(l, es) -> zip (repeat l) es) elss
    ecs  <- mapM toEnemyAction els
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toPair :: (CharacterID, Action) -> GameState (Int, BattleAction)
    toPair (id, act) = do
        agi <- agility . Chara.param <$> characterOf id
        key <- agiBonus agi
        return (key, ByParties id act)


toEnemyAction :: (Int, Enemy.Instance) -> GameState (Int, BattleAction)
toEnemyAction (l, ei) = do
    def <- enemyOf $ Enemy.id ei
    key <- agiBonus $ agility . Enemy.param $ def
    act <- randomIn $ Enemy.actions def
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
