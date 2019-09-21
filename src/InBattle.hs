module InBattle
where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import GameAuto
import Utils
import World
import Primitive
import Formula (parse')
import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Spells as Spell
import qualified Items as Item
import BattleAction

data BattleAction = ByParties Character.ID Action
                  | ByEnemies Int Enemy.Instance Enemy.Action
    deriving (Show)

data Action = Fight Int
            | Spell Spell.ID Int
            | Hide
            | Ambush Int
            | Run
            | Parry
            | UseItem Item.ID Int
    deriving (Show, Eq)

data Condition = Condition {
      afterWin  :: GameAuto 
    , afterRun  :: GameAuto
    , gotExps   :: Int
    , dropGold  :: Int
    , dropItems :: [Int]
    }

-- ==========================================================================
decideEnemyInstance :: Enemy.ID -> GameState [[Enemy.Instance]]
decideEnemyInstance e = decideEnemyInstance' 1 e >>= tryDetermineEnemies
  where
    decideEnemyInstance' :: Int -> Enemy.ID -> GameState [[Enemy.Instance]]
    decideEnemyInstance' n eid = if n > 4 then return [] else do
        def <- enemyOf eid
        n   <- eval $ Enemy.numOfOccurrences def
        withBack <- happens $ Enemy.withBackProb def
        bl  <- if withBack then decideEnemyInstance' (n + 1) =<< (Enemy.ID <$> eval (Enemy.backEnemyID def))
                           else return []
        el  <- createEnemyInstances n eid True
        return $ el : bl

createEnemyInstances :: Int          -- ^ num of create instaces.
                     -> Enemy.ID     -- ^ target id of enemy.
                     -> Bool         -- ^ maybe drop item or not.
                     -> GameState [Enemy.Instance]
createEnemyInstances 0 _ _              = return []
createEnemyInstances n eid dropItem = do
    def <- enemyOf eid
    es  <- createEnemyInstances (n - 1) eid False
    mhp <- eval $ Enemy.hpFormula def
    let e = Enemy.Instance {
        Enemy.id = eid
      , Enemy.determined = False
      , Enemy.hp    = mhp
      , Enemy.maxhp = mhp
      , Enemy.statusErrors = []
      , Enemy.maybeDropItem = dropItem
      , Enemy.modAc = 0
    }
    return $ e : es

tryDetermineEnemies :: [[Enemy.Instance]] -> GameState [[Enemy.Instance]]
tryDetermineEnemies = sequence . fmap tryDetermineGroup
  where
    tryDetermineGroup es = do
        determine <- (<=) <$> randomNext 1 10 <*> (length . party <$> world)
        return $ if determine then fmap (\e -> e { Enemy.determined = True }) es else es

-- ==========================================================================
startBattle :: Enemy.ID             -- ^ encounted enemy.
            -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
            -> GameAuto
startBattle eid (g1, g2) = Auto $ do
    es <- decideEnemyInstance eid
    moveToBattle es
    -- TODO:maybe enemies (or parties) ambush.
    -- TODO:maybe friendly enemy.
    run $ events [Message "\nEncounter!\n"] (selectBattleCommand 1 [] con)
    -- TODO:following code is ideal...
--  select (Message "\nEncounter!\n") [(Clock, selectBattleCommand 1)]
  where
    con = Condition { afterWin = g1, afterRun = g2, gotExps = 0, dropGold = 0, dropItems = [] }


selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(Character.ID, Action)]
                    -> Condition
                    -> GameAuto
selectBattleCommand i cmds con = Auto $ do
    p <- party <$> world
    if length p < i then run $ confirmBattle cmds con else do
        let cid = p !! (i - 1)
        c <- characterOf cid
        let cs = Character.enableBattleCommands $ Character.job c
        selectWhen (BattleCommand $ Character.name c ++ "'s Option\n\n" ++ concat (toMsg <$> cs))
                   [( Key "f"
                    , selectFightTarget $ \a -> selectBattleCommand (i + 1) ((cid, a) : cmds) con
                    , Character.Fight `elem` cs)
                   ,( Key "p"
                    , selectBattleCommand (i + 1) ((cid, Parry) : cmds) con
                    , Character.Parry `elem` cs)
                   ,( Key "r"
                    , events [Message $ Character.name c ++ " flees."] (afterRun con) -- TODO:implement possible of fail to run.
                    , Character.Run `elem` cs)
                    ]
  where
    toMsg cmd = case cmd of Character.Fight   -> "F)ight\n"
                            Character.Spell   -> "S)pell\n"
                            Character.Hide    -> "H)ide\n"
                            Character.Ambush  -> "A)mbush\n"
                            Character.Run     -> "R)un\n"
                            Character.Parry   -> "P)arry\n"
                            Character.UseItem -> "U)se Item\n"

selectFightTarget :: (Action -> GameAuto) -> GameAuto
selectFightTarget next = Auto $ do
    ess <- lastEnemies
    if length ess == 1 then run $ next (Fight 1)
    else selectWhen (BattleCommand "Target group?")
                    [(Key "1", next (Fight 1), length ess > 0)
                    ,(Key "2", next (Fight 2), length ess > 1)
                    ,(Key "3", next (Fight 3), length ess > 2)
                    ,(Key "4", next (Fight 4), length ess > 3)]


confirmBattle :: [(Character.ID, Action)]
              -> Condition
              -> GameAuto
confirmBattle cmds con = Auto $ select (BattleCommand "Are you OK?\n\nF)ight\nT)ake Back")
                                       [(Key "f", startProgressBattle cmds con)
                                       ,(Key "t", selectBattleCommand 1 [] con)
                                       ]
                            
-- ==========================================================================

nextTurn :: Condition -> GameAuto
nextTurn con = Auto $ do
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
        if notElem Dead $ Enemy.statusErrors e then return (0, 0, [])
        else (,,) <$> eval (Enemy.dropGold edef) <*> pure (Enemy.exp edef) <*> pure [])
    let (g, exp, is) = foldl' (\(g1, e1, is1) (g2, e2, is2) -> (g1 + g2, e1 + e2, is1 ++ is2)) (0, 0, []) drops
    return $ con { dropGold = dropGold con + g, gotExps = gotExps con + exp, dropItems = dropItems con ++ is }

wonBattle :: Condition -> GameAuto
wonBattle con = Auto $ do
    ps <- party <$> world
    let e = gotExps con  `div` length ps
        g = dropGold con `div` length ps
        es = Just (Message $ "Each survivor got " ++ show e ++ " E.P.")
           : (if g > 0 then Just (Message $ "Each survivor got " ++ show g ++ " G.P.") else Nothing)
           : []
    forM_ ps $ flip updateCharacterWith (\c -> c { Character.exp = Character.exp c + e, Character.gold = Character.gold c + g })
    run $ events (catMaybes es) (afterWin con)


-- ==========================================================================
startProgressBattle :: [(Character.ID, Action)]
                    -> Condition
                    -> GameAuto
startProgressBattle cmds con = Auto $ run =<< nextProgressBattle <$> determineActions cmds <*> pure con


nextProgressBattle :: [BattleAction]
                   -> Condition
                   -> GameAuto
nextProgressBattle [] con     = nextTurn con
nextProgressBattle (a:as) con = act a (nextProgressBattle as con)

act :: BattleAction -> GameAuto -> GameAuto
act (ByParties id a) next = case a of
    Fight l -> fightOfCharacter id l next
    Parry   -> next
    Run     -> next
act (ByEnemies l e a) next = case a of
    Enemy.Fight n d t effs -> next -- TODO:
    Enemy.Run              -> Auto $ do
        en   <- enemyNameOf e
        updateEnemy l e $ const e { Enemy.hp = 0}
        run $ events [Message $ en ++ " flees."] next

--  vs = ["charges at", "claws at"]

-- "*** hidden away"
--  vs = ["tries to ambush"]

-- ==========================================================================
determineActions :: [(Character.ID, Action)]
                 -> GameState [BattleAction]
determineActions cmds = do
    pcs  <- sequence $ toPair <$> cmds
    elss <- zip [1..] <$> lastEnemies
    let els = concat $ (\(l, es) -> zip (repeat l) es) <$> elss
    ecs  <- sequence $ toEnemyAction <$> els
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toPair :: (Character.ID, Action) -> GameState (Int, BattleAction)
    toPair (id, act) = do
        agi <- agility . Character.param <$> characterOf id
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

