module InBattle
where

import Data.List
import Control.Monad
import GameAuto
import Utils
import World
import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Spells as Spell
import qualified Items as Item


data BattleAction = ByParties Character.ID Action
                  | ByEnemies Enemy.Instance Enemy.Action
    deriving (Show)

data Action = Fight Int
            | Spell Spell.ID Int
            | Hide
            | Ambush Int
            | Run
            | Parry
            | UseItem Item.ID Int
    deriving (Show, Eq)

-- ==========================================================================
decideEnemyInstance :: Enemy.ID -> GameState [[Enemy.Instance]]
decideEnemyInstance = decideEnemyInstance' 1
  where
    decideEnemyInstance' :: Int -> Enemy.ID -> GameState [[Enemy.Instance]]
    decideEnemyInstance' n eid = if n > 4 then return [] else do
        def <- enemyOf eid
        n   <- eval $ Enemy.numOfOccurrences def
        withBack <- happens $ Enemy.withBackProb def
        bl  <- if withBack then decideEnemyInstance' (n + 1) =<< (Enemy.ID <$> eval (Enemy.backEnemyID def))
                           else return []
        det <- identifyEnemies
        el  <- createEnemyInstances n eid det
        return $ el : bl

identifyEnemies :: GameState Bool
identifyEnemies = (<=) <$> randomNext 1 10 <*> (length . party <$> world)

createEnemyInstances :: Int          -- ^ num of create instaces.
                     -> Enemy.ID     -- ^ target id of enemy.
                     -> Bool         -- ^ enemies are determined or not.
                     -> GameState [Enemy.Instance]
createEnemyInstances 0 _ _     = return []
createEnemyInstances n eid det = do
    def <- enemyOf eid
    es  <- createEnemyInstances (n - 1) eid det
    mhp <- eval $ Enemy.maxhp def
    let e = Enemy.Instance {
      Enemy.id = eid, Enemy.determined = det, Enemy.hp = mhp, Enemy.statusErrors = []
    }
    return $ e : es

-- ==========================================================================
startBattle :: Enemy.ID             -- ^ encounted enemy.
            -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
            -> GameAuto
startBattle eid gp = Auto $ do
    es <- decideEnemyInstance eid
    moveToBattle es
    -- TODO:maybe enemies (or parties) ambush.
    -- TODO:maybe friendly enemy.
    run $ events [Message "\nEncounter!\n"] (selectBattleCommand 1 [] gp)
    -- TODO:following code is ideal...
--  select (Message "\nEncounter!\n") [(Clock, selectBattleCommand 1)]


selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(Character.ID, Action)]
                    -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
                    -> GameAuto
selectBattleCommand i cmds gp = Auto $ do
    p <- party <$> world
    if length p < i then run $ confirmBattle cmds gp else do
        let cid = p !! (i - 1)
        c <- characterOf cid
        let cs = Character.enableBattleCommands $ Character.job c
        selectWhen (Message $ Character.name c ++ "'s Option\n\n" ++ concat (toMsg <$> cs))
                   [( Key "f"
                    , selectFightTarget $ \a -> selectBattleCommand (i + 1) ((cid, a) : cmds) gp
                    , Character.Fight `elem` cs)
                   ,( Key "p"
                    , selectBattleCommand (i + 1) ((cid, Parry) : cmds) gp
                    , Character.Parry `elem` cs)
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
    else selectWhen (Message "Target group?")
                    [(Key "a", next (Fight 1), length ess > 0)
                    ,(Key "b", next (Fight 2), length ess > 1)
                    ,(Key "c", next (Fight 3), length ess > 2)
                    ,(Key "d", next (Fight 4), length ess > 3)]


confirmBattle :: [(Character.ID, Action)]
              -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
              -> GameAuto
confirmBattle cmds gp = Auto $ select (Message "Are you OK?\n\nF)ight\nT)ake Back")
                                      [(Key "f", startProgressBattle cmds gp)
                                      ,(Key "t", selectBattleCommand 1 [] gp)
                                      ]
                            
-- ==========================================================================
startProgressBattle :: [(Character.ID, Action)]
                    -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
                    -> GameAuto
startProgressBattle cmds gp = Auto $ run =<< nextProgressBattle <$> determineActions cmds <*> pure gp


nextProgressBattle :: [BattleAction]
                   -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
                   -> GameAuto
nextProgressBattle [] gp     = undefined
nextProgressBattle (a:as) gp = Auto $ run =<< events <$> act a <*> pure (nextProgressBattle as gp)

act :: BattleAction -> GameState [Event]
act (ByParties id a) = case a of
    Fight l -> return []
    Parry   -> return []
    Run     -> return []
act (ByEnemies e a) = case a of
    Enemy.Fight n d t effs -> return []
    Enemy.Run              -> return []

-- ==========================================================================
determineActions :: [(Character.ID, Action)]
                 -> GameState [BattleAction]
determineActions cmds = do
    pcs <- sequence $ toPair <$> cmds
    ecs <- sequence =<< map toEnemyAction <$> (concat <$> lastEnemies)
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toPair :: (Character.ID, Action) -> GameState (Int, BattleAction)
    toPair (id, act) = do
        agi <- Character.agility . Character.param <$> characterOf id
        key <- agiBonus agi
        return (key, ByParties id act)


toEnemyAction :: Enemy.Instance -> GameState (Int, BattleAction)
toEnemyAction ei = do
    def <- enemyOf $ Enemy.id ei
    key <- agiBonus $ Character.agility . Enemy.param $ def
    act <- randomIn $ Enemy.actions def
    return (key, ByEnemies ei act)


agiBonus :: Int -> GameState Int
agiBonus agi = (+) <$> randomNext 1 10 <*> pure (bonus agi)
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

