module InBattle
where

import GameAuto
import Utils
import World
import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Spells as Spell
import qualified Items as Item


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
        el  <- createEnemyInstances n eid
        return $ el : bl

createEnemyInstances :: Int          -- ^ num of create instaces.
                     -> Enemy.ID     -- ^ target id of enemy.
                     -> GameState [Enemy.Instance]
createEnemyInstances 0 _   = return []
createEnemyInstances n eid = do
    def <- enemyOf eid
    es  <- createEnemyInstances (n - 1) eid
    mhp <- eval $ Enemy.maxhp def
    det <- happens 50
    let e = Enemy.Instance {
      Enemy.id = eid, Enemy.determined = det, Enemy.hp = mhp, Enemy.statusErrors = []
    }
    return $ e : es


data Action = Fight Int
            | Spell Spell.ID Int
            | Hide
            | Ambush Int
            | Run
            | Parry
            | UseItem Item.ID Int
    deriving (Show, Eq)

startBattle :: Enemy.ID             -- ^ encounted enemy.
            -> (GameAuto, GameAuto) -- ^ after battle won, run from battle..
            -> GameAuto
startBattle eid gp = Auto $ do
    es <- decideEnemyInstance eid
    moveToBattle es
    -- TODO:maybe enemies (or parties) ambush.
    -- TODO:maybe friendly enemy.
    run $ events [Message "\nEncounter!\n"] (selectBattleCommand 1 [])
    -- TODO:following code is ideal...
--  select (Message "\nEncounter!\n") [(Clock, selectBattleCommand 1)]


selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(Character.ID, Character.BattleCommand)]
                    -> GameAuto
selectBattleCommand i cmds = Auto $ do
    p <- party <$> world
    c <- characterOf $ p !! (i - 1)
    let cmds = Character.enableBattleCommands $ Character.job c
    undefined

