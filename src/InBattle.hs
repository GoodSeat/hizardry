module InBattle
where

import GameAuto
import Utils
import qualified Enemies as Enemy


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
    let e = Enemy.Instance {
      Enemy.id = eid, Enemy.hp = mhp, Enemy.statusErrors = []
    }
    return $ e : es

startBattle :: Enemy.ID             -- ^ encounted enemy.
            -> (GameAuto, GameAuto) -- ^ after battle won, run from battle..
            -> GameAuto
startBattle eid gp = Auto $ do
    es <- decideEnemyInstance eid
    moveToBattle es
    -- TODO:maybe enemies (or parties) ambush.
    run $ selectBattleCommand 1


selectBattleCommand :: Int -- ^ character index in party.
                    -> GameAuto
selectBattleCommand = do
    undefined

