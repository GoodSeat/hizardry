module Engine.InMaze
where

import Control.Monad.State (modify, forM_, guard)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InBattle
import Engine.InEvent (doEvent)
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.GameEvent as Ev

exitGame' :: GameMachine
exitGame' = GameAuto $ return (Exit, const exitGame')

-- =======================================================================
-- depends on Scenario.

enterWithoutEncount :: Position -> GameMachine
enterWithoutEncount p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev False p

enterMaybeEncount :: Position -> GameMachine
enterMaybeEncount p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev True p

eventOn :: Position -> GameState (Maybe Ev.Define)
eventOn p = do
  evMap <- asks eventMap
  evDB  <- asks mazeEvents
  case Map.lookup (x p, y p, z p) evMap of
    Nothing  -> return Nothing
    Just eid -> return $ Map.lookup eid evDB

-- =======================================================================

enterGrid :: Maybe Ev.Define -- ^ happened event.
          -> Bool            -- ^ probably encount enemy.
          -> Position        -- ^ moved position.
          -> GameMachine
enterGrid e probEncount p = GameAuto $ do
    movePlace $ InMaze p
    modify $ \w -> w { visitHitory = Map.insert (coordOf p) True (visitHitory w) }
    -- TODO!:all character lost if they are in stone.
    encountId <- if probEncount then checkEncount $ coordOf p else return Nothing
    case e of Nothing   -> case encountId of Nothing -> run $ select None (moves p)
                                             Just ei -> run $ encountEnemy ei
              Just edef -> run $ doEvent edef escapeEvent endEvent

checkEncount :: Coord -> GameState (Maybe EnemyID)
checkEncount c = do
    emap <- asks encountMap
    r    <- randomNext 1 100
    let es' = do {
        (prob, es) <- Map.lookup c emap;
        guard $ r < prob;
        return es
        }
    case es' of Nothing -> return Nothing
                Just es -> Just <$> randomIn es


ouch :: Position -> GameMachine
ouch p = select (Message "Ouch !!") $ moves p

moves :: Position -> [(Input, GameMachine)]
moves p = [(Key "a", enterGrid Nothing True $ turnLeft p)
          ,(Key "d", enterGrid Nothing True $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", exitGame')
          ,(Key "s", GameAuto $ modify (\w -> w { statusOn = not $ statusOn w }) >> run (enterGrid Nothing False p))
          ,(Key "o", GameAuto $ modify (\w -> w { guideOn  = not $ guideOn  w }) >> run (enterGrid Nothing False p))
          ]
  where
    goStraight p f = GameAuto $ do
        lab <- mazeAt $ z p
        case f lab p of Nothing -> run $ ouch p
                        Just p' -> do 
                          ps <- party <$> world
                          forM_ ps $ \p -> do
                            c <- characterOf p
                            updateCharacter p $ foldl (&) c (whenWalking <$> statusErrorsOf c) 
                          sortPartyAuto
                          -- TODO!:if all character dead, move to gameover.
                          run $ enterMaybeEncount p'

-- =======================================================================

encountEnemy :: EnemyID -> GameMachine
encountEnemy id = startBattle id (escapeEvent, escapeEvent)

-- =======================================================================

openCamp :: Position -> GameMachine
openCamp p = GameAuto $ do
    movePlace (Camping p)
    np <- length . party <$> world
    run $ selectWhen (Message "#)Inspect\nR)eorder Party\nL)eave Camp")
          [(Key "l", enterWithoutEncount p, True)
          ,(Key "1", inspectCharacter (openCamp p) True F1, np >= 1)
          ,(Key "2", inspectCharacter (openCamp p) True F2, np >= 2)
          ,(Key "3", inspectCharacter (openCamp p) True F3, np >= 3)
          ,(Key "4", inspectCharacter (openCamp p) True B4, np >= 4)
          ,(Key "5", inspectCharacter (openCamp p) True B5, np >= 5)
          ,(Key "6", inspectCharacter (openCamp p) True B6, np >= 6)
          ]

endEvent :: GameMachine
endEvent = GameAuto $ do
    p <- currentPosition
    run $ enterWithoutEncount p

escapeEvent :: GameMachine
escapeEvent = GameAuto $ do
    p <- currentPosition
    run $ enterGrid Nothing False p

-- =======================================================================





