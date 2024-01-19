module Engine.InEdgeOfTown
where

import Control.Monad.State (modify)
import Data.List (sort)

import Engine.GameAuto
import Engine.Utils
import Engine.InMaze
import Data.World
import Data.Maze
import qualified Data.Characters as Character

inEdgeOfTown :: GameMachine
inEdgeOfTown = GameAuto $ do
    movePlace InEdgeOfTown
    notnull  <- not . null . party <$> world
    toCastle <- home
    run $ selectWhen msg [(Key "m", enteringMaze, notnull)
                         ,(Key "t", inTrainingGrounds, True)
                         ,(Key "r", toCastle, True)
                         ,(Key "q", exitGame, True)]
  where
    msg = Message $ "M)aze\n"
                 ++ "T)raining Grounds\n"
                 ++ "R)eturn to the Castle\n"
                 ++ "Q)uit Game\n"

-- =======================================================================

enteringMaze :: GameMachine
enteringMaze = GameAuto $ movePlace EnteringMaze >> run (events [msg] $ openCamp p)
  where
    msg = MessageTime (-1500) "\n\n  *** Entering Test Maze... *** \n\n\n" Nothing
    p   = Position { direction = N, x = 1, y = 1, z = 0 }

-- =======================================================================

inTrainingGrounds :: GameMachine
inTrainingGrounds = GameAuto $ do
    movePlace TrainingGrounds
    modify $ \w -> w { party = [], inTarvernMember = sort (inTarvernMember w ++ party w) }
    run $ selectEsc msg [(Key "l", inEdgeOfTown)
                        ,(Key "c", inTrainingGrounds)
                        ,(Key "q", exitGame)]
  where
    msg = Message $ "C)reate Character\n"
                 ++ "S)how List of Characters\n"
                 ++ "D)elete Character\n"
                 ++ "N)ame Change of Character\n"
                 ++ "J)ob Change of Character\n"
                 ++ "R)eorder List\n"
                 ++ "L)eave [ESC]\n"

-- =======================================================================

exitGame :: GameMachine
exitGame = GameAuto $ return (Exit, const exitGame)




