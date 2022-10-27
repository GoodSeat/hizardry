module InEdgeOfTown
where


import GameAuto
import World
import qualified Characters as Character
import Utils
import InMaze
import Maze

inEdgeOfTown :: GameMachine
inEdgeOfTown = GameAuto $ do
    movePlace InEdgeOfTown
    let msg = Message $ "M)aze\n" ++
                        "T)raining Grounds\n" ++
                        "R)eturn to the Castle\n" ++
                        "Q)uit Game\n"
    notnull  <- not . null . party <$> world
    toCastle <- home
    run $ selectWhen msg [(Key "m", enteringMaze, notnull)
                         ,(Key "t", inTrainingGrounds, True)
                         ,(Key "r", toCastle, True)
                         ,(Key "q", exitGame, True)]

-- =======================================================================

enteringMaze :: GameMachine
enteringMaze = events [msg] $ openCamp p
  where
    msg = MessageTime (-1500) "\n\n  *** Entering Test Maze... *** \n\n\n"
    p   = Position { direction = N, x = 1, y = 1, z = 0 }

-- =======================================================================

inTrainingGrounds :: GameMachine
inTrainingGrounds = undefined

-- =======================================================================

exitGame :: GameMachine
exitGame = GameAuto $ return (Exit, const exitGame)




