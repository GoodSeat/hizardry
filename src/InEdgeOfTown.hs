module InEdgeOfTown
where


import GameAuto
import World
import Characters
import Utils
import InLabyrinth
import Labyrinth


inEdgeOfTown :: GameAuto
inEdgeOfTown = Auto $ do
    movePlace InEdgeOfTown
    let msg = Message $ "M)aze\n" ++
                        "T)raining Post\n" ++
                        "R)eturn to the Castle\n" ++
                        "Q)uit Game\n"
    notnull <- not . null . party <$> world
    toCastle <- home
    selectWhen msg [(Key "m", enteringMaze, notnull)
                   ,(Key "t", inTrainingPost, True)
                   ,(Key "r", toCastle, True)
                   ,(Key "q", exitGame, True)]

-- =======================================================================

enteringMaze :: GameAuto
enteringMaze = events [Message "\n\n  *** Entering Test Maze... *** \n\n\n"] $ openCamp p
  where
    p = Position { direction = N, x = 1, y = 1, z = 0 }

-- =======================================================================

inTrainingPost :: GameAuto
inTrainingPost = undefined

-- =======================================================================

exitGame :: GameAuto
exitGame = Auto $ return (Exit, const exitGame)




