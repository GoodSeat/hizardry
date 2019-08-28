module InEdgeOfTown
where


import GameAuto
import World
import Characters
import Utils


inEdgeOfTown :: GameAuto
inEdgeOfTown = Auto $ do
    movePlace InEdgeOfTown
    let msg = Message $ "E)ntering Dunsion\n" ++
                        "T)raining Post\n" ++
                        "R)eturn to the Castle\n" ++
                        "Q)uit Game\n"
    notnull <- not . null . party <$> world
    toCastle <- home
    selectWhen msg [(Key "e", enteringDunsion, notnull)
                   ,(Key "t", inTrainingPost, True)
                   ,(Key "r", toCastle, True)
                   ,(Key "q", exitGame, True)]

-- =======================================================================

enteringDunsion :: GameAuto
enteringDunsion = undefined

-- =======================================================================

inTrainingPost :: GameAuto
inTrainingPost = undefined

-- =======================================================================

exitGame :: GameAuto
exitGame = Auto $ return (Exit, const exitGame)




