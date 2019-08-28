module InLabyrinth
where


import GameAuto
import World
import Characters
import Labyrinth
import Utils

exitGame' :: GameAuto
exitGame' = Auto $ return (Exit, const exitGame')

-- =======================================================================
enterGrid :: Position -> GameAuto
enterGrid p = Auto $ do
    movePlace $ InLabyrinth p
    case event allEvents p of
        Nothing -> select (Message $ show p) $ moves p
        Just a  -> run a

turnOnGrid :: Position -> GameAuto
turnOnGrid p = Auto $ do
    movePlace $ InLabyrinth p
    select (Message $ show p) $ moves p

ouch :: Position -> GameAuto
ouch p = Auto $ select (Message "Ouch !!") $ moves p

moves :: Position -> [(Input, GameAuto)]
moves p = [(Key "a", turnOnGrid $ turnLeft p)
          ,(Key "d", turnOnGrid $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", exitGame')
          ]
  where
    goStraight p f = Auto $ do
        lab <- labyrinthAt $ z p
        case f lab p of Nothing -> run $ ouch p
                        Just p' -> run $ enterGrid p'

-- =======================================================================

openCamp :: Position -> GameAuto
openCamp p = Auto $ select (Message "#)Inspect\nR)eorder Party\nL)eave Camp")
        [(Key "l", enterGrid p)]


-- =======================================================================

type Coord = (Int, Int, Int)

event :: [(Coord, Position -> GameAuto)] -> Position -> Maybe GameAuto
event [] _ = Nothing
event (((x', y', z'), e):es) p = if x p == x' && y p == y' && z p == z'
    then Just $ e p
    else event es p

allEvents :: [(Coord, Position -> GameAuto)]
allEvents = [((1, 1, 0), stairsToCastle)]

stairsToCastle :: Position -> GameAuto
stairsToCastle p = Auto $ do
    toCastle <- home
    select (Message "there is climbing stairs.\n...climbing?\n\n(Y/N)")
        [(Key "y", toCastle), (Key "n", turnOnGrid p)]


-- =======================================================================





