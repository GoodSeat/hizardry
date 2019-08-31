module InMaze
where

import Control.Monad.State

import GameAuto
import World
import Characters
import Maze
import Utils

exitGame' :: GameAuto
exitGame' = Auto $ return (Exit, const exitGame')

-- =======================================================================
enterGrid :: Maybe GameAuto -> Position -> GameAuto
enterGrid e p = Auto $ do
    movePlace $ InMaze p
    case e of Nothing -> select None $ moves p
              Just a  -> run a

ouch :: Position -> GameAuto
ouch p = Auto $ select (Message "Ouch !!") $ moves p

moves :: Position -> [(Input, GameAuto)]
moves p = [(Key "a", enterGrid Nothing $ turnLeft p)
          ,(Key "d", enterGrid Nothing $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", exitGame')
          ,(Key "s", Auto $ modify (\w -> w { statusOn = not $ statusOn w }) >> run (enterGrid Nothing p))
          ,(Key "o", Auto $ modify (\w -> w { guideOn = not $ guideOn w }) >> run (enterGrid Nothing p))
          ]
  where
    goStraight p f = Auto $ do
        lab <- mazeAt $ z p
        case f lab p of Nothing -> run $ ouch p
                        Just p' -> run $ enterGrid (eventOn allEvents p') p'

-- =======================================================================

openCamp :: Position -> GameAuto
openCamp p = Auto $ movePlace (Camping p) >> select (Message "#)Inspect\nR)eorder Party\nL)eave Camp")
        [(Key "l", enterGrid (eventOn allEvents p) p)]


-- =======================================================================

type Coord = (Int, Int, Int)

eventOn :: [(Coord, GameAuto)] -> Position -> Maybe GameAuto
eventOn [] _ = Nothing
eventOn (((x', y', z'), e):es) p = if x p == x' && y p == y' && z p == z'
    then Just $ e
    else eventOn es p

allEvents :: [(Coord, GameAuto)]
allEvents = [((1, 1, 0), stairsToCastle)]

escapeEvent :: GameAuto
escapeEvent = Auto $ do
    plc <- place <$> world
    case plc of InMaze p -> run $ enterGrid Nothing p
                _        -> err "failed on escapeEvent."

stairsToCastle :: GameAuto
stairsToCastle = Auto $ do
    toCastle <- home
    select (Message "there is climbing stairs.\n...climbing?\n\n(Y/N)")
        [(Key "y", Auto $ whenReturnCastle >> run toCastle), (Key "n", escapeEvent)]

-- | state machine when return to castle.
whenReturnCastle :: GameState ()
whenReturnCastle = return ()
-- todo: cure poison, remove dead/stoned/staned characters etc.

-- =======================================================================





