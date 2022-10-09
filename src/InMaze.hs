module InMaze
where

import Control.Monad.State

import GameAuto
import World
import Characters
import Maze
import Utils
import qualified Enemies as Enemy
import InBattle

exitGame' :: GameAuto
exitGame' = GameAuto $ return (Exit, const exitGame')

-- =======================================================================
enterGrid :: Maybe GameAuto -- ^ happened event.
          -> Bool           -- ^ probably encount enemy.
          -> Position       -- ^ moved position.
          -> GameAuto
enterGrid e probEncount p = GameAuto $ do
    movePlace $ InMaze p
    encountId <- if probEncount then checkEncount $ coordOf p else return Nothing
    case e of Nothing -> case encountId of Nothing -> select None $ moves p
                                           Just ei -> run $ encountEnemy ei
              Just a  -> run a

ouch :: Position -> GameAuto
ouch p = GameAuto $ select (Message "Ouch !!") $ moves p

moves :: Position -> [(Input, GameAuto)]
moves p = [(Key "a", enterGrid Nothing True $ turnLeft p)
          ,(Key "d", enterGrid Nothing True $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", exitGame')
          ,(Key "s", GameAuto $ modify (\w -> w { statusOn = not $ statusOn w }) >> run (enterGrid Nothing False p))
          ,(Key "o", GameAuto $ modify (\w -> w { guideOn = not $ guideOn w })   >> run (enterGrid Nothing False p))
          ]
  where
    goStraight p f = GameAuto $ do
        lab <- mazeAt $ z p
        case f lab p of Nothing -> run $ ouch p
                        Just p' -> run $ enterGrid (eventOn allEvents p') True p'

-- =======================================================================

encountEnemy :: Enemy.ID -> GameAuto
encountEnemy id = startBattle id (escapeEvent, escapeEvent)


-- =======================================================================

openCamp :: Position -> GameAuto
openCamp p = GameAuto $ movePlace (Camping p) >> select (Message "#)Inspect\nR)eorder Party\nL)eave Camp")
        [(Key "l", enterGrid (eventOn allEvents p) False p)]


-- =======================================================================

eventOn :: [(Coord, GameAuto)] -> Position -> Maybe GameAuto
eventOn [] _ = Nothing
eventOn (((x', y', z'), e):es) p = if x p == x' && y p == y' && z p == z' then Just e else eventOn es p

allEvents :: [(Coord, GameAuto)]
allEvents = [((1, 1, 0), stairsToCastle)]

escapeEvent :: GameAuto
escapeEvent = GameAuto $ do
    plc <- place <$> world
    case plc of InMaze p     -> run $ enterGrid Nothing False p
                InBattle p _ -> run $ enterGrid Nothing False p
                _            -> err "failed on escapeEvent."

stairsToCastle :: GameAuto
stairsToCastle = GameAuto $ do
    toCastle <- home
    select (Message "there is climbing stairs.\n...climbing?\n\n(Y/N)")
        [(Key "y", GameAuto $ whenReturnCastle >> run toCastle), (Key "n", escapeEvent)]

-- | state machine when return to castle.
whenReturnCastle :: GameState ()
whenReturnCastle = return ()
-- todo: cure poison, remove dead/stoned/staned characters etc.

-- =======================================================================





