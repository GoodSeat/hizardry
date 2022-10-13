module InMaze
where

import Control.Monad.State
import Data.Function

import GameAuto
import World
import Maze
import Utils
import Primitive
import qualified Enemies as Enemy
import InBattle

import Cui

exitGame' :: GameAuto
exitGame' = GameAuto $ return (Exit, const exitGame')

currentPosition = do
    plc <- place <$> world
    case plc of InMaze p     -> return p
                InBattle p _ -> return p
                _            -> err "failed on currentPosition."

-- =======================================================================
-- depends on Scenario.

enterWithoutEncount :: Position -> GameAuto
enterWithoutEncount p = enterGrid (eventOn allEvents p) False p

enterMaybeEncount :: Position -> GameAuto
enterMaybeEncount p = enterGrid (eventOn allEvents p) True p

allEvents :: [(Coord, GameAuto)]
allEvents = [((1, 1, 0), stairsToCastle)
            ,((2, 4, 0), stairsToLower (2, 4, 1))
            ,((2, 4, 1), stairsToUpper (2, 4, 0))
            ]

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
                          run $ enterMaybeEncount p'

-- =======================================================================

encountEnemy :: Enemy.ID -> GameAuto
encountEnemy id = startBattle id (escapeEvent, escapeEvent)


-- =======================================================================

openCamp :: Position -> GameAuto
openCamp p = GameAuto $ movePlace (Camping p) >> select (Message "#)Inspect\nR)eorder Party\nL)eave Camp")
        [(Key "l", enterWithoutEncount p)]


-- =======================================================================

eventOn :: [(Coord, GameAuto)] -> Position -> Maybe GameAuto
eventOn [] _ = Nothing
eventOn (((x', y', z'), e):es) p = if x p == x' && y p == y' && z p == z' then Just e else eventOn es p

escapeEvent :: GameAuto
escapeEvent = GameAuto $ do
    p <- currentPosition
    run $ enterGrid Nothing False p

stairsToCastle :: GameAuto
stairsToCastle = GameAuto $ do
    toCastle <- home
    let upStep  = modify (\w -> w { sceneTrans = sceneTrans w . translate (0, 5) })
    let upReset = modify (\w -> w { sceneTrans = id })
    select (Message "there is climbing stairs.\n...climbing?\n\n(Y/N)")
        [ (Key "y", events' (replicate 3 (upStep, Time 300)) (GameAuto $ upReset >> whenReturnCastle >> run toCastle))
        , (Key "n", escapeEvent) ]

stairsToLower :: Coord -> GameAuto
stairsToLower (x', y', z') = GameAuto $ do
    p <- currentPosition
    let p' = p { x = x', y = y', z = z' }
    select (Message "there is ladder to go down.\n...go down?\n\n(Y/N)")
        [ (Key "y", events' (upEffect p' False) (enterWithoutEncount p'))
        , (Key "n", escapeEvent) ]

stairsToUpper :: Coord -> GameAuto
stairsToUpper (x', y', z') = GameAuto $ do
    p <- currentPosition
    let p' = p { x = x', y = y', z = z' }
    select (Message "there is ladder to go up.\n...go up?\n\n(Y/N)")
        [ (Key "y", events' (upEffect p' True) (enterWithoutEncount p'))
        , (Key "n", escapeEvent) ]

upEffect :: Position -> Bool -> [(GameState (), Event)]
upEffect p toUp = replicate c (upStep, Time 150)
               ++ [(upRest >> movePlace (InMaze p), Time 150)]
               ++ replicate c (upStep, Time 150)
  where
    r = if toUp then 1 else -1
    u = 5 -- translate length by step.
    c = 4 -- step count.
    upStep = modify (\w -> w { sceneTrans = sceneTrans w . translate (0, u * r) })
    upRest = modify (\w -> w { sceneTrans = translate (0, -u * c * r) })
    


-- | state machine when return to castle.
whenReturnCastle :: GameState ()
whenReturnCastle = do
    ps <- party <$> world
    forM_ ps $ \p -> do
      c <- characterOf p
      updateCharacter p $ foldl (&) c (whenToNextCastle <$> statusErrorsOf c) 

-- todo: remove dead/stoned/staned characters etc.

-- =======================================================================





