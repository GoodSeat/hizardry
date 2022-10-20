module InMaze
where

import Control.Monad.State
import Control.Monad.Reader (asks)
import Data.Function
import qualified Data.Map as Map

import GameAuto
import World
import Maze
import qualified MazeEvent as Ev
import Utils
import Primitive
import qualified Enemies as Enemy
import InBattle

import Cui

exitGame' :: GameAuto
exitGame' = GameAuto $ return (Exit, const exitGame')

currentPosition :: GameState Position
currentPosition = do
    plc <- place <$> world
    case plc of InMaze p     -> return p
                InBattle p _ -> return p
                _            -> err "failed on currentPosition."

-- =======================================================================
-- depends on Scenario.

enterWithoutEncount :: Position -> GameAuto
enterWithoutEncount p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev False p

enterMaybeEncount :: Position -> GameAuto
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
          -> GameAuto
enterGrid e probEncount p = GameAuto $ do
    movePlace $ InMaze p
    -- TODO!:all character lost if they are in stone.
    encountId <- if probEncount then checkEncount $ coordOf p else return Nothing
    case e of Nothing   -> case encountId of Nothing -> select None $ moves p
                                             Just ei -> run $ encountEnemy ei
              Just edef -> run $ doEvent edef

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
                          sortPartyAuto
                          -- TODO!:if all character dead, move to gameover.
                          run $ enterMaybeEncount p'

-- =======================================================================

encountEnemy :: Enemy.ID -> GameAuto
encountEnemy id = startBattle id (escapeEvent, escapeEvent)


-- =======================================================================

openCamp :: Position -> GameAuto
openCamp p = GameAuto $ do
    movePlace (Camping p)
    ids <- party <$> world
    selectWhen (Message "#)Inspect\nR)eorder Party\nL)eave Camp")
        [(Key "l", enterWithoutEncount p, True)
        ,(Key "1", inspectCharacter (openCamp p) True 1, length ids >= 1)
        ,(Key "2", inspectCharacter (openCamp p) True 2, length ids >= 2)
        ,(Key "3", inspectCharacter (openCamp p) True 3, length ids >= 3)
        ,(Key "4", inspectCharacter (openCamp p) True 4, length ids >= 4)
        ,(Key "5", inspectCharacter (openCamp p) True 5, length ids >= 5)
        ,(Key "6", inspectCharacter (openCamp p) True 6, length ids >= 6)
        ]

endEvent :: GameAuto
endEvent = GameAuto $ do
    p <- currentPosition
    run $ enterWithoutEncount p

escapeEvent :: GameAuto
escapeEvent = GameAuto $ do
    p <- currentPosition
    run $ enterGrid Nothing False p

-- =======================================================================

doEvent :: Ev.Define -> GameAuto
doEvent edef = doEvent' edef $ doEvent' Ev.Escape undefined
  where
    doEvent' :: Ev.Define -> GameAuto -> GameAuto
    doEvent' Ev.ReturnCastle next = GameAuto $ do
        toCastle <- home
        whenReturnCastle >> run toCastle
    doEvent' (Ev.MoveTo (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        movePlace (InMaze p') >> run next
    doEvent' (Ev.StairsToUpper (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        run $ events' (updownEffect p' True) next
    doEvent' (Ev.StairsToLower (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        run $ events' (updownEffect p' False) next
    doEvent' (Ev.Message msg picID) next = events [Message msg] next
    doEvent' (Ev.Ask msg picID ways) next = GameAuto $ select (Ask msg) ss
      where ss = (\(m, edef) -> (Key m, doEvent edef)) <$> ways
    doEvent' (Ev.Select msg picID ways) next = GameAuto $ select (Message msg) ss
      where ss = (\(m, edef) -> (Key m, doEvent edef)) <$> ways
    doEvent' (Ev.Events []) next = next
    doEvent' (Ev.Events (edef:es)) next = doEvent' edef $ doEvent' (Ev.Events es) next
    doEvent' (Ev.Reference eid) next = GameAuto $ do
      evDB  <- asks mazeEvents
      case Map.lookup eid evDB of Nothing   -> run next
                                  Just edef -> run $ doEvent' edef next
    doEvent' Ev.Escape _ = escapeEvent
    doEvent' Ev.End _    = endEvent

updownEffect :: Position -> Bool -> [(GameState (), Event)]
updownEffect p toUp = replicate c (upStep, Time 150)
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





