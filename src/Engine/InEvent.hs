module Engine.InEvent
where

import Control.Monad.State (modify, forM_)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import qualified Data.Map as Map

import Data.Maze
import Data.World
import Data.Primitive
import Engine.GameAuto
import Engine.Utils
import qualified Data.GameEvent as Ev

import Control.CUI (translate)

-- =======================================================================

doEvent :: Ev.Define -> GameMachine -> GameMachine -> GameMachine
doEvent edef whenEscape whenEnd = doEvent' edef $ doEvent' Ev.Escape undefined
  where
    doEvent' :: Ev.Define -> GameMachine -> GameMachine
    -- moving
    doEvent' Ev.ReturnCastle next = GameAuto $ do
        toCastle <- home
        returnToCastle >> run toCastle
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
    -- interactive
    doEvent' (Ev.Message msg picID) next = events [MessagePic msg picID] next
    doEvent' (Ev.Ask msg picID ways) next = select (Ask msg picID) ss
      where ss = (\(m, edef) -> (Key m, doEvent edef whenEscape whenEnd)) <$> ways
    doEvent' (Ev.Select msg picID ways) next = select (MessagePic msg picID) ss
      where ss = (\(m, edef) -> (Key m, doEvent edef whenEscape whenEnd)) <$> ways
    -- in battle

    -- happens

    -- others
    doEvent' (Ev.Reference eid) next = GameAuto $ do
      evDB  <- asks mazeEvents
      case Map.lookup eid evDB of Nothing   -> run next
                                  Just edef -> run $ doEvent' edef next
    doEvent' Ev.End _    = whenEnd
    doEvent' Ev.Escape _ = whenEscape
    doEvent' (Ev.Events [])        next = next
    doEvent' (Ev.Events (edef:es)) next = doEvent' edef $ doEvent' (Ev.Events es) next

updownEffect :: Position -> Bool -> [(GameState (), Event)]
updownEffect p toUp = replicate c (upStep, Time 150 Nothing)
                   ++ [(upRest >> movePlace (InMaze p), Time 150 Nothing)]
                   ++ replicate c (upStep, Time 150 Nothing)
  where
    r = if toUp then 1 else -1
    u = 5 -- translate length by step.
    c = 4 -- step count.
    upStep = modify (\w -> w { sceneTrans = sceneTrans w . translate (0, u * r) })
    upRest = modify (\w -> w { sceneTrans = translate (0, -u * c * r) })
    

-- | state machine when return to castle.
returnToCastle :: GameState ()
returnToCastle = do
    ps <- party <$> world
    forM_ ps $ \p -> do
      c <- characterOf p
      updateCharacter p $ foldl (&) c (whenReturnCastle <$> statusErrorsOf c) 

-- todo: remove dead/stoned/staned characters etc.



