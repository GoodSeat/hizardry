module Engine.InEvent
where

import Control.Monad (when)
import Control.Monad.State (modify, gets, forM_)
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

doEvent :: Ev.Define -> (Bool -> GameMachine) -> (Bool -> GameMachine) -> GameMachine
doEvent = doEventInner True

doEventInner :: Bool -> Ev.Define -> (Bool -> GameMachine) -> (Bool -> GameMachine) -> GameMachine
doEventInner isHidden edef whenEscape whenEnd = doEvent' edef whenEscape
  where
    doEvent' :: Ev.Define -> (Bool -> GameMachine) -> GameMachine
    -- moving
    doEvent' Ev.ReturnCastle _ = GameAuto $ do
        toCastle <- home
        returnToCastle >> run toCastle
    doEvent' (Ev.MoveTo (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        when (z' /= z p) resetRoomBattle
        movePlace (InMaze p') >> run (next isHidden)
    doEvent' (Ev.StairsToUpper (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        when (z' /= z p) resetRoomBattle
        run $ events' (updownEffect p' True) (next False)
    doEvent' (Ev.StairsToLower (x', y', z')) next = GameAuto $ do
        p <- currentPosition
        let p' = p { x = x', y = y', z = z' }
        when (z' /= z p) resetRoomBattle
        run $ events' (updownEffect p' False) (next False)

    -- interactive
    doEvent' (Ev.Message msg picID) next = events [MessagePic msg picID] (next False)
    doEvent' (Ev.MessageTime msg picID t) next = events [MessageTime t msg picID] (next False)
    doEvent' (Ev.Select msg picID ways) next = select (MessagePic msg picID) ss
      where ss = (\(m, edef) -> (Key m, doEventInner False edef whenEscape whenEnd)) <$> ways
    doEvent' (Ev.Ask msg picID ways) next = select (Ask msg picID) ss
      where ss = (\(m, edef) -> (Key m, doEventInner False edef whenEscape whenEnd)) <$> ways
    -- in battle

    -- happens
    doEvent' (Ev.Switch []) next = next isHidden
    doEvent' (Ev.Switch (c:cs)) next = GameAuto $ do
      match <- matchCondition (fst c)
      run $ if match then doEvent' (snd c)        next
                     else doEvent' (Ev.Switch cs) next

    doEvent' (Ev.ChangeEventFlag idx f) next = GameAuto $ do
      efs <- eventFlags <$> world
      ps  <- party <$> world
      os  <- mapM characterByID ps
      map <- addEvFlagToFormulaMap $ Ev.formulaMapParty os
      n   <- evalWith map f
      modify $ \w -> w { eventFlags = take idx efs ++ [n] ++ drop (idx + 1) efs }
      run $ next isHidden


    -- others
    doEvent' (Ev.Reference eid) next = GameAuto $ do
      evDB  <- asks mazeEvents
      case Map.lookup eid evDB of Nothing   -> run $ next isHidden
                                  Just edef -> run $ doEvent' edef next
    doEvent' Ev.End    _ = whenEnd    isHidden
    doEvent' Ev.Escape _ = whenEscape isHidden
    doEvent' (Ev.Events [])        next = next isHidden
    doEvent' (Ev.Events (edef:es)) next = doEvent' edef $ \isHidden' -> doEventInner isHidden' (Ev.Events es) whenEscape whenEnd


matchCondition :: Ev.Condition -> GameState Bool
matchCondition (Ev.FormulaCheckParty f) = do
    ps  <- party <$> world
    os  <- mapM characterByID ps
    map <- addEvFlagToFormulaMap $ Ev.formulaMapParty os
    n   <- evalWith map f
    happens n
matchCondition (Ev.FormulaCheckLeader f) = do
    c   <- characterByID . head . party =<< world
    map <- addEvFlagToFormulaMap $ formulaMapS c
    n   <- evalWith map f
    happens n
matchCondition Ev.Otherwise = return True


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
    resetRoomBattle
    setLightValue True  0
    setLightValue False 0
    ps <- party <$> world
    forM_ ps $ \p -> do
      c <- characterByID p
      updateCharacter p $ foldl (&) c (whenReturnCastle <$> statusErrorsOf c)

-- todo: remove dead/stoned/staned characters etc.


resetRoomBattle :: GameState ()
resetRoomBattle = modify $ \w -> w { roomBattled = [] }

setLightValue :: Bool -> Int -> GameState ()
setLightValue super n = modify $ \w -> if super then w { partyLight' = n }
                                                else w { partyLight = n }

setLightValueWith :: Bool -> (Int -> Int) -> GameState ()
setLightValueWith super f = do
    n <- gets (f . if super then partyLight' else partyLight)
    when (n >= 0) $ setLightValue super n

