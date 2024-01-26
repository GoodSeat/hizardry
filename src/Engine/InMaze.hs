{-# LANGUAGE TupleSections #-}
module Engine.InMaze (openCamp) where

import Control.Monad (when)
import Control.Monad.State (modify, forM_, guard)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InBattle
import Engine.InEvent (doEvent, setLightValueWith)
import Engine.CharacterAction (inspectCharacter)
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.GameEvent as Ev

exitGame' :: GameMachine
exitGame' = GameAuto $ return (Exit, const exitGame')

-- =======================================================================
-- depends on Scenario.

enterWithoutEncount :: Position -> GameMachine
enterWithoutEncount p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev False p

enterMaybeEncount :: Position -> GameMachine
enterMaybeEncount p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev True p

enterMaybeEncount' :: Position -> GameMachine
enterMaybeEncount' p = GameAuto $ do
  ev <- eventOn' p
  run $ enterGrid ev True p

eventOn :: Position -> GameState (Maybe Ev.Define)
eventOn p = do
  evMap <- asks eventMap
  evDB  <- asks mazeEvents
  case Map.lookup (x p, y p, z p) evMap of
    Nothing  -> eventOn' p
    Just eid -> return $ Map.lookup eid evDB

eventOn' :: Position -> GameState (Maybe Ev.Define)
eventOn' p = do
  evMap <- asks eventMapDir
  evDB  <- asks mazeEvents
  case Map.lookup p evMap of
    Nothing  -> return Nothing
    Just eid -> return $ Map.lookup eid evDB

-- =======================================================================

enterGrid :: Maybe Ev.Define -- ^ happened event.
          -> Bool            -- ^ probably encount enemy.
          -> Position        -- ^ moved position.
          -> GameMachine
enterGrid e probEncount p = GameAuto $ do
    movePlace $ InMaze p
    modify $ \w -> w { visitHitory = Map.insert (coordOf p) True (visitHitory w) }
    -- TODO!:all character lost if they are in stone.
    lab <- mazeAt $ z p
    let c = coordOf p
    encount <- if probEncount then
                 if visiblityAt lab p 0 0 B /= Passage then checkRoomBattle c
                 else fmap (,False) <$> checkEncount c False
               else return Nothing
    case e of Just edef -> run $ doEvent edef escapeEvent endEvent
              Nothing   -> case encount of
                Nothing         -> run $ with [updateRoomVisit] (select None $ moves p)
                Just (ei, isRB) -> run $ encountEnemy ei isRB
              

checkRoomBattle :: Coord -> GameState (Maybe (EnemyID, Bool))
checkRoomBattle c = do
    isRB <- notElem c . roomBattled <$> world
    res  <- checkEncount c isRB
    return $ (,isRB) <$> res

checkEncount :: Coord -> Bool -> GameState (Maybe EnemyID)
checkEncount c checkRoomBattle = do
    emap <- if checkRoomBattle then asks roomBattleMap else asks encountMap
    r    <- randomNext 1 100
    let es' = do {
        (prob, es) <- Map.lookup c emap;
        guard $ r < prob;
        return es
        }
    case es' of Nothing -> return Nothing
                Just es -> Just <$> randomIn es


ouch :: Position -> GameMachine
ouch p = select (Message "Ouch !!") $ moves p

moves :: Position -> [(Input, GameMachine)]
moves p = [(Key "a", enterMaybeEncount' $ turnLeft p)
          ,(Key "d", enterMaybeEncount' $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", exitGame')
          ,(Key "s", with [modify (\w -> w { statusOn = not $ statusOn w })] (select None $ moves p))
          ,(Key "o", with [modify (\w -> w { guideOn  = not $ guideOn  w })] (select None $ moves p))
          ,(Key " "   , select None $ moves p)
          ,(Key "\ESC", select None $ moves p)
          ]
  where
    goStraight p f = GameAuto $ do
        lab <- mazeAt $ z p
        case f lab p of
          Nothing -> run $ ouch p
          Just p' -> do 
            -- update milwa effect.
            setLightValueWith True  (\n -> n - 1)
            setLightValueWith False (\n -> n - 1)
            -- update party status.
            ps <- party <$> world
            forM_ ps $ \p -> do
              c <- characterByID p
              updateCharacter p $ foldl (&) c (whenWalking <$> statusErrorsOf c) 
            sortPartyAuto
            -- TODO!:if all character dead, move to gameover.
            run $ enterMaybeEncount p'

-- =======================================================================

encountEnemy :: EnemyID -> Bool -> GameMachine
encountEnemy id isRB = startBattle id isRB (with [updateRoomVisit] escapeEvent
                                           ,with [when isRB backfoward] escapeEvent)

updateRoomVisit :: GameState ()
updateRoomVisit = do
    c  <- coordOf <$> currentPosition
    rd <- asks roomDefine
    let cas = fromMaybe [c] $ find (elem c) rd
    modify $ \w -> w { roomBattled = nub $ cas ++ roomBattled w }

-- =======================================================================

openCamp :: Position -> GameMachine
openCamp p = GameAuto $ do
    movePlace (Camping p)
    np <- length . party <$> world
    run $ selectWhenEsc (Message "^#)Inspect\n^R)eorder Party\n^L)eave Camp `[`E`S`C`]")
          [(Key "l", enterWithoutEncount p, True)
          ,(Key "1", inspectCharacter (openCamp p) True F1, np >= 1)
          ,(Key "2", inspectCharacter (openCamp p) True F2, np >= 2)
          ,(Key "3", inspectCharacter (openCamp p) True F3, np >= 3)
          ,(Key "4", inspectCharacter (openCamp p) True B4, np >= 4)
          ,(Key "5", inspectCharacter (openCamp p) True B5, np >= 5)
          ,(Key "6", inspectCharacter (openCamp p) True B6, np >= 6)
          ]

endEvent :: GameMachine
endEvent = GameAuto $ do
    p <- currentPosition
    run $ enterWithoutEncount p

escapeEvent :: GameMachine
escapeEvent = GameAuto $ do
    p <- currentPosition
    run $ enterGrid Nothing False p

backfoward :: GameState ()
backfoward = sortPartyAuto >> ((movePlace . InMaze) . moveBack =<< currentPosition)


-- =======================================================================





