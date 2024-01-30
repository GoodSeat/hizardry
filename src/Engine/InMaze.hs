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
import Engine.InEvent (doEvent, setLightValueWith, setLightValue)
import Engine.CharacterAction (inspectCharacter)
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.GameEvent as Ev

exitGame' :: GameMachine
exitGame' = GameAuto $ return (Exit, const exitGame')

-- =======================================================================
-- depends on Scenario.

enterWithoutEncount :: Event -> Position -> GameMachine
enterWithoutEncount evMoved p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev False evMoved p

enterMaybeEncount :: Event -> Position -> GameMachine
enterMaybeEncount evMoved p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev True evMoved p

enterMaybeEncount' :: Event -> Position -> GameMachine
enterMaybeEncount' evMoved p = GameAuto $ do
  ev <- eventOn' p
  run $ enterGrid ev True evMoved p

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
          -> Event           -- ^ event when moved.
          -> Position        -- ^ moved position.
          -> GameMachine
enterGrid e probEncount evMoved p = GameAuto $ do
    movePlace $ InMaze p
    modify $ \w -> w { visitHitory = Map.insert (coordOf p) True (visitHitory w) }
    -- TODO!:all character lost if they are in stone.
    lab <- mazeAt $ z p
    let c = coordOf p
    when (Dark `elem` noticesInView lab p 0 0) $ setLightValue False 0
    encount <- if probEncount then
                 if visiblityAt lab p 0 0 B /= Passage then checkRoomBattle c
                 else fmap (,False) <$> checkEncount c False
               else return Nothing
    case e of Just edef -> run $ doEvent edef (escapeEvent evMoved) (endEvent evMoved)
              Nothing   -> case encount of
                Nothing         -> run $ with [updateRoomVisit] (select evMoved $ moves p)
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
ouch p = select (FlashMessage (-500) " Ouch !! ") $ moves p

flashMoveView :: String -> Event
flashMoveView = FlashMessage (-100)

moves :: Position -> [(Input, GameMachine)]
moves p = [(Key "a", enterMaybeEncount' (flashMoveView " <- ") $ turnLeft p)
          ,(Key "d", enterMaybeEncount' (flashMoveView " -> ") $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", exitGame')
          ,(Key "s", with [modify (\w -> w { statusOn = not $ statusOn w })] (select None $ moves p))
          ,(Key "o", with [modify (\w -> w { guideOn  = not $ guideOn  w })] (select None $ moves p))
          ,(Key "m", with [nextMiniMap] (select None $ moves p))
          ,(Key " "   , select None $ moves p)
          ,(Key "\ESC", select None $ moves p)
          ,(Clock     , select None $ moves p)
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
            run $ enterMaybeEncount (flashMoveView " 1  ") p'


nextMiniMap :: GameState ()
nextMiniMap = do
    o  <- minimapType . worldOption <$> world
    os <- enableMinimapType <$> option
    let nm = if length os <= 1 then o
                               else next o os os
    modify (\w -> w { worldOption = (worldOption w) { minimapType = nm } })
  where
    next a org []  = head org
    next a org [n] = head org
    next a org (n:ns) | n == a = head ns
                      | n /= a = next a org ns


-- =======================================================================

encountEnemy :: EnemyID -> Bool -> GameMachine
encountEnemy id isRB = startBattle id isRB (with [updateRoomVisit] (escapeEvent None False)
                                           ,with [when isRB backfoward] (escapeEvent None False))

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
          [(Key "l", enterWithoutEncount None p, True)
          ,(Key "1", inspectCharacter (openCamp p) True F1, np >= 1)
          ,(Key "2", inspectCharacter (openCamp p) True F2, np >= 2)
          ,(Key "3", inspectCharacter (openCamp p) True F3, np >= 3)
          ,(Key "4", inspectCharacter (openCamp p) True B4, np >= 4)
          ,(Key "5", inspectCharacter (openCamp p) True B5, np >= 5)
          ,(Key "6", inspectCharacter (openCamp p) True B6, np >= 6)
          ]

endEvent :: Event -> Bool -> GameMachine
endEvent ev isHidden = GameAuto $ do
    p <- currentPosition
    run $ if isHidden then enterWithoutEncount ev   p
                      else enterWithoutEncount None p

escapeEvent :: Event -> Bool -> GameMachine
escapeEvent ev isHidden = GameAuto $ do
    p <- currentPosition
    run $ if isHidden then enterGrid Nothing False ev p
                      else enterGrid Nothing False None p

backfoward :: GameState ()
backfoward = sortPartyAuto >> ((movePlace . InMaze) . moveBack =<< currentPosition)


-- =======================================================================





