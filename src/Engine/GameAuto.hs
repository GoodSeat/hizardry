{-# LANGUAGE TupleSections #-}
module Engine.GameAuto (
      module Data.PlayEvent
    , module Engine.GameAuto
    ) where

import PreludeL
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map

import Data.Primitive
import Data.PlayEvent
import Data.World
import Data.Maze
import Data.Characters
import qualified Data.GameEvent as GameEvent
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item
import qualified Data.RomajiToHiragana as R2H

-- ==========================================================================

data ScenarioOption = ScenarioOption {
      enableEffectDumapic :: [Spell.CheckLocationType]
    , enableMinimapType   :: [MiniMapType]
    }
      

-- | scenario immutable data.
data Scenario = Scenario {
      scenarioName     :: !String -- ^ use this name as save file path.
    , scenarioVersion  :: ![Int]
    , scenarioOption   :: !ScenarioOption
    , scenarioHome     :: !GameMachine
    , racies           :: ![Race]
    , jobs             :: ![Job]
    , mazes            :: Int -> GameState (Maybe (String, Size2D, Maze))
    , encountMap       :: !(Map.Map Coord (Int, [EnemyID]))
    , roomBattleMap    :: !(Map.Map Coord (Int, [EnemyID]))
    , roomDefine       :: ![[Coord]]
    , eventMap         :: !(Map.Map Coord GameEventID)
    , eventMapDir      :: !(Map.Map Position GameEventID)
    , eventInspect     :: !(Map.Map Position GameEventID)
    , mazeEvents       :: !GameEvent.DB
    , enemies          :: !Enemy.DB
    , spells           :: !Spell.DB
    , items            :: !Item.DB
    , scenarioFormulas :: !ScenarioFormulas
    , encKey           :: !String
    }

data InitScenario = InitScenario {
      initScenarioName     :: !String
    , initScenarioVersion  :: ![Int]
    , initScenarioOption   :: !ScenarioOption
    , initRacies           :: ![Race]
    , initJobs             :: ![Job]
    , initMazes            :: ![(String, Size2D, Maze)]
    , initEncountMap       :: !(Map.Map Coord (Int, [EnemyID]))
    , initRoomBattleMap    :: !(Map.Map Coord (Int, [EnemyID]))
    , initRoomDefine       :: ![[Coord]]
    , initEventMap         :: !(Map.Map Coord GameEventID)
    , initEventMapDir      :: !(Map.Map Position GameEventID)
    , initEventInspect     :: !(Map.Map Position GameEventID)
    , initMazeEvents       :: !GameEvent.DB
    , initEnemies          :: !Enemy.DB
    , initSpells           :: !Spell.DB
    , initItems            :: !Item.DB
    , initScenarioFormulas :: !ScenarioFormulas
    , initEncKey           :: !String
    }

initScenario :: InitScenario -> GameMachine -> Scenario
initScenario i home = Scenario {
      scenarioName              = initScenarioName     i
    , scenarioVersion           = initScenarioVersion  i
    , scenarioOption            = initScenarioOption   i
    , scenarioHome              = home
    , racies                    = initRacies           i
    , jobs                      = initJobs             i
    , mazes                     = \z -> pure $ if z < length (initMazes i) then Just (initMazes i !! z) else Nothing
    , encountMap                = initEncountMap       i
    , roomBattleMap             = initRoomBattleMap    i
    , roomDefine                = initRoomDefine       i
    , eventMap                  = initEventMap         i
    , eventMapDir               = initEventMapDir      i
    , eventInspect              = initEventInspect     i
    , mazeEvents                = initMazeEvents       i
    , enemies                   = initEnemies          i
    , Engine.GameAuto.spells    = initSpells           i
    , Engine.GameAuto.items     = initItems            i
    , scenarioFormulas          = initScenarioFormulas i
    , encKey                    = initEncKey           i
}

data ScenarioFormulas = ScenarioFormulas {
      invokeTrapInspectProb :: !String -- ^ probality when invoke trap when failed inspect trap(%).
    , invokeTrapDisarmProb  :: !String -- ^ probality when invoke trap when failed disarm trap(%).
    , fleeSucceedProb       :: !String -- ^ probality succeed to flee(%).
    }
defaultFormulas ::ScenarioFormulas
defaultFormulas = ScenarioFormulas {
      invokeTrapInspectProb = "100*(19-agi)/20"
    , invokeTrapDisarmProb  = "100*(20-agi)/20"
    , fleeSucceedProb       = "100-enemyCount/2-partySize*3"
    }

-- ==========================================================================

-- | State used in game.
type GameState o = ExceptT String (StateT World (Reader Scenario)) o

runGameState :: Scenario -> World -> GameState a -> (Either String a, World)
runGameState s w g = runReader (runStateT (runExceptT g) w) s

-- | Automaton used in Game.
newtype GameAuto i o = GameAuto { run :: GameState (o, i -> GameAuto i o) }

instance Functor (GameAuto i) where
  fmap f (GameAuto s) = GameAuto $ do
    (o, next) <- s
    return (f o, fmap f . next)

instance Applicative (GameAuto i) where
    pure o = GameAuto $ return (o, const $ pure o)
    (GameAuto sf) <*> (GameAuto sx) = GameAuto $ do
        (f, next_f) <- sf
        (x, next_x) <- sx
        return (f x, \i -> next_f i <*> next_x i)

instance Monad (GameAuto i) where
    (GameAuto m) >>= k = GameAuto $ do
        (a, m') <- m
        let (GameAuto next_m) = k a
        (b, _) <- next_m
        return (b, \i -> m' i >>= k)
      
-- | GameMachine by Automaton.
type GameMachine = GameAuto Input Event

addEff :: (Event -> Event) -> GameMachine -> GameMachine
addEff f g = GameAuto $ do
  (o, next) <- run g
  return (f o, next)

-- ==========================================================================
--
type DisplayIO = Event -> World -> IO ()
type InputIO   = InputType -> IO Input

type UpdateBackUpList = Scenario -> IO [String]
type SavingGame  = Int    -- ^ slot id
                -> String -- ^ tag
                -> Scenario
                -> World
                -> IO (Maybe World)
type LoadingGame = Int    -- ^ slot id
                -> Scenario
                -> IO (Maybe World)

runGame :: DisplayIO    -- ^ renderer of game.
        -> InputIO      -- ^ input command.
        -> UpdateBackUpList
        -> SavingGame
        -> LoadingGame
        -> Scenario     -- ^ game scenario.
        -> World        -- ^ current environment.
        -> GameMachine  -- ^ target GameMachine.
        -> IO World
runGame = runGameE None

runGameE :: Event        -- ^ initial event.
         -> DisplayIO    -- ^ renderer of game.
         -> InputIO      -- ^ input command.
         -> UpdateBackUpList
         -> SavingGame
         -> LoadingGame
         -> Scenario     -- ^ game scenario.
         -> World        -- ^ current environment.
         -> GameMachine  -- ^ target GameMachine.
         -> IO World
runGameE ei render cmd updateBackupList saving loading s = runGame' True ei
  where
    runGame' loadBackupList e0 w g = do
        wp <- if not loadBackupList then return w else do
                ls <- updateBackupList s 
                return $ w { backUpSlotInfo = ls }
        let (e1, w', next') = stepGame s wp g
        case e1 of
          Exit -> return w'
          _ -> do
            (wn, emsg) <- case e1 of SaveGame i tag -> do
                                       wh <- saving i tag s w'
                                       return $ case wh of Just wh' -> (wh', "")
                                                           Nothing  -> (w' , "* failed data saving *")
                                     LoadGame i     -> do
                                       wh <- loading i s
                                       return $ case wh of Just wh' -> (wh', "")
                                                           Nothing  -> (w' , "* failed data loading *")
                                     _              -> return (w', "")

            let e2 | not (null emsg) = changeFlashTime emsg 3000 e0
                   | otherwise       = case e1 of Resume f -> f e0
                                                  _        -> e1
            let itype = nextInputType e2

            let (needUpdateBackupList, eu) = case e2 of SaveGame _ _ -> (True , e0) -- use before event for display.
                                                        LoadGame _   -> (True , e0) -- use before event for display.
                                                        _            -> (False, e2)
            render eu wn

            i <- cmd itype
            let w'' = wn { debugMessage = show i : debugMessage wn }
            runGame' needUpdateBackupList eu w'' (next' i)

loadGame :: [Input]
         -> DisplayIO   -- ^ renderer of game.
         -> InputIO     -- ^ input command.
         -> UpdateBackUpList
         -> SavingGame
         -> LoadingGame
         -> Scenario    -- ^ game scenario.
         -> World       -- ^ initial environment.
         -> GameMachine -- ^ initial GameMachine.
         -> IO World
loadGame = loadGameE None
  where
    loadGameE e0 [] render cmd update saving loading s w g = runGameE e0 render cmd update saving loading s w g
    loadGameE _  (i:is) render cmd update saving loading s w g = do
        let (e, w', next') = stepGame s w g
        loadGameE e is render cmd update saving loading s w' (next' i)


stepGame :: Scenario
         -> World
         -> GameMachine
         -> (Event, World, Input -> GameMachine)
stepGame s w g = let (res, w') = runGameState s w $ run g in case res of
    Left msg     -> let end = GameAuto $ return (Exit, const g) in (message msg, w', const end)
    Right (e, n) -> (e, w', n)


nextInputType :: Event -> InputType
nextInputType e = case e of
    General (Display _ _ _ w _ n _ _)
      | isJust w  -> WaitClock $ fromJust w 
      | n         -> SequenceKey
      | otherwise -> SingleKey
    ShowStatus _ _ (Display _ _ _ w _ n _ _)
      | isJust w  -> WaitClock $ fromJust w 
      | n         -> SequenceKey
      | otherwise -> SingleKey
    SaveGame _ _  -> WaitClock 1
    LoadGame _    -> WaitClock 1
    _             -> SingleKey

-- ==========================================================================

events :: [Event] -> GameMachine -> GameMachine
events es = events' $ map (return (),) es

events' :: [(GameState a, Event)] -> GameMachine -> GameMachine
events' []           next = next
events' ((gs, e):es) next = GameAuto $ gs >> return (e, const $ events' es next)


selectWhen1 :: Event -> [(Input, GameMachine, Bool)] -> GameMachine
selectWhen1 = selectWhen' " "

selectWhenEsc :: Event -> [(Input, GameMachine, Bool)] -> GameMachine
selectWhenEsc = selectWhen' "\ESC"

selectWhen' :: String -> Event -> [(Input, GameMachine, Bool)] -> GameMachine
selectWhen' k e ns = selectWhen e ns'
  where
    fs ((_,g,True) :_)  = [(Key k, g, True)]
    fs ((_,_,False):cs) = fs cs
    fs []               = []
    ns'                 = fs ns ++ ns

selectWhen :: Event -> [(Input, GameMachine, Bool)] -> GameMachine
selectWhen e ns = GameAuto $ return (e, select' ns)
  where
    select' ((AnyKey, s1, enable):ns) (Key k) = if enable then s1 else select' ns (Key k)
    select' ((i1, s1, enable):ns) (Key k)     = if i1 `elem` (Key <$> addHiragana k) && enable then s1 else select' ns (Key k)
    select' ((i1, s1, enable):ns) i           = if i == i1 && enable then s1 else select' ns i
    select' [] (Key s)                        = if s /= "" then select' ns (Key "") else selectWhen e ns
    select' [] _                              = selectWhen e ns

select :: Event -> [(Input, GameMachine)] -> GameMachine
select e ns = selectWhen e $ map (\(i, g) -> (i, g, True)) ns

select1 :: Event -> [(Input, GameMachine)] -> GameMachine
select1 e ns = selectWhen1 e $ map (\(i, g) -> (i, g, True)) ns

selectEsc :: Event -> [(Input, GameMachine)] -> GameMachine
selectEsc e ns = selectWhenEsc e $ map (\(i, g) -> (i, g, True)) ns

-- --------------------------------------------------------------------------
addHiragana :: String -> [String]
addHiragana s = case R2H.romajiToHiragana s of
                  Just h | s /= h -> [s, h]
                  _               -> [s]

-- --------------------------------------------------------------------------

talk :: String -> Int -> Maybe PictureInf -> GameMachine -> GameMachine
talk msg t picInf next = talkSelect msg t picInf (\ev -> events [ev] next)

-- | negative time means enable skip.
talkSelect :: String -> Int -> Maybe PictureInf -> (Event -> GameMachine) -> GameMachine
talkSelect msg t picInf lastStep = ac msgs
  where
    msgs = reverse $ reverse <$> foldr (\c acc -> (c:head acc):acc) [[]] (reverse msg)
    lstep = lastStep $ messagePic msg picInf
    ac ms = let nstep = if length ms <= 1 then lstep else ac (tail ms);
                cs = [(Key "\ESC", lstep), (Key " ", lstep), (Clock, nstep)]
            in select (messageTime t (head ms) picInf) cs


-- ==========================================================================

with :: [GameState a] -> GameMachine -> GameMachine
with gs next = GameAuto $ sequence_ gs >> run next

