{-# LANGUAGE TupleSections #-}
module Engine.GameAuto
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map

import Data.Primitive
import Data.World
import Data.Maze
import Data.Characters
import qualified Data.GameEvent as GameEvent
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item
import qualified Data.RomajiToHiragana as R2H

-- ==========================================================================

data Input = Key String
           | AnyKey
           | Clock
           | Abort
    deriving (Show, Eq, Read)

data InputType = SingleKey
               | SequenceKey
               | WaitClock Int -- ^ time to wait(ms), negative wait time means enable to skip by key input
    deriving (Show, Eq)

data Event = None
           | Exit
           | General       Display
           | ShowStatus    CharacterID String InputType -- ^ target character ID, manu message, next input type.
           | ShowMap       String (Int, Int)            -- ^ message, translete
    deriving (Show, Eq)

data Display = Display {
      messageBox :: !(Maybe String)
    , commandBox :: !(Maybe String)
    , flashBox   :: !(Maybe String)
    , waitTime   :: !(Maybe Int)       -- ^ negative wait time means enable to skip by key input
    , picture    :: !(Maybe PictureInf)
    , needPhrase :: Bool
} deriving (Show, Eq)

message s = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    }
messagePic s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = False
    }
ask s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = True
    }
messageTime t s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    }
wait t p = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    }
flashMessage t s = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Just s
    , waitTime   = Just t
    , picture    = Nothing
    , needPhrase = False
    }
battleCommand s = General $ Display {
      messageBox = Nothing
    , commandBox = Just s
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    }
spellCommand s = General $ Display {
      messageBox = Nothing
    , commandBox = Just s
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = True
    }



data ScenarioOption = ScenarioOption {
      enableEffectDumapic :: [Spell.CheckLocationType]
    , enableMinimapType   :: [MiniMapType]
    }
      

-- | scenario immutable data.
data Scenario = Scenario {
      scenarioOption :: !ScenarioOption
    , scenarioHome   :: !GameMachine
    , racies         :: ![Race]
    , jobs           :: ![Job]
    , mazes          :: Int -> GameState (String, Size2D, Maze)
    , encountMap     :: !(Map.Map Coord (Int, [EnemyID]))
    , roomBattleMap  :: !(Map.Map Coord (Int, [EnemyID]))
    , roomDefine     :: ![[Coord]]
    , eventMap       :: !(Map.Map Coord GameEventID)
    , eventMapDir    :: !(Map.Map Position GameEventID)
    , eventInspect   :: !(Map.Map Position GameEventID)
    , mazeEvents     :: !GameEvent.DB
    , enemies        :: !Enemy.DB
    , spells         :: !Spell.DB
    , items          :: !Item.DB
    }
data InitScenario = InitScenario {
      initScenarioOption :: !ScenarioOption
    , initRacies         :: ![Race]
    , initJobs           :: ![Job]
    , initMazes          :: ![(String, Size2D, Maze)]
    , initEncountMap     :: !(Map.Map Coord (Int, [EnemyID]))
    , initRoomBattleMap  :: !(Map.Map Coord (Int, [EnemyID]))
    , initRoomDefine     :: ![[Coord]]
    , initEventMap       :: !(Map.Map Coord GameEventID)
    , initEventMapDir    :: !(Map.Map Position GameEventID)
    , initEventInspect   :: !(Map.Map Position GameEventID)
    , initMazeEvents     :: !GameEvent.DB
    , initEnemies        :: !Enemy.DB
    , initSpells         :: !Spell.DB
    , initItems          :: !Item.DB
    }

initScenario :: InitScenario -> GameMachine -> Scenario
initScenario i home = Scenario {
      scenarioOption            = initScenarioOption i
    , scenarioHome              = home
    , racies                    = initRacies         i
    , jobs                      = initJobs           i
    , mazes                     = \z -> pure $ initMazes i !! z 
    , encountMap                = initEncountMap     i
    , roomBattleMap             = initRoomBattleMap  i
    , roomDefine                = initRoomDefine     i
    , eventMap                  = initEventMap       i
    , eventMapDir               = initEventMapDir    i
    , eventInspect              = initEventInspect   i
    , mazeEvents                = initMazeEvents     i
    , enemies                   = initEnemies        i
    , Engine.GameAuto.spells    = initSpells         i
    , Engine.GameAuto.items     = initItems          i
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
      
-- | GameMachine by Automaton.
type GameMachine = GameAuto Input Event

-- ==========================================================================
--

runGame :: (Event -> World -> IO a) -- ^ renderer of game.
        -> (InputType -> IO Input)   -- ^ input command.
        -> Scenario                  -- ^ game scenario.
        -> World                     -- ^ current environment.
        -> GameMachine               -- ^ target GameMachine.
        -> IO World
runGame render cmd s w g = do
    let (e, w', itype, next') = stepGame s w g
    if e == Exit then return w'
    else do
      render e w'
      i <- cmd itype
      runGame render cmd s w' (next' i)


loadGame :: [Input]
         -> (Event -> World -> IO a) -- ^ renderer of game.
         -> (InputType -> IO Input)   -- ^ input command.
         -> Scenario                  -- ^ game scenario.
         -> World                     -- ^ initial environment.
         -> GameMachine               -- ^ initial GameMachine.
         -> IO World
loadGame [] render cmd s w g = runGame render cmd s w g
loadGame (i:is) render cmd s w g = do
    let (e, w', _, next') = stepGame s w g
    loadGame is render cmd s w' (next' i)


stepGame :: Scenario
         -> World
         -> GameMachine
         -> (Event, World, InputType, Input -> GameMachine)
stepGame s w g = 
    let (res, w') = runGameState s w $ run g
    in case res of
      Left msg         -> let end = GameAuto $ return (Exit, const g)
                          in (message msg, w', SingleKey, const end)
      Right (e, next') -> let itype = case e of
                                        General (Display _ _ _ w _ n)
                                          | isJust w            -> WaitClock $ fromJust w 
                                          | n                   -> SequenceKey
                                          | otherwise           -> SingleKey
                                        ShowStatus _ _ i'       -> i'
                                        _                       -> SingleKey
                          in (e, w', itype, next')

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

