module GameAuto
where

import System.Random
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import World
import Maze
import MazeEvent
import qualified Enemies as Enemy
import qualified Spells as Spell

-- ==========================================================================

data Input = Key String
           | Clock
           | Abort
    deriving (Show, Eq)

data InputType = SingleKey
               | SequenceKey
               | WaitClock Int -- ^ time to wait(ms), negative wait time means enable to skip by key input

data Event = None
           | Exit
           | Ask           String
           | Message       String
           | MessageTime   Int String -- ^ negative wait time means enable to skip by key input
           | Time          Int        -- ^ negative wait time means enable to skip by key input
           | BattleCommand String
           | SpellCommand  String
           | ShowStatus    Int String -- ^ order of member, manu message.
    deriving (Show, Eq)

data Option = Option String

-- | scenario immutable data.
data Scenario = Scenario {
      scenarioOption :: !Option
    , scenarioHome   :: !GameMachine
    , mazes          :: ![Maze]
    , encountMap     ::  Map.Map Coord (Int, [Enemy.ID])
    , eventMap       ::  Map.Map Coord MazeEvent.ID
    , mazeEvents     :: !MazeEvent.DB
    , enemies        :: !Enemy.DB
    , spells         :: !Spell.DB
    }

-- ==========================================================================

-- | State used in game.
type GameState o = ExceptT String (StateT World (Reader Scenario)) o

-- | Automaton used in Game.
newtype GameAuto i o = GameAuto { run :: GameState (o, i -> GameAuto i o) }

instance Functor (GameAuto i) where
  fmap f (GameAuto s) = GameAuto $ do
    (o, next) <- s
    return (f o, fmap f . next)
      
-- | GameMachine by Automaton.
type GameMachine = GameAuto Input Event

-- ==========================================================================

runGame :: (Event -> World -> IO a)     -- ^ renderer of game.
        -> (InputType -> IO Input)      -- ^ input command.
        -> Scenario                     -- ^ game scenario.
        -> (GameMachine, World)            -- ^ target GameMachine, and current environment.
        -> IO String
runGame render cmd scenario (game, w) = do
    let (res, w') = runReader (runStateT (runExceptT $ run game) w) scenario
    case res of Left msg        -> return msg
                Right (e, next) -> if e == Exit then return "thank you for playing."
                                   else do
                                       render e w'
                                       let itype = case e of SpellCommand _  -> SequenceKey
                                                             MessageTime n _ -> WaitClock n
                                                             Time n          -> WaitClock n
                                                             GameAuto.Ask _  -> SequenceKey
                                                             _               -> SingleKey
                                       i <- cmd itype
                                       runGame render cmd scenario (next i, w')

-- ==========================================================================

events :: [Event] -> GameMachine -> GameMachine
events es = events' $ zip (repeat $ return ()) es

events' :: [(GameState a, Event)] -> GameMachine -> GameMachine
events' []           next = next
events' ((gs, e):es) next = GameAuto $ gs >> return (e, const $ events' es next)


selectWhen :: Event -> [(Input, GameMachine, Bool)] -> GameMachine
selectWhen e ns = GameAuto $ return (e, select' ns)
  where
    select' ((i1, s1, enable):ns) i = if i == i1 && enable then s1 else select' ns i
    select' [] (Key s) = if s /= "" then select' ns (Key "")
                                    else selectWhen e ns
    select' [] _ = selectWhen e ns

select :: Event -> [(Input, GameMachine)] -> GameMachine
select e ns = selectWhen e $ map (\(i, g) -> (i, g, True)) ns

-- ==========================================================================

world :: GameState World
world = get

option :: GameState Option
option = asks scenarioOption

home :: GameState GameMachine
home = asks scenarioHome

mazeAt :: Int -> GameState Maze
mazeAt z = do
   ls <- asks mazes
   return $ ls !! z

checkEncount :: Coord -> GameState (Maybe Enemy.ID)
checkEncount c = do
    emap <- asks encountMap
    r    <- randomNext 1 100
    let es' = do {
        (prob, es) <- Map.lookup c emap;
        guard $ r < prob;
        return es
        }
    case es' of Nothing -> return Nothing
                Just es -> Just <$> randomIn es

happens :: Int -> GameState Bool
happens prob = do
    r <- randomNext 1 100
    return $ prob >= r

err :: String -> GameState a
err = throwError

-- =================================================================================

randomNext :: Int -> Int -> GameState Int
randomNext min max = do
    w <- world
    let (v, g') = randomR (min, max) $ randomGen w
    put w { randomGen = g' }
    return v

randomIn :: [a] -> GameState a
randomIn as = do
    n <- randomNext 1 $ length as
    return $ as !! (n - 1)

-- =================================================================================
--



