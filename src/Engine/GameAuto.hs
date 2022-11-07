module Engine.GameAuto
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import Data.Primitive
import Data.World
import Data.Maze
import qualified Data.GameEvent as GameEvent
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item

-- ==========================================================================

data Input = Key String
           | Clock
           | Abort
    deriving (Show, Eq)

data InputType = SingleKey
               | SequenceKey
               | WaitClock Int -- ^ time to wait(ms), negative wait time means enable to skip by key input
    deriving (Show, Eq)

data Event = None
           | Exit

           | Message       String
           | MessagePic    String (Maybe PictureID)
           | Ask           String (Maybe PictureID)
           | MessageTime   Int String (Maybe PictureID) -- ^ negative wait time means enable to skip by key input
           | Time          Int        (Maybe PictureID) -- ^ negative wait time means enable to skip by key input

           | BattleCommand String
           | SpellCommand  String
           | ShowStatus    PartyPos String InputType -- ^ target position in party, manu message, next input type.
    deriving (Show, Eq)

data Option = Option String

-- | scenario immutable data.
data Scenario = Scenario {
      scenarioOption :: !Option
    , scenarioHome   :: !GameMachine
    , mazes          :: ![Maze]
    , encountMap     ::  Map.Map Coord (Int, [EnemyID])
    , eventMap       ::  Map.Map Coord GameEventID
    , mazeEvents     :: !GameEvent.DB
    , enemies        :: !Enemy.DB
    , spells         :: !Spell.DB
    , items          :: !Item.DB
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

runGame :: (Event -> World -> IO a)  -- ^ renderer of game.
        -> (InputType -> IO Input)   -- ^ input command.
        -> Scenario                  -- ^ game scenario.
        -> (GameMachine, World)      -- ^ target GameMachine, and current environment.
        -> IO String
runGame render cmd scenario (game, w) = do
    let (res, w') = runReader (runStateT (runExceptT $ run game) w) scenario
    case res of
      Left msg        -> return msg
      Right (e, next) -> if e == Exit then return "thank you for playing."
                         else do
                           render e w'
                           let itype = case e of
                                 SpellCommand _          -> SequenceKey
                                 ShowStatus _ _ i        -> i
                                 MessageTime n _ _       -> WaitClock n 
                                 Time n _                -> WaitClock n
                                 Engine.GameAuto.Ask _ _ -> SequenceKey
                                 _                       -> SingleKey
                           i <- cmd itype
                           runGame render cmd scenario (next i, w')

-- TODO!: playIO of gloss is
-- playIO  :: Display
--         -> Color
--         -> Int
--         -> world
--         -> (world -> IO Picture)
--         -> (Event -> world -> IO world)
--         -> (Float -> world -> IO world)
--         -> IO ()

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

with :: [GameState a] -> GameMachine -> GameMachine
with gs next = GameAuto $ sequence_ gs >> run next

