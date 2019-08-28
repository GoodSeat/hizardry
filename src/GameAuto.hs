{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameAuto
where

import Control.Monad.State
import Control.Monad.Reader

import World
import Labyrinth

data Input = Key String
           | Clock
           | Abort
    deriving (Show, Eq)

data Event = None
           | Exit
           | Message String
           | And Event Event
    deriving (Show, Eq)

data Option = Option String


-- ==========================================================================
-- | scenario immutable data.
data Scenario = Scenario {
      scenarioOption :: Option
    , scenarioHome   :: GameAuto
    , labyrinths     :: [Labyrinth]
    }

-- | State used in game.
newtype GameState o = GameState { exec :: StateT World (Reader Scenario) o }
    deriving (Functor, Applicative, Monad, MonadReader Scenario, MonadState World)

-- | Automaton for running game.
newtype GameAuto = Auto { run :: GameMachine }

-- | Machine used in GameAuto.
type GameMachine = GameState (Event, Input -> GameAuto)
    

runGame :: (Event -> World -> IO a)     -- ^ renderer of game.
        -> IO Input                     -- ^ input command.
        -> Scenario                     -- ^ game scenario.
        -> (GameAuto, World)            -- ^ target GameAuto, and current environment.
        -> IO ()
runGame render cmd scenario (game, w) = do
    let ((e, next), w') = runReader (runStateT (exec $ run game) w) scenario
    if e == Exit then return ()
    else do
        render e w'
        i <- cmd
        runGame render cmd scenario (next i, w')

-- ==========================================================================

events :: [Event] -> GameAuto -> GameAuto
events [] l     = l
events (e:es) l = Auto $ return (e, \_ -> events es l)


selectWhen :: Event -> [(Input, GameAuto, Bool)] -> GameMachine
selectWhen e ns = return (e, select' ns)
  where
    select' ((i1, s1, enable):ns) i = if i == i1 && enable then s1 else select' ns i
    select' [] _ = Auto $ selectWhen e ns

select :: Event -> [(Input, GameAuto)] -> GameMachine
select e ns = selectWhen e $ map (\(i, g) -> (i, g, True)) ns 

selectNext :: Event -> [(Input, GameAuto)] -> GameAuto
selectNext e ns = Auto $ select e ns

-- ==========================================================================

world :: GameState World
world = get

option :: GameState Option
option = scenarioOption <$> ask

home :: GameState GameAuto
home = scenarioHome <$> ask

