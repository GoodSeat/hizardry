{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameAuto
where

import Control.Monad.State
import Control.Monad.Reader

import World

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

-- | State used in game.
newtype GameState o = GameState { exec :: StateT World (Reader Option) o }
    deriving (Functor, Applicative, Monad, MonadReader Option, MonadState World)

-- | Automaton for running game.
newtype GameAuto = Auto { run :: GameMachine }

-- | Machine used in GameAuto.
type GameMachine = GameState (Event, Input -> GameAuto)
    

runGame :: (Event -> World -> IO a) -> IO Input -> (GameAuto, World, Option) -> IO ()
runGame render cmd (game, w, o) = do
    let r = runStateT (exec $ run game) w
    let ((e, next), w') = runReader r o
    if e == Exit then return ()
    else do
        render e w'
        i <- cmd
        runGame render cmd (next i, w', o)

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

