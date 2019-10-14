{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameAuto
where

import System.Random
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import World
import Maze
import qualified Enemies as Enemy
import qualified Spells as Spell

data Input = Key String
           | Clock
           | Abort
    deriving (Show, Eq)

data InputType = SingleKey | SequenceKey

data Event = None
           | Exit
           | Message String
           | BattleCommand String
           | SpellCommand String
           | And Event Event
    deriving (Show, Eq)

data Option = Option String


-- ==========================================================================
-- | scenario immutable data.
data Scenario = Scenario {
      scenarioOption :: !Option
    , scenarioHome   :: !GameAuto
    , mazes          :: ![Maze]
    , encountMap     ::  Map.Map Coord (Int, [Enemy.ID])
    , enemies        :: !Enemy.DB
    , spells         :: !Spell.DB
    }

-- | State used in game.
newtype GameState o = GameState { exec :: ExceptT String (StateT World (Reader Scenario)) o }
    deriving (Functor, Applicative, Monad, MonadReader Scenario, MonadState World, MonadError String)

-- | Automaton for running game.
newtype GameAuto = Auto { run :: GameMachine }

-- | Machine used in GameAuto.
type GameMachine = GameState (Event, Input -> GameAuto)
    

runGame :: (Event -> World -> IO a)     -- ^ renderer of game.
        -> (InputType -> IO Input)      -- ^ input command.
        -> Scenario                     -- ^ game scenario.
        -> (GameAuto, World)            -- ^ target GameAuto, and current environment.
        -> IO String
runGame render cmd scenario (game, w) = do
    let (res, w') = runReader (runStateT (runExceptT $ exec $ run game) w) scenario
    case res of Left msg        -> return msg
                Right (e, next) -> if e == Exit then return "thank you for playing."
                                   else do
                                       render e w'
                                       let itype = case e of SpellCommand _ -> SequenceKey
                                                             _              -> SingleKey
                                       i <- cmd itype
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
option = asks scenarioOption

home :: GameState GameAuto
home = asks scenarioHome

mazeAt :: Int -> GameState Maze
mazeAt z = do
   ls <- mazes <$> ask
   return $ ls !! z

checkEncount :: Coord -> GameState (Maybe Enemy.ID)
checkEncount c = do
    emap <- encountMap <$> ask
    r    <- randomNext 1 100
    let es' = do {
        (prob, es) <- Map.lookup c emap;
        guard $ r < prob;
        return es
        }
    case es' of Nothing -> return $ Nothing
                Just es -> Just <$> randomIn es

happens :: Int -> GameState Bool
happens prob = do
    r <- randomNext 1 100
    return $ prob >= r

err :: String -> GameState a
err msg = throwError msg

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
    
    

