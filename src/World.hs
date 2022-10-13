module World
where

import System.Random
import qualified Data.Map as Map

import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Items as Item
import Maze

import Cui

data World = World {
      randomGen       :: StdGen
    , guideOn         :: !Bool   -- ^ visible guidw window in maze.
    , statusOn        :: !Bool   -- ^ visible status window in maze.

    , party           :: ![Character.ID]
    , place           :: !Place   -- ^ current party position.
    , roomBattled     :: ![Coord] -- ^ already room battled in current mazing.

    , inTarvernMember :: ![Character.ID]
    , inMazeMember    :: ![(Character.ID, Position)]
    , shopItems       :: !(Map.Map Item.ID Int)

    , allCharacters   :: !Character.DB

    , sceneTrans      :: Filter
} -- deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern 
            | Adventure'sInn
            | Boltac'sTradingPost 
            | TempleOfCant 
            | InEdgeOfTown
            | TrainingGrounds
            | InMaze Position
            | InBattle Position [[Enemy.Instance]]
            | Camping Position
    deriving (Show, Eq)


