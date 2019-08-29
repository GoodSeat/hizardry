module World
where

import System.Random
import qualified Data.Map as Map

import qualified Characters as Character
import qualified Items as Item
import Maze

data World = World {
      randomGen       :: StdGen

    , party           :: [Character.ID]
    , place           :: Place

    , inTarvernMember :: [Character.ID]
    , inMazeMember    :: [(Character.ID, Position)]
    , shopItems       :: Map.Map Item.ID Int

    , allCharacters   :: Character.DB
} deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern 
            | Adventure'sInn
            | Boltac'sTradingPost 
            | TempleOfCant 
            | InEdgeOfTown
            | TrainingGrounds
            | InMaze Position
    deriving (Show, Eq)


