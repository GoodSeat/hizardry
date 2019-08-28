module World
where

import System.Random
import qualified Data.Map as Map

import Characters
import Maze
import Items

data World = World {
      randomGen       :: StdGen

    , party           :: [Character]
    , place           :: Place

    , inTarvernMember :: [Character]
    , inMazeMember    :: [(Character, Position)]
    , shopItems       :: Map.Map Item Int
} deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern (Maybe Character)
            | Adventure'sInn
            | Boltac'sTradingPost (Maybe Character)
            | TempleOfCant (Maybe Character)
            | EdgeOfTown
            | TrainingGrounds
            | InMaze Position
    deriving (Show, Eq)


