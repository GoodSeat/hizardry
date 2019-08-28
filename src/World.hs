module World
where

import System.Random
import qualified Data.Map as Map

import Characters
import Items
import Labyrinth

data World = World {
      randomGen       :: StdGen

    , party           :: [Character]
    , place           :: Place

    , inTarvernMember :: [Character]
    , inMazeMember    :: [(Character, Position)]
    , shopItems       :: Map.Map Item Int
} deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern 
            | Adventure'sInn
            | Boltac'sTradingPost 
            | TempleOfCant 
            | InEdgeOfTown
            | TrainingGrounds
            | InLabyrinth Position
    deriving (Show, Eq)


