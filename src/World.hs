module World
where

import System.Random
import qualified Data.Map as Map

import qualified Characters as Character
import qualified Items as Item
import Labyrinth

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
            | InLabyrinth Position
    deriving (Show, Eq)


