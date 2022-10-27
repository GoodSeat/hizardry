module Data.MazeEvent
where

import qualified Data.Map as Map
import Data.Maze
import Data.Primitive

newtype ID = ID {
    num :: Int
} deriving (Show, Eq, Ord)

data Define = ReturnCastle
            | MoveTo Coord
            | StairsToUpper Coord
            | StairsToLower Coord
            | Message String (Maybe PictureID)
            | Select String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.
            | Ask String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.
            | Reference ID
            | End    -- ^ if there is another event, it start.
            | Escape -- ^ end event with ignore event on there.
            | Events [Define]

type DB = Map.Map ID Define

