module Maze
where

data Direction = N | S | E | W deriving (Show, Eq)

data Position = Position {
      direction :: Direction 
    , floor     :: Integer
    , x         :: Integer
    , y         :: Integer
    } deriving (Show, Eq)

