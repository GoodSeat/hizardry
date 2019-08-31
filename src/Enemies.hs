module Enemies
where



data Instance = Instance {
      id :: !Int
    , hp :: !Int
} deriving (Show, Eq)

data Define = Define {
      name              :: !String
    , nameUndetermined  :: !String
    , lv                :: !Int
    , maxhp             :: !Int

} deriving (Show, Eq)


