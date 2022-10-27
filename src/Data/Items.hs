module Data.Items
where

import qualified Data.Map as Map

data ID = ID {
      id         :: Int  -- ^ identify number.
    , determined :: Bool -- ^ already determined or not.
} deriving (Show, Eq, Ord)

data Define = Define {
      name             :: String
    , nameUndetermined :: String
    , efficacy         :: Effect
} deriving (Show, Eq)

data Effect =
      Weapon
    | Shield
    | Helmet
    | Armor
    | Gantlett
    | Accessories
    | Potion
    deriving (Show, Eq)
        

-- | data base of items.
type DB = Map.Map Int Define

