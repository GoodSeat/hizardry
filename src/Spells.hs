module Spells
where

import qualified Data.Map as Map

data ID = ID {
    id :: Int -- ^ identify number.
} deriving (Show, Eq, Ord)

data Kind = M | P deriving (Show, Eq)

data Define = Define {
      name       :: String    -- ^ name of spell.
    , kind       :: Kind      -- ^ kind of spell.
    , lv         :: Integer   -- ^ level of spell.
    , attribute  :: Attribute -- ^ attribute of spell.
} deriving (Show, Eq)

data Attribute = None
               | Fire
               | Frost
    deriving (Show, Eq)


type DB = Map.Map ID Define

