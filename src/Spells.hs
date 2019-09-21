module Spells
where

import qualified Data.Map as Map
import Data.List

import Primitive

data ID = ID {
    id :: Int -- ^ identify number.
} deriving (Show, Eq, Ord)

data Kind = M | P deriving (Show, Eq)

data Define = Define {
      name       :: String    -- ^ name of spell.
    , kind       :: Kind      -- ^ kind of spell.
    , lv         :: Int       -- ^ level of spell.
    , attribute  :: Attribute -- ^ attribute of spell.
} deriving (Show, Eq)

data Attribute = None
               | Fire
               | Frost
    deriving (Show, Eq)


-- type SpellEffect = 
-- 
-- data Effect = Attack Int [StatusError]
--             | Cure   Int [StatusError]

data TargetType = Attack Int
                | AttackAll
                | Cure   Int
                | CureAll

type DB = Map.Map ID Define


-- | find ID of spell from spell's name.
findID :: DB -> String -> Maybe ID
findID db n = fmap fst $ find ((== n).name.snd) $ Map.assocs db
