module Data.Spells
where

import qualified Data.Map as Map
import Data.List

import Data.Primitive
import Data.Formula

data ID = ID {
    id :: Int -- ^ identify number.
} deriving (Show, Eq, Ord)

data Kind = M | P deriving (Show, Eq)

data Define = Define {
      name       :: String     -- ^ name of spell.
    , kind       :: Kind       -- ^ kind of spell.
    , lv         :: Int        -- ^ level of spell.
    , attribute  :: Attribute  -- ^ attribute of spell.
    , target     :: TargetType -- ^ type of target.
    , effect     :: Effect     -- ^ type of effect.
} deriving (Show)

data Attribute = None
               | Fire
               | Frost
    deriving (Show, Eq)


-- type SpellEffect = 
-- 
data Effect = Damage Formula
            | Cure   Formula [StatusError]
    deriving (Show)

data TargetType = OpponentSingle
                | OpponentGroup
                | OpponentAll
                | AllySingle
                | AllyGroup
                | AllyAll
    deriving (Show, Eq)

type DB = Map.Map ID Define


-- | find ID of spell from spell's name.
findID :: DB -> String -> Maybe ID
findID db n = fmap fst $ find ((== n).name.snd) $ Map.assocs db
