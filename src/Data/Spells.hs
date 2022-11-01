module Data.Spells
where

import qualified Data.Map as Map
import Data.List

import Data.Primitive
import Data.Formula

data Kind = M | P deriving (Show, Eq)

data Define = Define {
      name       :: String      -- ^ name of spell.
    , kind       :: Kind        -- ^ kind of spell.
    , lv         :: Int         -- ^ level of spell.
    , attribute  :: Attribute   -- ^ attribute of spell.
    , target     :: TargetType  -- ^ type of target.
    , effect     :: Effect      -- ^ type of effect.
    , enableIn   :: [CastPlace] -- ^ place enable to cast.
} deriving (Show)

data Attribute = None
               | Fire
               | Frost
    deriving (Show, Eq)

data CastPlace = InCamp
               | InCastle
               | InBattle
    deriving (Show, Eq)


-- type SpellEffect = 
-- 
data Effect = Damage   Formula
            | Cure     Formula [StatusError]
            | ChangeAC Formula
            | Kill     Formula String -- ^ probability (0~100), message when kill(exp:"is dead").
            | Event    GameEventID
    deriving (Show)

data TargetType = OpponentSingle
                | OpponentGroup
                | OpponentAll
                | AllySingle
                | AllyGroup
                | AllyAll
    deriving (Show, Eq)

type DB = Map.Map SpellID Define


-- | find ID of spell from spell's name.
findID :: DB -> String -> Maybe SpellID
findID db n = fmap fst $ find ((== n).name.snd) $ Map.assocs db


defToID :: DB -> Define -> SpellID
defToID db def = case findID db (name def) of Just id -> id
                                              Nothing -> undefined


