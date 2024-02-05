module Data.Spells
where

import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromMaybe)

import Data.Primitive
import Data.Formula


type Name = String

data Kind = M | P deriving (Show, Eq, Read)

data Define = Define {
      name       :: Name        -- ^ name of spell.
    , kind       :: Kind        -- ^ kind of spell.
    , lv         :: Int         -- ^ level of spell.
    , attribute  :: Attribute   -- ^ attribute of spell.
    , target     :: TargetType  -- ^ type of target.
    , effect     :: Effect      -- ^ type of effect.
    , enableIn   :: [CastPlace] -- ^ place enable to cast.
} deriving (Show, Eq, Read)

data Attribute = None
               | Fire
               | Frost
    deriving (Show, Eq, Read)

data CastPlace = InCamp
               | InCastle
               | InBattle
    deriving (Show, Eq, Read)


-- type SpellEffect = 
-- 
data Effect = Damage        Formula
            | Cure          Formula [StatusError]
            | ChangeParam   AdParam Term String -- ^ [optional]message when apply(exp:"is protected.").
            | Kill          Formula String -- ^ probability (0~100), message when kill(exp:"is dead").
            | AddLight      Int Bool -- ^ time, super light or not.
            | CheckLocation CheckLocationType
            | Event         GameEventID
    deriving (Show, Eq, Read)

data TargetType = OpponentSingle
                | OpponentGroup
                | OpponentAll
                | AllySingle
                | AllyGroup
                | AllyAll
                | Party
    deriving (Show, Eq, Read)

type DB = Map.Map SpellID Define

data CheckLocationType = OnlyCoord | ViewMap deriving (Show, Eq, Read)


-- | find ID of spell from spell's name.
findID :: DB -> String -> Maybe SpellID
findID db n = fmap fst $ find ((== n).name.snd) $ Map.assocs db


defToID :: DB -> Define -> SpellID
defToID db def = fromMaybe undefined (findID db (name def))



applyChangeParam :: Term -> ParamChange -> [(Term, ParamChange)] -> [(Term, ParamChange)]
applyChangeParam t ad org
  | effectName ad == "" = (t, ad) : org
  | otherwise           = (t, ad) : filter ((/= effectName ad) . effectName . snd) org
