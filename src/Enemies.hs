{-# LANGUAGE FlexibleInstances #-}
module Enemies
where

import qualified Data.Map as Map

import Formula
import Primitive
import qualified Characters as Character
import qualified Spells as Spell
import qualified Items as Item

newtype ID = ID {
    num :: Int
} deriving (Show, Eq, Ord)

data Instance = Instance {
      id            :: !ID
    , determined    :: !Bool
    , hp            :: !Int
    , maxhp         :: !Int
    , statusErrors  :: ![StatusError]
    , maybeDropItem :: !Bool
    , modAc         :: !Int
} deriving (Show, Eq)

data Define = Define {
      name              :: !String
    , nameUndetermined  :: !String
    , lv                :: !Int
    , hpFormula         :: !Formula

    , param             :: !Parameter
    , ac                :: !Int

    , exp               :: !Int
    , kind              :: !String
    , frendlyProb       :: !Int
    , numOfOccurrences  :: !Formula
    , resistProbM       :: !Int
    , resistProbP       :: !Int
    , healPerTurn       :: !Int
    , moveFrontProb     :: !Int

    , resistError       :: ![(StatusError, Int)]
    , resistAttributes  :: ![Spell.Attribute]
    , weakAttributes    :: ![Spell.Attribute]

    , actions           :: ![Action]

    , dropItem          :: ![(Int, Formula)] -- ^ drop probablity, and it's item ID.
    , dropGold          :: !Formula

    , withBackProb      :: !Int
    , backEnemyID       :: !Formula

    , enableRun         :: !Bool
    , trapCandidate     :: ![Trap]

} deriving (Show)


instance Object (Instance, Define) where
  hpOf            = hp . fst
  maxhpOf         = maxhp . fst
  paramOf         = param . snd
  acOf (e, def)   = ac def + modAc e
  lvOf            = lv . snd
  statusErrorsOf  = statusErrors . fst

  setHp           v (e, def) = (e { hp = v }, def)
  setAc           v (e, def) = let mod = v - acOf (e, def)
                               in (e { modAc = modAc e + mod }, def)
  setStatusErrors v (e, def) = (e { statusErrors = v }, def)


data Action = Fight Int     -- ^ count of attack.
                    Formula -- ^ damage per hit.
                    Formula -- ^ target number. 1~3 are front member, 4~6 are back member.
                    [(Int, StatusError)] -- ^ additinal effect, and it's probablity.
            | Spelling Formula -- ^ spel id.
            | Breath Formula   -- ^ damage.
            | Run
    deriving (Show)

data Trap = DropDirectly
          | NoTrap
          | PoisonNeedle
          | GasBomb
          | CrossbowBolt
          | ExplodingBox
          | Stunner
          | Teleporter
          | MageBlaster
          | PriestBlaster
          | Alarm
    deriving (Show, Eq, Enum)

-- | data base of enemies.
type DB = Map.Map ID Define

