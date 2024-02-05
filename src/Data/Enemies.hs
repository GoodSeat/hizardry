{-# LANGUAGE FlexibleInstances #-}
module Data.Enemies
where

import Data.List (nub)
import qualified Data.Map as Map

import Data.Formula
import Data.Primitive
import qualified Data.Characters as Character
import qualified Data.Spells as Spell
import qualified Data.Items as Item

data Instance = Instance {
      id            :: !EnemyID -- ^ define id.
    , define        :: !Define  -- ^ define.
    , noID          :: !Int     -- ^ number for identiry inscance.
    , determined    :: !Bool
    , hp            :: !Int
    , maxhp         :: !Int
    , statusErrors  :: ![StatusError]
    , maybeDropItem :: !Bool
    , modParam      :: !ParamChange
} deriving (Show, Eq)

data Define = Define {
      name              :: !String
    , nameUndetermined  :: !String
    , pic               :: !PictureID
    , picUndetermined   :: !PictureID
    , lv                :: !Int
    , hpFormula         :: !Formula

    , param             :: !Parameter
    , ac                :: !Int

    , exp               :: !Int
    , kind              :: !String
    , friendlyProb      :: !Int
    , numOfOccurrences  :: !Formula
    , resistProbM       :: !Int
    , resistProbP       :: !Int
    , healPerTurn       :: !Int
    , moveFrontProb     :: !Int

    , resistError       :: ![(StatusError, Int)]
    , resistAttributes  :: ![Spell.Attribute]
    , weakAttributes    :: ![Spell.Attribute]
    , attrLabels        :: ![String]

    , actions           :: ![Action]

    , dropItem          :: ![(Int, Formula)] -- ^ drop probablity, and it's item ID.
    , dropGold          :: !Formula

    , withBackProb      :: !Int
    , backEnemyID       :: !Formula

    , enableRun         :: !Bool
    , trapCandidate     :: ![Trap]

} deriving (Show, Eq)


instance Object Instance where
  nameOf            = name . define
  hpOf              = hp
  maxhpOf           = maxhp
  lvOf              = lv . define
  statusErrorsOf    = statusErrors

  setHp           v e = let e' = e { hp = max 0 v } in
                        if hp e' == 0 then addStatusError Dead e' else e'
  setStatusErrors v e = let e' = e { statusErrors = nub v }
                            ss = statusErrorsOf e' in
     if      Lost `elem` ss && length ss > 1 then setStatusErrors [Lost] e'
     else if Ash  `elem` ss && length ss > 1 then setStatusErrors [Ash]  e'
     else if Dead `elem` ss && length ss > 1 then setStatusErrors [Dead] e'
     else                                         e'


data Action = Fight Int     -- ^ count of attack.
                    Formula -- ^ damage per hit.
                    Formula -- ^ target number. 1~3 are front member, 4~6 are back member.
                    [(Int, StatusError)] -- ^ additinal effect, and it's probablity.
            | Spelling Formula -- ^ spel id.
            | Breath Formula   -- ^ damage.
            | Run
    deriving (Show, Eq)

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
type DB = Map.Map EnemyID Define

