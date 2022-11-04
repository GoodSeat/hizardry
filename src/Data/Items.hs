module Data.Items
where

import Data.Primitive
import qualified Data.Map as Map


type Name = String

data Define = Define {
      name             :: Name
    , nameUndetermined :: Name
    , itemType         :: ItemType
    , usingEffect      :: Maybe (Effect, Int) -- ^ effect, probablity broken after using.
    , spEffect         :: Maybe (Effect, Int) -- ^ effect, probablity broken after sp.
} deriving (Show, Eq)


data ItemType = 
      Misc
    | Equip
    deriving (Show, Eq)

data Effect = 
      EqSpell SpellID
    | Happens GameEventID
    deriving (Show, Eq)

data EquipType =
      Weapon
    | Shield
    | Helmet
    | Armor
    | Gantlett
    | Accessories
    deriving (Show, Eq)


class Equipable a where
  canEquip :: a -> String -> Bool  -- ^ jobname
  effectAC :: a -> Int
  


        

-- | data base of items.
type DB = Map.Map ItemID Define

