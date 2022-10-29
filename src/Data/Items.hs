module Data.Items
where

import Data.Primitive
import qualified Data.Map as Map


data Define = Define {
      name             :: String
    , nameUndetermined :: String
    , itemType         :: ItemType
    , spEffect         :: Maybe (Effect, Int) -- ^ effect, probablity broken after sp.
} deriving (Show, Eq)


data ItemType = 
      Potion
    | Equip
    deriving (Show, Eq)

data Effect = 
      EqSpell SpellID
--  | Happens Ev.
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
type DB = Map.Map Int Define

