module Data.Items
where

import Data.Primitive
import qualified Data.Map as Map


type Name = String

data Define = Define {
      name             :: Name
    , nameUndetermined :: Name
    , itemType         :: ItemType
    , usingEffect      :: Maybe (Effect, (Int, WhenBroken)) -- ^ effect, probablity broken after using.
    , spEffect         :: Maybe (Effect, (Int, WhenBroken)) -- ^ effect, probablity broken after sp.
    , attributes       :: [Attribute]
    , equipType        :: Maybe EquipObject
    , valueInShop      :: Int
    , enableToEquip    :: UserType
    , enableToUse      :: UserType
} deriving (Show, Eq)


data ItemType = 
      Misc
    | Equip
    deriving (Show, Eq)

data Effect = 
      EqSpell SpellID
    | Happens GameEventID
    deriving (Show, Eq)

data EquipObject =
      Weapon 
    | Shield      Int -- ^ ac
    | Helmet      Int -- ^ ac
    | Armor       Int -- ^ ac
    | Gantlett    Int -- ^ ac
    | Accessories Int -- ^ ac
    deriving (Show, Eq)

data UserType = All | Only [String] -- ^ job names
    deriving (Show, Eq)

data WhenBroken =
      Lost
    | ChangeTo ItemInf
    deriving (Show, Eq)

data Attribute =
      CantDrop
    | Cursed
    | Heal Int Bool -- ^ heal or damege HP (it's value, only when equip)
    deriving (Show, Eq)

  


        

-- | data base of items.
type DB = Map.Map ItemID Define

