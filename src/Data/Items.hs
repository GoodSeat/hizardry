module Data.Items
where

import PreludeL
import Data.Primitive
import Data.Formula
import qualified Data.Map as Map
import qualified Data.Spells as Spell


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
    , itemLv           :: Int
    , itemInformation  :: (String, PictureInf)
} deriving (Show, Eq, Read)


data ItemType = 
      Misc
    | Equip
    deriving (Show, Eq, Read)

data Effect = 
      EqSpell SpellID
    | Happens GameEventID
    deriving (Show, Eq, Read)

data EquipObject =
      Weapon     !EquipBaseAttr
                 !WeaponAttr
    | Shield     !EquipBaseAttr
    | Helmet     !EquipBaseAttr
    | Armour     !EquipBaseAttr
    | Gauntlet   !EquipBaseAttr
    | Accessory  !EquipBaseAttr
    deriving (Show, Eq, Read)

equipBaseAttr :: EquipObject -> EquipBaseAttr
equipBaseAttr eq = case eq of Weapon     attr _ -> attr
                              Shield     attr   -> attr
                              Helmet     attr   -> attr
                              Armour     attr   -> attr
                              Gauntlet   attr   -> attr
                              Accessory  attr   -> attr
  

data UserType = All | Only [String] -- ^ job names
    deriving (Show, Eq, Read)

data WhenBroken =
      Lost
    | ChangeTo ItemInf
    deriving (Show, Eq, Read)

data Attribute =
      CantDrop
    | Cursed
    | Heal Int Bool -- ^ heal or damege HP (it's value, only when equip)
    deriving (Show, Eq, Read)

data WeaponRange =
      ToSingle
    | ToGroup
    | ToAll
    deriving (Show, Eq, Read)

data EquipBaseAttr = EquipBaseAttr {
      ac             :: !Formula                  -- ^ ac
    , st             :: !Formula                  -- ^ st
    , at             :: !Formula                  -- ^ at
    , resistLabels   :: ![EnemyLabel]             -- ^ decrease damage of fight and breath.
    , resistError    :: ![(StatusError, Formula)] -- ^ resistant probablity
    , vsEffectLabels :: ![(EffectLabel, Formula)] -- ^ resistAttributes
} deriving (Show, Eq, Read)

data WeaponAttr = WeaponAttr {
      targetF         :: ![EnemyLine]                             -- ^ enable target enemy line in front.
    , targetB         :: ![EnemyLine]                             -- ^ enable target enemy line in back.
    , damage          :: !Formula                                 -- ^ damage per hit.
    , doubleLabels    :: ![EnemyLabel]                            -- ^ double damage target.
    , attrLabels      :: ![EffectLabel]                           -- ^ attribute of atack.
    , addStatusErrors :: ![(Formula, StatusError, [EffectLabel])] -- ^ additinal effect, and it's probablity.
    , atackMessages   :: ![String]                                -- ^ message candidates when fight with this weapon. [optional]
    , targetRange     :: !WeaponRange                             -- ^ range of weapon.
} deriving (Show, Eq, Read)
        

-- | data base of items.
type DB = Map.Map ItemID Define

-- =================================================================================
-- functions.
-- ---------------------------------------------------------------------------------

isWeapon :: Define -> Bool
isWeapon def = case equipType def of
                 Just (Weapon _ _) -> True
                 _                 -> False

isShield :: Define -> Bool
isShield def = case equipType def of
                 Just (Shield _) -> True
                 _               -> False

isHelmet :: Define -> Bool
isHelmet def = case equipType def of
                 Just (Helmet _) -> True
                 _               -> False

isArmour :: Define -> Bool
isArmour def = case equipType def of
                Just (Armour _) -> True
                _               -> False

isGauntlet :: Define -> Bool
isGauntlet def = case equipType def of
                   Just (Gauntlet _) -> True
                   _                 -> False

isAccessory :: Define -> Bool
isAccessory def = case equipType def of
                    Just (Accessory _) -> True
                    _                  -> False

allEquipTypeTest :: [Define -> Bool]
allEquipTypeTest = [isWeapon, isShield, isHelmet, isArmour, isGauntlet, isAccessory]

