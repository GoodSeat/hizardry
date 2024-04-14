module SampleScenario.Items where

import qualified Data.Items as Item
import qualified Data.Map as Map

import Data.Primitive

items :: Item.DB
items = Map.fromList [
    (ItemID 0, Item.Define {
          Item.name             = "BROKEN ITEM"
        , Item.nameUndetermined = "BROKEN ITEM?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 0
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 1, Item.Define {
          Item.name             = "DIOS POTION"
        , Item.nameUndetermined = "POTION?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.EqSpell $ SpellID 112, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 100
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 2, Item.Define {
          Item.name             = "CURSED STONE"
        , Item.nameUndetermined = "STONE?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.EqSpell $ SpellID 14, (0, Item.Lost))
        , Item.spEffect         = Nothing
        , Item.attributes       = [Item.CantDrop, Item.Heal (-2) False]
        , Item.equipType        = Nothing
        , Item.valueInShop      = 200
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 3, Item.Define {
          Item.name             = "WATER"
        , Item.nameUndetermined = "POTION?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 500
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 4, Item.Define {
          Item.name             = "HALITO POTION"
        , Item.nameUndetermined = "POTION?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.EqSpell $ SpellID 11, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 100
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 5, Item.Define {
          Item.name             = "KALKI POTION"
        , Item.nameUndetermined = "POTION?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.EqSpell $ SpellID 111, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 100
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 11, Item.Define {
          Item.name             = "SWORD OF IRON"
        , Item.nameUndetermined = "SWORD?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Weapon
                                         Item.EquipBaseAttr {
                                             Item.ac = read "0"
                                           , Item.st = read "4"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
                                         Item.WeaponAttr {
                                             Item.targetF       = [L1, L2]
                                           , Item.targetB       = []
                                           , Item.damage        = read "1d8"
                                           , Item.doubleLabels  = []
                                           , Item.attrLabels    = [EffectLabel "fire"]
                                           , Item.addStatusErrors = []
                                           , Item.atackMessages = []
                                         }
        , Item.valueInShop      = 500
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 12, Item.Define {
          Item.name             = "SHILD OF IRON"
        , Item.nameUndetermined = "SHILD?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Shield
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-2"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = [(Sleep, read "min(30,lv)")]
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 500
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 13, Item.Define {
          Item.name             = "HELMET OF IRON"
        , Item.nameUndetermined = "HELMET?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Helmet
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-2"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 300
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 14, Item.Define {
          Item.name             = "HELMET OF IRON"
        , Item.nameUndetermined = "HELMET?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Helmet
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-2"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 300
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 15, Item.Define {
          Item.name             = "HELMET OF IRON"
        , Item.nameUndetermined = "HELMET?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Helmet
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-2"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 300
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 16, Item.Define {
          Item.name             = "HELMET OF IRON"
        , Item.nameUndetermined = "HELMET?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Helmet
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-2"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 300
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 17, Item.Define {
          Item.name             = "HELMET OF IRON"
        , Item.nameUndetermined = "HELMET?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Helmet
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-2"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 300
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 103, Item.Define {
          Item.name             = "盗賊の弓"
        , Item.nameUndetermined = "弓?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Weapon
                                         Item.EquipBaseAttr {
                                             Item.ac = read "0"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
                                         Item.WeaponAttr {
                                             Item.targetF       = [L1, L2, L3, L4]
                                           , Item.targetB       = [L1, L2]
                                           , Item.damage        = read "3d8"
                                           , Item.doubleLabels  = []
                                           , Item.attrLabels    = []
                                           , Item.addStatusErrors = []
                                           , Item.atackMessages = ["aimed and shot"]
                                         }
        , Item.valueInShop      = 4000
        , Item.enableToEquip    = Item.Only ["Thief"]
        , Item.enableToUse      = Item.All
    })
    ,
    (ItemID 104, Item.Define {
          Item.name             = "火矢の弓"
        , Item.nameUndetermined = "弓?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Just $ Item.Weapon
                                         Item.EquipBaseAttr {
                                             Item.ac = read "0"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
                                         Item.WeaponAttr {
                                             Item.targetF       = [L1, L2, L3, L4]
                                           , Item.targetB       = [L1, L2]
                                           , Item.damage        = read "3d8"
                                           , Item.doubleLabels  = [EnemyLabel "beast"]
                                           , Item.attrLabels    = [EffectLabel "fire"]
                                           , Item.addStatusErrors = [(read "lv-o.lv", Dead, [EffectLabel "fire"])]
                                           , Item.atackMessages = ["aimed and shot"]
                                         }
        , Item.valueInShop      = 4000
        , Item.enableToEquip    = Item.Only ["Thief"]
        , Item.enableToUse      = Item.All
    })
    ]
