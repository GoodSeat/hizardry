module SampleScenario.Items where

import PreludeL
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
        , Item.itemLv           = 0
        , Item.itemInformation  = ("ガラクタ。何の役にも立たない。", Null)
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
        , Item.itemLv           = 1
        , Item.itemInformation  = ("Diosの効果を持つ薬。", Null)
    })
    ,
    (ItemID 2, Item.Define {
          Item.name             = "CURSED STONE"
        , Item.nameUndetermined = "STONE?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.EqSpell $ SpellID 14, (0, Item.Lost))
        , Item.spEffect         = Just (Item.Happens (GameEventID 000100), (10, Item.ChangeTo $ ItemInf (ItemID 0) False))
        , Item.attributes       = [Item.CantDrop, Item.Heal (-2) False]
        , Item.equipType        = Nothing
        , Item.valueInShop      = 200
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 1
        , Item.itemInformation  = ("呪われた石。", Null)
    })
    ,
    (ItemID 3, Item.Define {
          Item.name             = "WATER"
        , Item.nameUndetermined = "POTION?"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.Happens (GameEventID 000003), (0, undefined))
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 500
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 1
        , Item.itemInformation  = ("きれいな水。", List [Clip (Trans 0 (-10) (Single $ PictureID 0002)) (Single $ PictureID 0051), Single (PictureID 0051)])
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
        , Item.itemLv           = 1
        , Item.itemInformation  = ("投げつけるとHalito相当の火炎を噴出する薬瓶。", Null)
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
        , Item.itemLv           = 1
        , Item.itemInformation  = ("Kalkiの効果を噴出する薬瓶。", Null)
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
                                           , Item.targetRange   = Item.ToSingle
                                         }
        , Item.valueInShop      = 500
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord"]
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 2
        , Item.itemInformation  = ("鉄の剣。量産品。", Null)
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
        , Item.itemLv           = 2
        , Item.itemInformation  = ("鉄の盾。量産品。", Null)
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
        , Item.itemLv           = 2
        , Item.itemInformation  = ("鉄の兜。量産品。", Null)
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
        , Item.itemLv           = 2
        , Item.itemInformation  = ("鉄の兜。量産品。", Null)
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
        , Item.itemLv           = 2
        , Item.itemInformation  = ("鉄の兜。量産品。", Null)
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
        , Item.itemLv           = 2
        , Item.itemInformation  = ("鉄の兜。量産品。", Null)
    })
    ,
    (ItemID 17, Item.Define {
          Item.name             = "CURSED HELMET"
        , Item.nameUndetermined = "HELMET?"
        , Item.itemType         = Item.Equip
        , Item.usingEffect      = Nothing
        , Item.spEffect         = Nothing
        , Item.attributes       = [Item.Cursed]
        , Item.equipType        = Just $ Item.Helmet
                                         Item.EquipBaseAttr {
                                             Item.ac = read "-5"
                                           , Item.st = read "0"
                                           , Item.at = read "0"
                                           , Item.resistLabels   = []
                                           , Item.resistError    = []
                                           , Item.vsEffectLabels = []
                                         } 
        , Item.valueInShop      = 950
        , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 4
        , Item.itemInformation  = ("呪いのこもった兜。", Null)
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
                                           , Item.targetRange   = Item.ToSingle
                                         }
        , Item.valueInShop      = 4000
        , Item.enableToEquip    = Item.Only ["Thief"]
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 2
        , Item.itemInformation  = ("手先な器用な物にしか扱えない小型の弓。威力は低い。", Null)
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
                                           , Item.targetRange   = Item.ToSingle
                                         }
        , Item.valueInShop      = 4000
        , Item.enableToEquip    = Item.Only ["Thief"]
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 3
        , Item.itemInformation  = ("火矢を放つ小型の弓。", Null)
    })
    ,
    (ItemID 105, Item.Define {
          Item.name             = "聖職者の鞭"
        , Item.nameUndetermined = "紐?"
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
                                             Item.targetF       = [L1, L2]
                                           , Item.targetB       = []
                                           , Item.damage        = read "2d6"
                                           , Item.doubleLabels  = [EnemyLabel "undead"]
                                           , Item.attrLabels    = [EffectLabel "fire"]
                                           , Item.addStatusErrors = []
                                           , Item.atackMessages = ["aimed and hit"]
                                           , Item.targetRange   = Item.ToGroup
                                         }
        , Item.valueInShop      = 2000
        , Item.enableToEquip    = Item.Only ["Priest"]
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 3
        , Item.itemInformation  = ("聖職者のみが扱う事を許された神聖な鞭。", Null)
    })
    ,
    (ItemID 106, Item.Define {
          Item.name             = "盗賊のブーメラン"
        , Item.nameUndetermined = "曲がったもの?"
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
                                             Item.targetF       = [L1]
                                           , Item.targetB       = [L1]
                                           , Item.damage        = read "2d3"
                                           , Item.doubleLabels  = []
                                           , Item.attrLabels    = []
                                           , Item.addStatusErrors = []
                                           , Item.atackMessages = ["throw weqpon to"]
                                           , Item.targetRange   = Item.ToAll
                                         }
        , Item.valueInShop      = 3000
        , Item.enableToEquip    = Item.Only ["Thief"]
        , Item.enableToUse      = Item.All
        , Item.itemLv           = 3
        , Item.itemInformation  = ("手先な器用な物にしか扱えない小型のブーメラン。威力は低い。", Null)
    })
    ,
    (ItemID 107, Item.Define {
          Item.name             = "コンパクト・ダイナマイト"
        , Item.nameUndetermined = "?筒状のもの"
        , Item.itemType         = Item.Misc
        , Item.usingEffect      = Just (Item.EqSpell $ SpellID 11, (100, Item.Lost))
        , Item.spEffect         = Nothing
        , Item.attributes       = []
        , Item.equipType        = Nothing
        , Item.valueInShop      = 50
        , Item.enableToEquip    = Item.All
        , Item.enableToUse      = Item.Only ["Thief"]
        , Item.itemLv           = 2
        , Item.itemInformation  = ("手先な器用な物にしか扱えない小型の爆弾。威力は低い。", Null)
    })
    ]
