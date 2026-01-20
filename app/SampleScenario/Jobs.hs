module SampleScenario.Jobs where

import PreludeL
import qualified Data.Items as Item

import Data.Primitive
import Data.Formula
import qualified Data.Characters as Chara

jobs :: [Chara.Job]
jobs = [fighter, mage, priest, thief, bishop, samurai, lord, ninja]

fighter = Chara.Job {
      Chara.jobName              = "Fighter"
    , Chara.enableAlignments     = [Chara.G, Chara.N, Chara.E]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Spell, Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "agi"
    , Chara.disarmTrapAbility    = parse' "agi"
    , Chara.needParameter        = Parameter { strength = 11, iq = 0, piety = 0, vitality = 0, agility = 0, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "2d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "min(lv/5+1,10)"
    , Chara.fightHitBonus        = read "lv/3+2"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1000, 1480, 2190, 3241, 4797, 7100, 10508, 15553, 23021, 34076]
    , Chara.hpFormula            = read "(lv)d10 + lv*max(vit-15,min(-(vit=5)+vit-5,0))"
    , Chara.mpFormula            = ([], [])
    , Chara.learningSpells       = []
}

mage = Chara.Job {
      Chara.jobName              = "Mage"
    , Chara.enableAlignments     = [Chara.G, Chara.N, Chara.E]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Spell, Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "agi"
    , Chara.disarmTrapAbility    = parse' "agi"
    , Chara.needParameter        = Parameter { strength = 0, iq = 11, piety = 0, vitality = 0, agility = 0, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "1"
    , Chara.fightHitBonus        = read "lv/5"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1100, 1628, 2409, 3565, 5276, 7808, 11556, 17104, 25314, 37468]
    , Chara.hpFormula            = read "1d4"
    , Chara.mpFormula            = (replicate 7 (read "max(0, (1d(pie/5) + lv - mlv*2)/2)"), [])
    , Chara.learningSpells       = []
}

priest = Chara.Job {
      Chara.jobName              = "Priest"
    , Chara.enableAlignments     = [Chara.G, Chara.N]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Spell, Chara.Dispell (read "min(95,max(50+5*lv-10*o.lv,5))"), Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "agi"
    , Chara.disarmTrapAbility    = parse' "agi"
    , Chara.needParameter        = Parameter { strength = 0, iq = 0, piety = 11, vitality = 0, agility = 0, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d3", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "1"
    , Chara.fightHitBonus        = read "lv/3+2"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1050, 1554, 2300, 3404, 5038, 7456, 11035, 16332, 24172, 35777]
    , Chara.hpFormula            = read "(lv)d8 + lv*max(vit-15,min(-(vit=5)+vit-5,0))"
    , Chara.mpFormula            = ( replicate 7 (read "0")
                                       , read "min(9,max(1,lv*2-mlv*4+pie/6+1d3))"
                                       : replicate 6 (read "min(9,lv*2-mlv*4+pie/6+1d3)"))
    , Chara.learningSpells       = [(read "max(1,lv*2-4+1d(pie/5))", SpellID <$> [111..115])
                                       ,(read "lv*2- 8+1d(pie/5)"      , SpellID <$> [121..124])
                                       ,(read "lv*2-12+1d(pie/5)"      , SpellID <$> [131..134])
                                       ,(read "lv*2-16+1d(pie/5)"      , SpellID <$> [141..144])
                                       ,(read "lv*2-20+1d(pie/5)"      , SpellID <$> [151..156])
                                       ,(read "lv*2-24+1d(pie/5)"      , SpellID <$> [161..164])
                                       ,(read "lv*2-28+1d(pie/5)"      , SpellID <$> [171..172])
                                       ]
}

thief = Chara.Job {
      Chara.jobName              = "Thief"
    , Chara.enableAlignments     = [Chara.N, Chara.E]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Hide, Chara.Ambush, Chara.Spell, Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "min(agi*6, 95)"
    , Chara.disarmTrapAbility    = parse' "(lv-7+50)*100/70"
    , Chara.needParameter        = Parameter { strength = 0, iq = 0, piety = 0, vitality = 0, agility = 11, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d4", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "1"
    , Chara.fightHitBonus        = read "lv/5"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [900, 1332, 1971, 2917, 4318, 6390, 9458, 13998, 20718, 30664]
    , Chara.hpFormula            = read "(lv)d6 + lv*max(vit-15,min(-(vit=5)+vit-5,0))"
    , Chara.mpFormula            = ([], [])
    , Chara.learningSpells       = []
}

bishop = Chara.Job {
      Chara.jobName              = "Bishop"
    , Chara.enableAlignments     = [Chara.G, Chara.N]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Spell, Chara.EnableWhen (Chara.Dispell (read "min(95,max(50+5*lv-10*o.lv-20,5))")) (read "lv>=4"), Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "min(95, lv*2 + pie*2)"
    , Chara.disarmTrapAbility    = parse' "agi"
    , Chara.needParameter        = Parameter { strength = 0, iq = 12, piety = 12, vitality = 0, agility = 0, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d4", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "1"
    , Chara.fightHitBonus        = read "lv/5"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Just (read "max(5,min(95,50+(lv-itemLv)*5))")
    , Chara.lvupExps             = [1200, 1776, 2628, 3890, 5757, 8520, 12610, 18665, 27625, 40889]
    , Chara.hpFormula            = read "1d6"
    , Chara.mpFormula            = (replicate 7 (read "max(0, (1d(iq/5) + lv - mlv*3)/2)"), replicate 7 (read "max(0, (1d(pie/5) + lv - mlv*3)/2)"))
    , Chara.learningSpells       = []
}

samurai = Chara.Job {
      Chara.jobName              = "Samurai"
    , Chara.enableAlignments     = [Chara.G, Chara.N]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Spell, Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "agi"
    , Chara.disarmTrapAbility    = parse' "agi"
    , Chara.needParameter        = Parameter { strength = 14, iq = 10, piety = 9, vitality = 14, agility = 11, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d6", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "min(lv/5+1,10)"
    , Chara.fightHitBonus        = read "lv/3+2"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1150, 1628, 2378, 3508, 5178, 7640, 11276, 16640, 24552, 36224]
    , Chara.hpFormula            = read "1d8"
    , Chara.mpFormula            = (replicate 7 (read "max(0, (1d(iq/5) + lv - mlv*4)/2)"), [])
    , Chara.learningSpells       = []
}

lord = Chara.Job {
      Chara.jobName              = "Lord"
    , Chara.enableAlignments     = [Chara.G]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Spell, Chara.EnableWhen (Chara.Dispell (read "min(95,max(50+5*lv-10*o.lv-40,5))")) (read "lv>=9"), Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "agi"
    , Chara.disarmTrapAbility    = parse' "agi"
    , Chara.needParameter        = Parameter { strength = 15, iq = 12, piety = 12, vitality = 15, agility = 14, luck = 14 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d4", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "min(lv/5+1,10)"
    , Chara.fightHitBonus        = read "lv/3+2"
    , Chara.baseAC               = read "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1250, 1800, 2592, 3732, 5374, 7738, 11142, 16044, 23103, 33268]
    , Chara.hpFormula            = read "1d10"
    , Chara.mpFormula            = ([], replicate 7 (read "max(0, (1d(pie/5) + lv - mlv*4)/2)"))
    , Chara.learningSpells       = []
}

ninja = Chara.Job {
      Chara.jobName              = "Ninja"
    , Chara.enableAlignments     = [Chara.E]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.Hide, Chara.Ambush, Chara.Spell, Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "min(agi*4, 95)"
    , Chara.disarmTrapAbility    = parse' "(lv-7+50)*100/70"
    , Chara.needParameter        = Parameter { strength = 17, iq = 17, piety = 17, vitality = 17, agility = 17, luck = 17 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "2d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Chara.fightTryCount        = read "min(lv/5+2,10)"
    , Chara.fightHitBonus        = read "lv/3+2"
    , Chara.baseAC               = read "10-lv/3"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1400, 2072, 3066, 4538, 6717, 9940, 14711, 21773, 32225, 47692]
    , Chara.hpFormula            = read "1d6"
    , Chara.mpFormula            = ([], [])
    , Chara.learningSpells       = []
}
