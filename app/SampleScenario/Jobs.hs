module SampleScenario.Jobs where

import PreludeL
import qualified Data.Items as Item

import Data.Primitive
import Data.Formula
import qualified Data.Characters as Character

jobs :: [Character.Job]
jobs = [fighter, mage, priest, thief, bishop, samurai, lord, ninja]

fighter = Character.Job {
      Character.jobName              = "Fighter"
    , Character.enableAlignments     = [Character.G, Character.N, Character.E]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Spell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "agi"
    , Character.disarmTrapAbility    = parse' "agi"
    , Character.needParameter        = Parameter { strength = 11, iq = 0, piety = 0, vitality = 0, agility = 0, luck = 0 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "2d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "min(lv/5+1,10)"
    , Character.fightHitBonus        = read "lv/3+2"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [1000, 1480, 2190, 3241, 4797, 7100, 10508, 15553, 23021, 34076]
    , Character.hpFormula            = read "(lv)d10 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
    , Character.mpFormula            = ([], [])
    , Character.learningSpells       = []
}

mage = Character.Job {
      Character.jobName              = "Mage"
    , Character.enableAlignments     = [Character.G, Character.N, Character.E]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Spell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "agi"
    , Character.disarmTrapAbility    = parse' "agi"
    , Character.needParameter        = Parameter { strength = 0, iq = 11, piety = 0, vitality = 0, agility = 0, luck = 0 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "1"
    , Character.fightHitBonus        = read "lv/5"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [1100, 1628, 2409, 3565, 5276, 7808, 11556, 17104, 25314, 37468]
    , Character.hpFormula            = read "1d4"
    , Character.mpFormula            = (replicate 7 (read "max(0, (1d(pie/5) + lv - mlv*2)/2)"), [])
    , Character.learningSpells       = []
}

priest = Character.Job {
      Character.jobName              = "Priest"
    , Character.enableAlignments     = [Character.G, Character.N]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Spell, Character.Dispell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "agi"
    , Character.disarmTrapAbility    = parse' "agi"
    , Character.needParameter        = Parameter { strength = 0, iq = 0, piety = 11, vitality = 0, agility = 0, luck = 0 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d3", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "1"
    , Character.fightHitBonus        = read "lv/3+2"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [1050, 1554, 2300, 3404, 5038, 7456, 11035, 16332, 24172, 35777]
    , Character.hpFormula            = read "(lv)d8 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
    , Character.mpFormula            = ( replicate 7 (read "0")
                                       , read "min(9,max(1,lv*2-mlv*4+pie/6+1d3))"
                                       : replicate 6 (read "min(9,lv*2-mlv*4+pie/6+1d3)"))
    , Character.learningSpells       = [(read "max(1,lv*2-4+1d(pie/5))", SpellID <$> [111..115])
                                       ,(read "lv*2- 8+1d(pie/5)"      , SpellID <$> [121..124])
                                       ,(read "lv*2-12+1d(pie/5)"      , SpellID <$> [131..134])
                                       ,(read "lv*2-16+1d(pie/5)"      , SpellID <$> [141..144])
                                       ,(read "lv*2-20+1d(pie/5)"      , SpellID <$> [151..156])
                                       ,(read "lv*2-24+1d(pie/5)"      , SpellID <$> [161..164])
                                       ,(read "lv*2-28+1d(pie/5)"      , SpellID <$> [171..172])
                                       ]
}

thief = Character.Job {
      Character.jobName              = "Thief"
    , Character.enableAlignments     = [Character.N, Character.E]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Hide, Character.Ambush, Character.Spell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "min(agi*6, 95)"
    , Character.disarmTrapAbility    = parse' "(lv-7+50)*100/70"
    , Character.needParameter        = Parameter { strength = 0, iq = 0, piety = 0, vitality = 0, agility = 11, luck = 0 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d4", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "1"
    , Character.fightHitBonus        = read "lv/5"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [900, 1332, 1971, 2917, 4318, 6390, 9458, 13998, 20718, 30664]
    , Character.hpFormula            = read "(lv)d6 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
    , Character.mpFormula            = ([], [])
    , Character.learningSpells       = []
}

bishop = Character.Job {
      Character.jobName              = "Bishop"
    , Character.enableAlignments     = [Character.G, Character.N]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Spell, Character.Dispell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "min(95, lv*2 + pie*2)"
    , Character.disarmTrapAbility    = parse' "agi"
    , Character.needParameter        = Parameter { strength = 0, iq = 12, piety = 12, vitality = 0, agility = 0, luck = 0 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d4", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "1"
    , Character.fightHitBonus        = read "lv/5"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Just (read "max(5,min(95,50+(lv-itemLv)*5))")
    , Character.lvupExps             = [1200, 1776, 2628, 3890, 5757, 8520, 12610, 18665, 27625, 40889]
    , Character.hpFormula            = read "1d6"
    , Character.mpFormula            = (replicate 7 (read "max(0, (1d(iq/5) + lv - mlv*3)/2)"), replicate 7 (read "max(0, (1d(pie/5) + lv - mlv*3)/2)"))
    , Character.learningSpells       = []
}

samurai = Character.Job {
      Character.jobName              = "Samurai"
    , Character.enableAlignments     = [Character.G, Character.N]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Spell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "agi"
    , Character.disarmTrapAbility    = parse' "agi"
    , Character.needParameter        = Parameter { strength = 14, iq = 10, piety = 9, vitality = 14, agility = 11, luck = 0 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d6", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "min(lv/5+1,10)"
    , Character.fightHitBonus        = read "lv/3+2"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [1150, 1628, 2378, 3508, 5178, 7640, 11276, 16640, 24552, 36224]
    , Character.hpFormula            = read "1d8"
    , Character.mpFormula            = (replicate 7 (read "max(0, (1d(iq/5) + lv - mlv*4)/2)"), [])
    , Character.learningSpells       = []
}

lord = Character.Job {
      Character.jobName              = "Lord"
    , Character.enableAlignments     = [Character.G]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Spell, Character.Dispell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "agi"
    , Character.disarmTrapAbility    = parse' "agi"
    , Character.needParameter        = Parameter { strength = 15, iq = 12, piety = 12, vitality = 15, agility = 14, luck = 14 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "1d4", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "min(lv/5+1,10)"
    , Character.fightHitBonus        = read "lv/3+2"
    , Character.baseAC               = read "10"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [1250, 1800, 2592, 3732, 5374, 7738, 11142, 16044, 23103, 33268]
    , Character.hpFormula            = read "1d10"
    , Character.mpFormula            = ([], replicate 7 (read "max(0, (1d(pie/5) + lv - mlv*4)/2)"))
    , Character.learningSpells       = []
}

ninja = Character.Job {
      Character.jobName              = "Ninja"
    , Character.enableAlignments     = [Character.E]
    , Character.enableBattleCommands = [Character.Fight, Character.Parry, Character.Hide, Character.Ambush, Character.Spell, Character.UseItem, Character.Run]
    , Character.inspectTrapAbility   = parse' "min(agi*4, 95)"
    , Character.disarmTrapAbility    = parse' "(lv-7+50)*100/70"
    , Character.needParameter        = Parameter { strength = 17, iq = 17, piety = 17, vitality = 17, agility = 17, luck = 17 }
    , Character.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [L1, L2], Item.targetB = [], Item.damage = read "2d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [], Item.targetRange = Item.ToSingle }
    , Character.fightTryCount        = read "min(lv/5+2,10)"
    , Character.fightHitBonus        = read "lv/3+2"
    , Character.baseAC               = read "10-lv/3"
    , Character.identifyItemChance   = Nothing
    , Character.lvupExps             = [1400, 2072, 3066, 4538, 6717, 9940, 14711, 21773, 32225, 47692]
    , Character.hpFormula            = read "1d6"
    , Character.mpFormula            = ([], [])
    , Character.learningSpells       = []
}
