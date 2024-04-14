module SampleScenario.Jobs where

import qualified Data.Items as Item

import Data.Primitive
import Data.Formula
import qualified Data.Characters as Character

jobs :: [Character.Job]
jobs = [fighter, priest, thief]

fighter = Character.Job {
      Character.jobName              = "Fighter"
    , Character.enableAlignments     = [Character.G, Character.N, Character.E]
    , Character.enableBattleCommands = [
          Character.Fight
        , Character.Parry
        , Character.Spell
        , Character.UseItem
        , Character.Run
      ]
    , Character.inspectTrapAbility = parse' "agi"
    , Character.disarmTrapAbility  = parse' "(lv-7)*100/70"
    , Character.needParameter = Parameter {
          strength = 11
        , iq       = 0
        , piety    = 0
        , vitality = 0
        , agility  = 0
        , luck     = 0
      }
    , Character.baseWeaponAttr = Item.WeaponAttr {
          Item.targetF       = [L1, L2]
        , Item.targetB       = []
        , Item.damage        = read "2d2"
        , Item.doubleLabels  = []
        , Item.attrLabels    = []
        , Item.addStatusErrors = []
        , Item.atackMessages = []
      }
    , Character.fightTryCount = read "min(lv/5+1,10)"
    , Character.fightHitBonus = read "lv/3+2"
    , Character.baseAC        = read "10"
    , Character.lvupExps      = [
        1000,724,1248,2152,3710,6397,11029,19015,32785,56526,97458,168031,289709
      ]
    , Character.hpFormula     = read "(lv)d10 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
    , Character.mpFormula     = ([], [])
    , Character.learningSpells= []
}

priest = Character.Job {
      Character.jobName              = "Priest"
    , Character.enableAlignments     = [Character.G, Character.N]
    , Character.enableBattleCommands = [
          Character.Fight
        , Character.Parry
        , Character.Spell
        , Character.UseItem
        , Character.Run
      ]
    , Character.inspectTrapAbility = parse' "agi"
    , Character.disarmTrapAbility  = parse' "(lv-7)*100/70"
    , Character.needParameter = Parameter {
          strength = 0
        , iq       = 0
        , piety    = 11
        , vitality = 0
        , agility  = 0
        , luck     = 0
      }
    , Character.baseWeaponAttr = Item.WeaponAttr {
          Item.targetF       = [L1, L2]
        , Item.targetB       = []
        , Item.damage        = read "2d2"
        , Item.doubleLabels  = []
        , Item.attrLabels    = []
        , Item.addStatusErrors = []
        , Item.atackMessages = []
      }
    , Character.fightTryCount = read "1"
    , Character.fightHitBonus = read "lv/3+2"
    , Character.baseAC        = read "10"
    , Character.lvupExps      = [
        1050,760,1310,2259,3895,6715,11578,19962,34417,59343,102307,176397,304132
      ]
    , Character.hpFormula     = read "(lv)d8 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
    , Character.mpFormula     = ( replicate 7 (read "0")
                                , read "min(9,max(1,lv*2-mlv*4+pie/6+1d3))"
                                : replicate 6 (read "min(9,lv*2-mlv*4+pie/6+1d3)"))
    , Character.learningSpells= [(read "max(1,lv*2-4+1d(pie/5))", SpellID <$> [111..115])
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
    , Character.enableAlignments     = [Character.E, Character.N]
    , Character.enableBattleCommands = [
          Character.Fight
        , Character.Parry
        , Character.UseItem
        , Character.Run
        ]
    , Character.inspectTrapAbility = parse' "min(agi*6, 95)"
    , Character.disarmTrapAbility  = parse' "(lv-7+50)*100/70"
    , Character.needParameter = Parameter {
          strength = 0
        , iq       = 0
        , piety    = 0
        , vitality = 0
        , agility  = 11
        , luck     = 0
       }
    , Character.baseWeaponAttr = Item.WeaponAttr {
          Item.targetF       = [L1, L2]
        , Item.targetB       = []
        , Item.damage        = read "2d2"
        , Item.doubleLabels  = []
        , Item.attrLabels    = []
        , Item.addStatusErrors = []
        , Item.atackMessages = []
      }
    , Character.fightTryCount = read "min(lv/5+1,10)"
    , Character.fightHitBonus = read "lv/5"
    , Character.baseAC        = read "10"
    , Character.lvupExps      = [
        900,651,1123,1936,3338,5755,9922,17107,29495,50854,87679,151171,260639
      ]
    , Character.hpFormula     = read "(lv)d6 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
    , Character.mpFormula     = ([], [])
    , Character.learningSpells= []
}
