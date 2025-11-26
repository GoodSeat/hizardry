module Data.Characters
where

import Prelude hiding (exp)
import Data.List (nub)
import Data.Primitive
import Data.Formula (Formula)
import qualified Data.Map as Map
import qualified Data.Spells as Spell
import qualified Data.Items as Item

data Character = Character {
      name         :: !String         -- ^ name of character.
    , race         :: !Race           -- ^ race
    , age          :: !Int            -- ^ age
    , days         :: !Int            -- ^ past days after last birth.
    , lv           :: !Int            -- ^ level.
    , exp          :: !Int            -- ^ experiment points.
    , gold         :: !Int            -- ^ gold.

    , job          :: !Job            -- ^ class of character.
    , alignment    :: !Alignment      -- ^ alignment of character.

    , hp           :: !Int            -- ^ HP
    , maxhp        :: !Int            -- ^ MaxHP
    , param        :: !Parameter      -- ^ parameters.
    , marks        :: !Int            -- ^ count of defeated enemies.
    , rips         :: !Int            -- ^ count of dead.
    , statusErrors :: ![StatusError]  -- ^ status errors.
    , paramDelta   :: ![(Term, ParamChange)] -- ^ temporary changed paramter.

    , items        :: ![ItemInf]      -- ^ items you have.
    , equips       :: ![ItemInf]      -- ^ items you equips.

    , spells       :: ![SpellID]      -- ^ learned spells.
    , mp           :: !([Int], [Int]) -- ^ MP
    , maxmp        :: !([Int], [Int]) -- ^ MaxMP
} deriving (Show, Eq, Read)

instance Object Character where
  nameOf          = name
  hpOf            = hp
  maxhpOf         = maxhp
  lvOf            = lv
  statusErrorsOf  = statusErrors

  setHp           v c = let c' = c { hp = min (maxhpOf c) (max 0 v) }
                        in if hp c' == 0 then addStatusError Dead c' else c'
  setStatusErrors v c = let c' = c { statusErrors = nub v }
                            ss = statusErrorsOf c' in
     if      Lost `elem` ss && length ss > 1 then setStatusErrors [Lost] c'
     else if Ash  `elem` ss && length ss > 1 then setStatusErrors [Ash]  c'
     else if Dead `elem` ss && length ss > 1 then setStatusErrors [Dead] c'
     else                                         c'


data Race = Race {
      raceName     :: !String
    , initialParam :: !Parameter
    , maxParam     :: !Parameter
    , initialBonus :: !Formula
} deriving (Show, Eq, Read)


-- | data base of character.
type DB = Map.Map CharacterID Character


data Alignment = G | N | E deriving (Show, Eq, Read)

-- | define of character class.
data Job = Job {
      jobName              :: !String
    , enableAlignments     :: ![Alignment]
    , enableBattleCommands :: ![BattleCommand]
    , inspectTrapAbility   :: !Formula -- ^ Probability of success of trap identification.
    , disarmTrapAbility    :: !Formula -- ^ Probability of success of disarming trap.
    , needParameter        :: !Parameter
    , baseWeaponAttr       :: !Item.WeaponAttr -- ^ use when no weapon equipd.
    , fightTryCount        :: !Formula
    , fightHitBonus        :: !Formula
    , baseAC               :: !Formula
    , lvupExps             ::  [Int]   -- need exp to next Lv.
    , hpFormula            :: !Formula -- use determine HP when Lvup.
    , mpFormula            :: !([Formula], [Formula]) -- use determine MP when Lvup.(variable of "mlv" means target magic lv(1~7).)
    , learningSpells       :: ![(Formula, [SpellID])] -- count of learning magic on this lv.(variable of "mlv" means target magic lv(1~7).)
} deriving (Show, Eq, Read)


totalExpToLv :: Job
             -> Int -- ^ target lv.
             -> Int
totalExpToLv j lv = totalExpToLv' (lvupExps j) (lv - 1)
  where
    totalExpToLv' [] _      = undefined
    totalExpToLv' _ 0       = 0
    totalExpToLv' [n] lv    = n + totalExpToLv' [n] (lv - 1)
    totalExpToLv' (n:ns) lv = n + totalExpToLv' ns (lv - 1)


data ItemPos = ItemA
             | ItemB
             | ItemC
             | ItemD
             | ItemE
             | ItemF
             | ItemG
             | ItemH
             | ItemI
             | ItemJ
  deriving (Show, Eq, Ord)

itemInfAt :: Character -> ItemPos -> ItemInf
itemInfAt c pos = items c !! itemPosToNum pos

itemAt :: Character -> ItemPos -> ItemID
itemAt c pos = itemID $ items c !! itemPosToNum pos

hasMaxCountItem :: Character -> Bool
hasMaxCountItem c = length (items c) >= 10

itemPosByChar :: String -> Maybe ItemPos
itemPosByChar "a" = Just ItemA
itemPosByChar "b" = Just ItemB
itemPosByChar "c" = Just ItemC
itemPosByChar "d" = Just ItemD
itemPosByChar "e" = Just ItemE
itemPosByChar "f" = Just ItemF
itemPosByChar "g" = Just ItemG
itemPosByChar "h" = Just ItemH
itemPosByChar "i" = Just ItemI
itemPosByChar "j" = Just ItemJ
itemPosByChar _   = Nothing

itemPosToNum :: ItemPos -> Int
itemPosToNum ItemA = 0
itemPosToNum ItemB = 1
itemPosToNum ItemC = 2
itemPosToNum ItemD = 3
itemPosToNum ItemE = 4
itemPosToNum ItemF = 5
itemPosToNum ItemG = 6
itemPosToNum ItemH = 7
itemPosToNum ItemI = 8
itemPosToNum ItemJ = 9

itemPosToText :: ItemPos -> String
itemPosToText ItemA = "A"
itemPosToText ItemB = "B"
itemPosToText ItemC = "C"
itemPosToText ItemD = "D"
itemPosToText ItemE = "E"
itemPosToText ItemF = "F"
itemPosToText ItemG = "G"
itemPosToText ItemH = "H"
itemPosToText ItemI = "I"
itemPosToText ItemJ = "J"

numToItemPos :: Int -> ItemPos
numToItemPos 0 = ItemA
numToItemPos 1 = ItemB
numToItemPos 2 = ItemC
numToItemPos 3 = ItemD
numToItemPos 4 = ItemE
numToItemPos 5 = ItemF
numToItemPos 6 = ItemG
numToItemPos 7 = ItemH
numToItemPos 8 = ItemI
numToItemPos 9 = ItemJ
numToItemPos _ = error "invalid posToItemChar"

-- =================================================================================

data BattleCommand = Fight
                   | Spell
                   | Hide
                   | Ambush
                   | Run
                   | Dispell
                   | Parry
                   | UseItem
    deriving (Show, Eq, Read)

-- =================================================================================

healHp :: Int -> Character -> Character
healHp p c = setHp (hp c + p) c

healMp :: Character -> Character
healMp c = c { mp = maxmp c }

useGold :: Int -> Character -> Character
useGold p c = c { gold = gold c - p }

getGold :: Int -> Character -> Character
getGold p = useGold (-p)

getExp :: Int -> Character -> Character
getExp p c = c { exp = exp c + p }

addDay :: Int -> Character -> Character
addDay d c = let d' = days c + d in if d' >= 365 then c { days = d' - 365, age = age c + 1 }
                                                 else c { days = d' }


canEquip :: Character -> Item.Define -> Bool
canEquip c idef = case Item.enableToEquip idef of Item.All     -> True
                                                  Item.Only js -> jobName (job c) `elem` js


toText :: Int -> Character -> String
toText w c = leftString w (name c) ++ show (alignment c) ++ "-" ++ take 3 (jobName $ job c)
        ++ rightTxt 10 (hp c) ++ "/" ++ rightTxt 5 (maxhp c)

-- =================================================================================

knowSpell :: SpellID -> Character -> Bool
knowSpell id c = id `elem` spells c

knowSpell' :: Spell.DB -> Spell.Define -> Character -> Bool
knowSpell' db def = knowSpell (Spell.defToID db def)

canSpell :: Spell.DB -> SpellID -> Character -> Bool
canSpell db id c = knowSpell id c && (typ (mp c) !! (lv - 1)) > 0
  where
    def = (Map.!) db id
    lv  = Spell.lv def
    typ = if Spell.kind def == Spell.M then fst else snd

canSpell' :: Spell.DB -> Spell.Define -> Character -> Bool
canSpell' db def = canSpell db (Spell.defToID db def)

costSpell :: Spell.DB -> SpellID -> Character -> Character
costSpell db id c = c { mp = (m', p') }
  where
    def = (Map.!) db id
    lv  = Spell.lv def
    isM = Spell.kind def == Spell.M
    cost 1  (n:ns) = n - 1 : ns
    cost lv (n:ns) = n : cost (lv - 1) ns
    m' = if isM     then cost lv (fst $ mp c) else fst $ mp c
    p' = if not isM then cost lv (snd $ mp c) else snd $ mp c

costSpell' :: Spell.DB -> Spell.Define -> Character -> Character
costSpell' db def = costSpell db (Spell.defToID db def)

