module Data.Characters
where

import Prelude hiding (exp)
import Data.List (nub)
import Data.Primitive
import Data.Formula (Formula)
import qualified Data.Map as Map
import qualified Data.Spells as Spell

data Character = Character {
      name         :: !String         -- ^ name of character.
    , kind         :: !Kind           -- ^ kind.
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

    , items        :: ![ItemInf]       -- ^ items you have.
    , equips       :: ![ItemInf]       -- ^ items you equips.

    , spells       :: ![SpellID]      -- ^ learned spells.
    , mp           :: !([Int], [Int]) -- ^ MP
    , maxmp        :: !([Int], [Int]) -- ^ MaxMP
} deriving (Show, Eq)

instance Object Character where
  nameOf          = name
  hpOf            = hp
  maxhpOf         = maxhp
  paramOf         = param
  acOf            = const 10 -- TODO:
  lvOf            = lv
  statusErrorsOf  = statusErrors

  setHp           v c = let c' = c { hp = min (maxhpOf c) (max 0 v) } in if hp c' == 0 then addStatusError Dead c' else c'
  setAc           v c = c -- TODO:
  setStatusErrors v c = let c' = c { statusErrors = nub v }
                            ss = statusErrorsOf c' in
     if      Lost `elem` ss && length ss > 1 then setStatusErrors [Lost] c'
     else if Ash  `elem` ss && length ss > 1 then setStatusErrors [Ash]  c'
     else if Dead `elem` ss && length ss > 1 then setStatusErrors [Dead] c'
     else                                         c'


data Kind = Kind {
      kindName     :: !String
    , initialParam :: !Parameter
    , maxParam     :: !Parameter
    , initialBonus :: !Formula
} deriving (Show, Eq)


-- | data base of character.
type DB = Map.Map CharacterID Character


data Alignment = G | N | E deriving (Show, Eq)

-- | define of character class.
data Job = Job {
      jobName              :: !String
    , enableAlignments     :: ![Alignment]
    , enableBattleCommands :: ![BattleCommand]
    , inspectTrapAbility   :: !Formula -- ^ Probability of success of trap identification.
    , disarmTrapAbility    :: !Formula -- ^ Probability of success of disarming trap.
} --deriving (Show, Eq)
instance Show Job where
  show = jobName
instance Eq Job where
   j1 == j2 = jobName j1 == jobName j2



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

itemAt :: Character -> ItemPos -> ItemID
itemAt c ItemA = itemID $ items c !! 0
itemAt c ItemB = itemID $ items c !! 1
itemAt c ItemC = itemID $ items c !! 2
itemAt c ItemD = itemID $ items c !! 3
itemAt c ItemE = itemID $ items c !! 4
itemAt c ItemF = itemID $ items c !! 5
itemAt c ItemG = itemID $ items c !! 6
itemAt c ItemH = itemID $ items c !! 7
itemAt c ItemI = itemID $ items c !! 8
itemAt c ItemJ = itemID $ items c !! 9

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
                   | Parry
                   | UseItem
    deriving (Show, Eq)

-- =================================================================================

lvup :: Character -> (String, Character)
lvup c = (txt, c {
      lv    = lv c + 1
    , maxhp = maxhp c + uphp
    , hp    = hp c + uphp
    })
  where
    txt = "You made the next level !\n\n You gained 5 HitPoitns.\n"
    uphp = 5

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

