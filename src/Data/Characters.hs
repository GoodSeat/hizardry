module Data.Characters
where

import Data.List (nub)
import Data.Primitive
import qualified Data.Map as Map
import qualified Data.Spells as Spell

data Character = Character { 
      name         :: !String         -- ^ name of character.
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



-- | data base of character.
type DB = Map.Map CharacterID Character


data Alignment = G | N | E deriving (Show, Eq)

-- | define of character class.
data Job = Job {
      jobName              :: !String
    , enableAlignments     :: ![Alignment]
    , enableBattleCommands :: ![BattleCommand]
} deriving (Show, Eq)


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

addDay :: Int -> Character -> Character
addDay d c = let d' = days c + d in if d' >= 365 then c { days = d' - 365, age = age c + 1 }
                                                 else c { days = d' }


-- =================================================================================


knowSpell :: SpellID -> Character -> Bool
knowSpell id c = id `elem` spells c

canSpell :: Spell.DB -> SpellID -> Character -> Bool
canSpell db id c = knowSpell id c && (typ (mp c) !! lv) > 0
  where
    def = (Map.!) db id
    lv  = Spell.lv def
    typ = if Spell.kind def == Spell.M then fst else snd

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



