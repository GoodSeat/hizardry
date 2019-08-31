module Characters
where

import qualified Data.Map as Map

import qualified Spells as Spell
import qualified Items as Item

data ID = ID {
    id :: Int
} deriving (Show, Eq, Ord)

data Character = Character { 
      name         :: !String         -- ^ name of character.
    , age          :: !Int            -- ^ age
    , days         :: !Int            -- ^ past days after last birth.
    , lv           :: !Int            -- ^ level.
    , exp          :: !Int            -- ^ experiment points.
    , gold         :: !Int            -- ^ gold.

    , hp           :: !Int            -- ^ HP
    , maxhp        :: !Int            -- ^ MaxHP
    , param        :: !Parameter      -- ^ parameters.
    , marks        :: !Int            -- ^ count of defeated enemies.
    , rips         :: !Int            -- ^ count of dead.
    , statusErrors :: ![StatusError]  -- ^ status errors.
    
    , items        :: ![Item.ID]      -- ^ items you have.
    , equips       :: ![Item.ID]      -- ^ items you equips.

    , spells       :: ![Spell.ID]     -- ^ learned spells.
    , mp           :: !([Int], [Int]) -- ^ MP
    , maxmp        :: !([Int], [Int]) -- ^ MaxMP
} deriving (Show, Eq)

-- | define of character's parameter.
data Parameter = Parameter {
      strength :: !Int -- ^ strength
    , iq       :: !Int -- ^ I.Q.
    , piety    :: !Int -- ^ piety
    , vitality :: !Int -- ^ vitality
    , agility  :: !Int -- ^ agility
    , luck     :: !Int -- ^ luck
} deriving (Show, Eq)

-- | data base of character.
type DB = Map.Map ID Character


-- | type of status error.
data StatusError = Silence
                 | Paralysis
                 | Stoned
                 | Poison
                 | Fear
                 | Sleep
                 | Rigor
                 | Drain Int
                 | Dead
                 | Ash
                 | Lost
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
healHp p c = c { hp = min (hp c + p) (maxhp c) }

useGold :: Int -> Character -> Character
useGold p c = c { gold = gold c - p }
