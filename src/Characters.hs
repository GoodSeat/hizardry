module Characters
where

import qualified Data.Map as Map

import qualified Spells as Spell
import qualified Items as Item

data ID = ID {
    id :: Int
} deriving (Show, Eq, Ord)

data Character = Character { 
      name     :: String     -- ^ 名前
    , age      :: Int        -- ^ 年齢
    , lv       :: Int        -- ^ レベル
    , exp      :: Int        -- ^ 経験値
    , gold     :: Int        -- ^ 所持金

    , hp       :: Int        -- ^ HP
    , maxhp    :: Int        -- ^ MaxHP
    , param    :: Parameter  -- ^ ステータス
    , marks    :: Int        -- ^ 倒した敵の数
    , rips     :: Int        -- ^ 死亡数
    
    , items    :: [Item.ID]      -- ^ 所持アイテム
    , equips   :: [Item.ID]      -- ^ 装備中のアイテム

    , spells   :: [Spell.Spell]  -- ^ 習得済みの魔法
    , mp       :: ([Int], [Int]) -- ^ MP
    , maxmp    :: ([Int], [Int]) -- ^ MaxMP
} deriving (Show, Eq)

-- | define of character's parameter.
data Parameter = Parameter {
      strength :: Int -- ^ strength
    , iq       :: Int -- ^ I.Q.
    , piety    :: Int -- ^ piety
    , vitality :: Int -- ^ vitality
    , agility  :: Int -- ^ agility
    , luck     :: Int -- ^ luck
} deriving (Show, Eq)

-- | data base of character.
type DB = Map.Map ID Character

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
