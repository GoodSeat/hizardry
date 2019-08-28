module Characters
where

import qualified Spells as Spell
import qualified Items as Item

data Character = Character { 
      name     :: String  -- ^ 名前
    , age      :: Int     -- ^ 年齢
    , lv       :: Int     -- ^ レベル
    , exp      :: Int     -- ^ 経験値
    , gold     :: Int     -- ^ 所持金

    , hp       :: Int     -- ^ HP
    , maxhp    :: Int     -- ^ MaxHP
    , status   :: Status  -- ^ ステータス
    , marks    :: Int     -- ^ 倒した敵の数
    , rips     :: Int     -- ^ 死亡数
    
    , items    :: [Item.Item]    -- ^ 所持アイテム
    , spells   :: [Spell.Spell]  -- ^ 習得済みの魔法
    , mp       :: ([Int], [Int]) -- ^ MP
    , maxmp    :: ([Int], [Int]) -- ^ MaxMP
} deriving (Show, Eq)

data Status = Status {
      strength :: Int -- ^ 力
    , iq       :: Int -- ^ 知恵
    , piety    :: Int -- ^ 信仰心
    , vitality :: Int -- ^ 生命力
    , agility  :: Int -- ^ 素早さ
    , luck     :: Int -- ^ 運の強さ
} deriving (Show, Eq)


lvup :: Character -> (String, Character)
lvup c = (txt, c { 
      lv    = lv c + 1
    , maxhp = maxhp c + uphp
    , hp    = hp c + uphp
    })
  where
    txt = name c ++ " is Lv Up!!\n\n  * HP up 5. \n"
    uphp = 5

healHp :: Int -> Character -> Character
healHp p c = c { hp = min (hp c + p) (maxhp c) }
