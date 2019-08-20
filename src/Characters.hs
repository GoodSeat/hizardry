module Characters
where

import Spells
import Items

data Character = Character { 
      name     :: String  -- ^ 名前
    , age      :: Integer -- ^ 年齢
    , lv       :: Integer -- ^ レベル
    , exp      :: Integer -- ^ 経験値
    , gold     :: Integer -- ^ 所持金

    , hp       :: Integer -- ^ HP
    , status   :: Status  -- ^ ステータス
    , marks    :: Integer -- ^ 倒した敵の数
    , rips     :: Integer -- ^ 死亡数
    
    , items    :: [Item]         -- ^ 所持アイテム
    , spells   :: [Spell]        -- ^ 習得済みの魔法
    , mp       :: ([Int], [Int]) -- ^ MP
} deriving (Show, Eq)

data Status = Status {
      strength :: Integer -- ^ 力
    , iq       :: Integer -- ^ 知恵
    , piety    :: Integer -- ^ 信仰心
    , vitality :: Integer -- ^ 生命力
    , agility  :: Integer -- ^ 素早さ
    , luck     :: Integer -- ^ 運の強さ
} deriving (Show, Eq)


