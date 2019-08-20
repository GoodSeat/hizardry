module Spells
where


data Kind = M | P deriving (Show, Eq)

data Spell = Spell {
      name       :: String   -- ^ 名前
    , kind       :: Kind     -- ^ 種別
    , lv         :: Integer  -- ^ 魔法レベル
} deriving (Show, Eq)
