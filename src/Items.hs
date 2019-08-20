module Items
where


data Item = Item {
      name       :: String -- ^ 名前
    , identified :: Bool   -- ^ 識別済みか否か
} deriving (Show, Eq, Ord)
