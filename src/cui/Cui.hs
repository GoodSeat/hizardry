module Cui
where

import Control.Monad
import Data.Char


-- | (1,1) means top-left position.
type Point = (Int, Int)

-- | (width, height) of graphic.
type Size  = (Int, Int)

data Dot = Blank | Draw Char deriving (Eq)

instance Show Dot where
    show Blank = " "
    show (Draw c) = [c]

-- | Graphic on console.
newtype Craphic = Craphic { at :: Point -> Dot }

instance Semigroup Craphic where
    v1 <> v2 = Craphic $ \p -> if at v1 p == Blank then at v2 p else at v1 p

instance Monoid Craphic where
    mempty = Craphic $ const Blank
    mappend = (<>)

-- ========================================================================
-- | draw Graphic at console.
draw :: Size -> Craphic -> IO()
draw (w,h) v = forM_ [1..h] $ \r -> drawRow r [1..w] v
  where
    drawRow :: Int -> [Int] -> Craphic -> IO()
    drawRow _ [] _ = putStrLn ""
    drawRow r (c:cs) v = do
        putStr $ show $ at v (c, r)
        drawRow r cs v


text :: Point -> String -> Craphic
text (c, r) txt = Craphic $ \(c', r') -> if r' /= r then Blank
                                                    else dotAt (const False) txt (c' - c) 

-- | draw rectangle.
rect :: Point    -- ^ base position of rectangle.
     -> Size     -- ^ size of rectangle.
     -> Dot      -- ^ fill inner of rectangle.
     -> Craphic  -- ^ created graphic.
rect (c, r) (w, h) fill = Craphic $ \(c', r') ->
    let isLR   = c' == c || c' == c + w - 1
        isTB   = r' == r || r' == r + h - 1
        isInLR = c' >= c && c' < c + w
        isInTB = r' >= r && r' < r + h in
    if      isLR   && isTB   then Draw '+'
    else if isLR   && isInTB then Draw '|'
    else if isTB   && isInLR then Draw '-'
    else if isInTB && isInLR then fill
    else                          Blank


-- | initialize Craphic from text lines.
fromTexts :: Char     -- ^ character that treat as blank.
          -> [String] -- ^ target text lines.
          -> Craphic  -- ^ created graphic
fromTexts blank s = Craphic $ \(c, r) -> at (lineOf r) c
  where
    lineOf r = if length s < r || r < 1 then [] else s !! (r - 1)
    at [] _                 = Blank
    at (t:cs) c | c < 1     = Blank
                | c == 1    = if t == blank then Blank else Draw t
                | otherwise = at cs (c - len [t])

-- | Dot from text at specified index.
dotAt :: (Char -> Bool) -- ^ judge method that chara is blank.
      -> String         -- ^ text of base.
      -> Int            -- ^ target index.
      -> Dot            -- ^ picked Dot.
dotAt isBlank [] _                 = Blank
dotAt isBlank (t:cs) c | c < 1     = Blank
                       | c == 1    = if isBlank t then Blank else Draw t
                       | otherwise = dotAt isBlank cs (c - len [t])


-- ========================================================================
type Transform = Point -> Point

translateP :: (Int, Int) -> Transform
translateP (dx, dy) (x, y) = (x + dx, y + dy)

reverseHP :: Int -> Transform
reverseHP c (x, y) = (-(x - c) + c, y)

reverseVP :: Int -> Transform
reverseVP c (x, y) = (x, -(y - c) + c)


type Filter = Craphic -> Craphic

filterWith :: Transform -> Filter
filterWith t v = Craphic $ at v . t

translate :: (Int, Int) -> Filter
translate (dx, dy) = filterWith $ translateP (-dx, -dy)

reverseH :: Int -> Filter
reverseH c = filterWith $ reverseHP c

reverseV :: Int -> Filter
reverseV c = filterWith $ reverseVP c

replace :: Dot -> Dot -> Filter
replace d1 d2 v = Craphic $ \(x, y) -> let o = at v (x, y) in
                                       if o == d1 then d2 else o

-- ========================================================================
-- | length of string.(count non-half character as 2)
len :: String -> Int
len s = length s + (length . filter (not . isHalfChar) $ s)

isHalfChar :: Char -> Bool
--isHalfChar c = 0xff61 <= n && n <= 0xff9f -- utf8
isHalfChar c = n <= 0xdf -- cp932
  where n = ord c

-- ========================================================================
                                                   
