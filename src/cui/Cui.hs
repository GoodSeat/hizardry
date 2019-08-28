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

-- | test function.
main' :: IO ()
main' = do
    let t = text (10, 20) "T E S T" <>
            rect (8,  18) (25, 5) (Draw ' ')
    let v = t <>
            translate (10, -3) t <>
            status <> werdna <> dunsion
    draw (70, 36) v


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
    lineOf r = if length s < r then [] else s !! (r - 1)
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

type Filter = Craphic -> Craphic
translate :: (Int, Int) -> Filter
translate (dx, dy) v = Craphic $ at v . translateP (-dx, -dy)



-- ========================================================================
-- | length of string.(count non-half character as 2)
len :: String -> Int
len s = length s + (length . filter (not . isHalfChar) $ s)

isHalfChar :: Char -> Bool
--isHalfChar c = 0xff61 <= n && n <= 0xff9f -- utf8
isHalfChar c = n <= 0xdf -- cp932
  where n = ord c




-- ========================================================================
dunsion :: Craphic
dunsion = fromTexts ' '
  ["       +-------------------------------------------------------+      "  --  1
  ,"    +--|    C)AMP   S)TATUS   I)NSPECT   O)FF   A-W-D   K      |--+   "  --  2
  ,"    |  +-------------------------------------------------------+  |   "  --  3
  ,"    |        \\                                           _________|   "  --  4
  ,"    |        |\\                                         /|        |   "  --  5
  ,"    |        | \\                                       / |        |   "  --  6
  ,"    |        |  \\                                     /  |        |   "  --  7
  ,"    |        |   \\                                   /   |        |   "  --  8
  ,"    |        |    \\                                 /    |        |   "  --  9
  ,"    |＼      |     \\                               /     |        |   "  --  10
  ,"    |  ＼    |      \\                             /      |        |   "  --  11
  ,"    |    ＼  |       \\                           /       |        |   "  --  12
  ,"    |      | |        \\                         /        |        |   "  --  13
  ,"    |      | |        |\\                        |        |        |   "  --  14
  ,"    |      | | |＼    | \\                       |        |        |   "  --  15
  ,"    |      | | |  ＼  |  \\                      |        |        |   "  --  16
  ,"    |      | | |    | |   \\                     |        |        |   "  --  17
  ,"    |      | | |    | |    \\                ----|        |        |   "  --  18
  ,"    |      | | |    | ||＼ |\\             /|    |        |        |   "  --  19
  ,"    |      | | |    | ||  || \\           / |    |        |        |   "  --  20
  ,"    |      | | |    | ||  ||  \\         /  |    |        |        |   "  --  21
  ,"    |      | | |    | ||  ||＼|\\       /|  |    |        |        |   "  --  22
  ,"    |      | | |    | ||  ||||| \\     / |  |    |        |        |   "  --  23
  ,"    |      | | |    | ||  ||||| |     | |  |    |        |        |   "  --  24
  ,"    |      | | |    | ||  |||||／     ＼|  |    |        |        |   "  --  25
  ,"    |      | | |    | ||  |||／         ＼ |    |        |        |   "  --  26
  ,"    |      | | |    | ||  |／             ＼____|        |        |   "  --  27
  ,"    |      | | |    | || ／                     |        |        |   "  --  28
  ,"    |      | | |    | |／                       |        |        |   "  --  29
  ,"    |      | | |    |／                         ＼       |        |   "  --  30
  ,"    |      | | |   ／                             ＼     |        |   "  --  31
  ,"    |      | | | ／                                 ＼   |        |   "  --  32
  ,"    |      | | ／                                     ＼ |        |   "  --  33
  ,"    |      | ／                                         ＼________|   "  --  34
  ,"    |      ／                                                     |   "  --  35
  ,"    +-------------------------------------------------------------+   "] --  36
--  1234567890123456789012345678901234567890123456789012345678901234567890
--           1         2         3         4         5         6         7

status :: Craphic
status = fromTexts '*'
  ["**********************************************************************"  --   1
  ,"**********************************************************************"  --   2
  ,"**********************************************************************"  --   3
  ,"**********************************************************************"  --   4
  ,"**********************************************************************"  --   5
  ,"**********************************************************************"  --   6
  ,"**********************************************************************"  --   7
  ,"**********************************************************************"  --   8
  ,"**********************************************************************"  --   9
  ,"**********************************************************************"  --   10
  ,"**********************************************************************"  --   11
  ,"**********************************************************************"  --   12
  ,"**********************************************************************"  --   13
  ,"**********************************************************************"  --   14
  ,"**********************************************************************"  --   15
  ,"**********************************************************************"  --   16
  ,"**********************************************************************"  --   17
  ,"**********************************************************************"  --   18
  ,"**********************************************************************"  --   19
  ,"**********************************************************************"  --   20
  ,"**********************************************************************"  --   21
  ,"**********************************************************************"  --   22
  ,"**********************************************************************"  --   23
  ,"**********************************************************************"  --   24
  ,"**********************************************************************"  --   25
  ,"**********************************************************************"  --   26
  ,"**********************************************************************"  --   27
  ,"**********************************************************************"  --   28
  ,"*+--#--CHARACTER NAME---------------CLASS-----AC----HITS---STATUS---+*"  --   29
  ,"*|  1  TestPlayer1                  G-FIG      1      83     123    |*"  --   30
  ,"*|  2  TestPlayer2                  G-FIG      1      69     135    |*"  --   31
  ,"*|  3  TestPlayer3                  G-FIG      1     123     201    |*"  --   32
  ,"*|  4  TestPlayer4                  G-FIG      1     123     150    |*"  --   33
  ,"*|  5  TestPlayer5                  G-FIG      1     123     123    |*"  --   34
  ,"*|  6  TestPlayer6                  G-FIG      1     123     123    |*"  --   35
  ,"*+------------------------------------------------------------------+*"] --   36
--  1234567890123456789012345678901234567890123456789012345678901234567890
--           1         2         3         4         5         6         7

werdna :: Craphic
werdna = fromTexts ' '
  ["                                                                      "   --   1
  ,"                                                                      "   --   2
  ,"                                                                      "   --   3
  ,"                                                                      "   --   4
  ,"                                                                      "   --   5
  ,"                                                                      "   --   6
  ,"                                                                      "   --   7
  ,"                                    .8H[                              "   --   8
  ,"                                   .HKON-  .m,                        "   --   9
  ,"                                  . HHkK` .dNg,                       "   --   10
  ,"                       WUHX+...  jNNMQQWNNLdMM]  ...J++,              "   --   11
  ,"                            dM8dHNMdMHmdHuHMBkMM9^?!   ?`             "   --   12
  ,"                           (NgdkMMW#dC>dYMMMNQMb                      "   --   13
  ,"                       ..gHMMMSdkNKVw<(+4dMMMNNHMHWH,                 "   --   14
  ,"                    .&HHMHHHMNMMMMK1W$~j+NHMMMHMMMMMM|                "   --   15
  ,"                   .MMMHNMMMMMMMHMSljD<_vHWMMMMMMMMMMN,               "   --   16
  ,"                JHgWMMMHMMMMMMMMMMh+Wl~(WJHMMMMMMM#Cd#!               "   --   17
  ,"                4HMMHMHMMMMMNMMMNH5dXb(<UWHMM^!    T#`                "   --   18
  ,"                dHHHNHMMHMMMMMNMMSRzZPI~(dNM~                         "   --   19
  ,"               (F`  7^=7T^1dNMMMNWNOP$<~+gMM]                         "   --   20
  ,"             .MY        .dMMMMMMNM#dR1>((WHM]                         "   --   21
  ,"            .MP        .8JHMMM#MMMHMNk>(<1dNK+                        "   --   22
  ,"            (MR        JndmMMMMMMbMMMMz(l_dNWd~                       "   --   23
  ,"             ?Hn.,  . .HHHqHMMN#MNKHMMwJRwWMBf                        "   --   24
  ,"              (H;   .~JFOgHNMMMMM#qMMEdH6dMMM%                        "   --   25
  ,"               .74JT^ (;(HMMMMMHMHWHQHMBdMMM]                         "   --   26
  ,"                       ?(HHMMM#HM@@MMMNdMNMM]                         "   --   27
  ,"                          ?4MNNHHMHqMNMMHMHM]                         "   --   28
  ,"                       .   .BYHHMHYGiMMMMMHM]                         "   --   29
  ,"                       h, .HNMNHMMMMMMqMMMgM+....                     "   --   30
  ,"                   +JNMMNmgMMNMHMNHMMqkMqMNMMMMMY^                    "   --   31
  ,"                        .??????^^WHMMNNMNMHB^=!                       "   --   32
  ,"                                                                      "]  --   33
--  1234567890123456789012345678901234567890123456789012345678901234567890
--           1         2         3         4         5         6         7
                                                   
                                                   
