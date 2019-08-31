module Maze
where

import Data.List

-- | direction of view.
data Direction = N | S | E | W deriving (Show, Eq)

-- | position in maze.
data Position = Position {
      direction :: Direction 
    , x         :: Int
    , y         :: Int
    , z         :: Int
    } deriving (Show, Eq)

-- | coordinate in maze.
type Coord = (Int, Int, Int)

-- | face data around Grid.
data Face = Wall | Door | SecretDoor | Passage deriving (Show, Eq)

-- | special notice on Grid.
data Notice = Dark | Up | Down | Stone deriving (Show, Eq)

-- | grid of maze.
data Grid = Grid (Face, Face, Face, Face) [Notice] deriving (Show, Eq)

faceOf :: Grid -> Direction -> Face
faceOf (Grid (n, e, s, w) _) N = n
faceOf (Grid (n, e, s, w) _) E = e
faceOf (Grid (n, e, s, w) _) S = s
faceOf (Grid (n, e, s, w) _) W = w
          

-- | maze (group of grid).
type Maze = (Int, Int) -> Grid

-- ==========================================================================

-- | parse maze from text lines.
{-
 - -- : wall.
 - |  : wall.
 -
 - vv : one directon top to bottom.
 - v- : one directon door top to bottom.
 - ^^ : one directon bottom to top.
 - ^- : one directon door bottom to top.
 - ++ : door bottom to top.
 -
 - <  : one directon rihgt to left.
 - K  : one directon door rihgt to left.
 - >  : one directon left to right.
 - D  : one directon door left to right.
 - =  : door left to right.
 -
 - [] : stone.
 - XX : can't spelling zone.
 - ## : dark zone.
 - ~  : ceiling face notice.
 -  _ : floor face notice.

0     -- -- -- --
9 5  |     K  |[]|
8        ^-    --
7 4  |  |  >  |##|
6           ^^
5 3  |  D     =##|
4     ++           
3 2  |  |     |  |
2        v- vv    
1 1  |~ <  *  |_ |
0     -- -- -- --
      1  2  3  4
     0123456789012
-}
fromText :: [String]   -- ^ text lines to parse.
         -> (Int, Int) -- ^ size of maze.
         -> Maze
fromText ls (w, h) (x, y) = Grid (fn, fe, fs, fw) ntc
  where
    x' = (x - 1) `mod` w + 1
    y' = (y - 1) `mod` h + 1
    ts = blockText ls (x', y')
    fn = topWallFromText  N (ts !! 6, ts !! 7)
    fe = sideWallFromText E (ts !! 5)
    fs = topWallFromText  S (ts !! 0, ts !! 1)
    fw = sideWallFromText W (ts !! 2)
    ntc = noticeFromText [ts !! 3, ts !! 4]

topWallFromText :: Direction -> (Char, Char) -> Face
topWallFromText N (c1, c2) 
    | c1 == 'v' && c2 == 'v' = Wall
    | c1 == 'v' && c2 == '-' = Wall
    | c1 == '^' && c2 == '^' = Passage
    | c1 == '^' && c2 == '-' = Door
    | c1 == '+' && c2 == '+' = Door
    | c1 == '-' && c2 == '-' = Wall
    | otherwise              = Passage
topWallFromText S (c1, c2) 
    | c1 == '^' && c2 == '^' = Wall
    | c1 == '^' && c2 == '-' = Wall
    | c1 == 'v' && c2 == 'v' = Passage
    | c1 == 'v' && c2 == '-' = Door
    | c1 == '+' && c2 == '+' = Door
    | c1 == '-' && c2 == '-' = Wall
    | otherwise              = Passage

sideWallFromText :: Direction -> Char -> Face
sideWallFromText E c1
    | c1 == '<' = Wall
    | c1 == 'K' = Wall
    | c1 == '>' = Passage
    | c1 == 'D' = Door
    | c1 == '=' = Door
    | c1 == '|' = Wall
    | otherwise = Passage
sideWallFromText W c1
    | c1 == '>' = Wall
    | c1 == 'D' = Wall
    | c1 == '<' = Passage
    | c1 == 'K' = Door
    | c1 == '=' = Door
    | c1 == '|' = Wall
    | otherwise = Passage

noticeFromText :: [Char] -> [Notice]
noticeFromText [] = []
noticeFromText (c:cs)
    | c == '~'  = nub $ Up    : noticeFromText cs
    | c == '_'  = nub $ Down  : noticeFromText cs
    | c == '#'  = nub $ Dark  : noticeFromText cs
    | c == '['  = nub $ Stone : noticeFromText cs
    | otherwise = noticeFromText cs

 -- | get block text from text lines.
 --   result's index follows below position.
 --
 --   67
 --  2345
 --   01 
 --
blockText :: [String] -> (Int, Int) -> [Char]
blockText ls (x, y) = [ (ls' `on` (y * 2 - 2)) `at` (x * 3 - 2)   -- 0
                      , (ls' `on` (y * 2 - 2)) `at` (x * 3 - 1)   -- 1
                      , (ls' `on` (y * 2 - 1)) `at` (x * 3 - 3)   -- 2
                      , (ls' `on` (y * 2 - 1)) `at` (x * 3 - 2)   -- 3
                      , (ls' `on` (y * 2 - 1)) `at` (x * 3 - 1)   -- 4
                      , (ls' `on` (y * 2 - 1)) `at` (x * 3 - 0)   -- 5
                      , (ls' `on` (y * 2 - 0)) `at` (x * 3 - 2)   -- 6
                      , (ls' `on` (y * 2 - 0)) `at` (x * 3 - 1) ] -- 7
  where
    ls' = reverse ls
    on a n = if length a <= n then []  else a !! n
    at a n = if length a <= n then ' ' else a !! n

-- ==========================================================================

data Side = F | L | R

sideOf :: Side -> Direction -> Direction
sideOf F d = d
sideOf L N = W
sideOf L E = N
sideOf L S = E
sideOf L W = S
sideOf R N = E
sideOf R E = S
sideOf R S = W
sideOf R W = N

visiblityAt :: Maze -- ^ target Maze.
            -> Position  -- ^ current position.
            -> Int       -- ^ distance to target grid. 0 means grid at current position.
            -> Int       -- ^ side distance. front is 0, and left is minus value.
            -> Side      -- ^ target side.
            -> Face      -- ^ face that seen.
visiblityAt l p d lr s = faceOf g $ sideOf s dir
  where
    dir  = direction p
    dx | dir == E  =  d
       | dir == W  = -d
       | dir == N  =  lr
       | dir == S  = -lr
    dy | dir == N  =  d
       | dir == S  = -d
       | dir == E  = -lr
       | dir == W  =  lr
    g = l (x p + dx, y p + dy)

-- ==========================================================================

turnLeft :: Position -> Position
turnLeft p = p { direction = sideOf L $ direction p }

turnRight :: Position -> Position
turnRight p = p { direction = sideOf R $ direction p }

moveForward :: Position -> Position
moveForward p = p { x = x', y = y' }
  where
    dir  = direction p
    x'   = x p + if      dir == E then  1
                 else if dir == W then -1
                 else                   0
    y'   = y p + if      dir == N then  1
                 else if dir == S then -1
                 else                   0

moveBack :: Position -> Position
moveBack p = p { x = x', y = y' }
  where
    dir  = direction p
    x'   = x p + if      dir == E then -1
                 else if dir == W then  1
                 else                   0
    y'   = y p + if      dir == N then -1
                 else if dir == S then  1
                 else                   0

kickForward :: Maze -> Position -> Maybe Position
kickForward mz p = if visiblityAt mz p 0 0 F /= Wall then Just $ moveForward p else Nothing

walkForward :: Maze -> Position -> Maybe Position
walkForward mz p = if visiblityAt mz p 0 0 F == Passage then Just $ moveForward p else Nothing

-- ==========================================================================

testMaze :: Maze
testMaze = fromText (lines txt) (4, 5)
  where
    txt = "+--------+--+\n" ++
          "|     K  |[]|\n" ++
          "|  +^-+  +--+\n" ++
          "|  |  >  |##|\n" ++
          "|  |   ^^|  |\n" ++
          "|  D     =##|\n" ++
          "|++|     |  |\n" ++
          "|  |     |  |\n" ++
          "|  +v-+vv+  |\n" ++
          "|~ <  =  |_ |\n" ++
          "+-----+-----+\n"


main' :: IO()
main' = do
    let lbr = testMaze
    print $ lbr (1, 1)
    print $ lbr (2, 1)
    print $ lbr (3, 1)
    print $ lbr (4, 1)
    print $ lbr (5, 1)
    putStrLn ""
    print $ lbr (1, 2)
    print $ lbr (2, 2)
    print $ lbr (3, 2)
    print $ lbr (4, 2)
    putStrLn ""
    print $ lbr (1, 3)
    print $ lbr (2, 3)
    print $ lbr (3, 3)
    print $ lbr (4, 3)
    putStrLn ""
    print $ lbr (1, 4)
    print $ lbr (2, 4)
    print $ lbr (3, 4)
    print $ lbr (4, 4)
    putStrLn ""
    print $ lbr (1, 5)
    print $ lbr (2, 5)
    print $ lbr (3, 5)
    print $ lbr (4, 5)

    let p = Position {
      direction = N
    , z         = -1
    , x         = 1
    , y         = 1
    }
    putStrLn ""
    print $ visiblityAt lbr p 0 (-1) F
    print $ visiblityAt lbr p 0 0 L
    print $ visiblityAt lbr p 0 0 F
    print $ visiblityAt lbr p 0 0 R
    print $ visiblityAt lbr p 0 1 F
    putStrLn ""
    print $ visiblityAt lbr p 1 (-1) F
    print $ visiblityAt lbr p 1 0 L
    print $ visiblityAt lbr p 1 0 F
    print $ visiblityAt lbr p 1 0 R
    print $ visiblityAt lbr p 1 1 F

