module Data.Maze
where

import Data.List
import Data.Maybe (isJust)
import qualified Data.Map as Map

-- | direction of view.
data Direction = N | S | E | W deriving (Read, Show, Eq, Ord)

-- | position in maze.
data Position = Position {
      direction :: Direction 
    , x         :: Int
    , y         :: Int
    , z         :: Int
    } deriving (Read, Show, Eq, Ord)

-- | coordinate in maze.
type Coord = (Int, Int, Int)

coordOf :: Position -> Coord
coordOf p = (x p, y p, z p)

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

rotate :: Direction -> (Int, Int) -> Maze -> Maze
rotate N _ m = m
rotate newN (w, h) m = \(x, y) ->
    let w' = (case newN of Data.Maze.E -> h
                           Data.Maze.W -> h
                           _           -> w);
        h' = (case newN of Data.Maze.E -> w
                           Data.Maze.W -> w
                           _           -> h);
        Position _ x' y' _ = rotatePositionRev newN (w', h') (Position N x y 0)
    in rotateGrid newN $ m (x', y')
  where
    rotatePositionRev newN (w, h) p
      | newN == E = let d' = case direction p of N -> W
                                                 E -> N
                                                 S -> E
                                                 W -> S
                    in  Position d' (y p) (w - x p + 1) (z p)
      | newN == W = let d' = case direction p of N -> E
                                                 E -> S
                                                 S -> W
                                                 W -> N
                    in Position d' (h - y p + 1) (x p) (z p)
      | otherwise = rotatePosition newN (w, h) p

rotatePosition :: Direction -> (Int, Int) -> Position -> Position
rotatePosition N _ p = p
rotatePosition newN (w, h) p
  | newN == S = let d' = case direction p of N -> S
                                             E -> W
                                             W -> E
                                             S -> N
                in Position d' (w - x p + 1) (h - y p + 1) (z p)
  | newN == E = let d' = case direction p of N -> W
                                             E -> N
                                             S -> E
                                             W -> S
                in Position d' (h - y p + 1) (x p) (z p)
  | newN == W = let d' = case direction p of N -> E
                                             E -> S
                                             S -> W
                                             W -> N
                in  Position d' (y p) (w - x p + 1) (z p)
  | otherwise = undefined

rotateGrid :: Direction -> Grid -> Grid
rotateGrid newN (Grid (fN, fE, fS, fW) ns)
  | newN == S = Grid (fS, fW, fN, fE) ns
  | newN == E = Grid (fE, fS, fW, fN) ns
  | newN == W = Grid (fW, fN, fE, fS) ns
  | otherwise = undefined

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
topWallFromText _ _ = undefined

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
sideWallFromText _ _ = undefined

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

data Side = F | L | R | B

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
sideOf B d = sideOf L .sideOf L $ d

noticesInView :: Maze      -- ^ target Maze.
              -> Position  -- ^ current position.
              -> Int       -- ^ distance to target grid. 0 means grid at current position.
              -> Int       -- ^ side distance. front is 0, and left is minus value.
              -> [Notice]
noticesInView l p d lr = ns
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
    Grid (_, _, _, _) ns = l (x p + dx, y p + dy)

visiblityAt :: Maze      -- ^ target Maze.
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

makeMazeMask :: Map.Map Coord Bool -> Char -> Char -> Int -> (Int, Int) -> [String]
makeMazeMask mvt mask blank z (_, 0) = []
makeMazeMask mvt mask blank z (w, h) = makeMazeMaskRow h w ++ makeMazeMask mvt mask blank z (w, h - 1)
  where
    makeMazeMaskRow :: Int -> Int -> [String]
    makeMazeMaskRow 1 w = [makeMazeMaskCol 1 1 w
                          ,makeMazeMaskCol 2 1 w
                          ,makeMazeMaskCol 3 1 w]
    makeMazeMaskRow y w = [makeMazeMaskCol 1 y w
                          ,makeMazeMaskCol 2 y w]
        
    makeMazeMaskCol :: Int -> Int -> Int -> String
    makeMazeMaskCol n y 1 = makeMask' (1, y) !! (n - 1)
    makeMazeMaskCol n y x = makeMazeMaskCol n y (x - 1) ++ tail (makeMask' (x, y) !! (n - 1))
    
    makeMask' :: (Int, Int) -> [String]
    makeMask' (x, y) = [[nw] ++ n ++ [ne]
                       ,[w'] ++ v ++ [e]
                       ,[sw] ++ s ++ [se]]
      where
        isVisited c = Map.lookup c mvt == Just True
        f0  = isVisited (x, y, z)
        fW  = isVisited (x - 1, y, z) || (x <= 1 && f0)
        fE  = isVisited (x + 1, y, z) || (x >= w && f0)
        fN  = isVisited (x, y + 1, z) || (y >= h && f0)
        fS  = isVisited (x, y - 1, z) || (y <= 1 && f0)
        fNW = isVisited (x - 1, y + 1, z)
        fNE = isVisited (x + 1, y + 1, z)
        fSE = isVisited (x + 1, y - 1, z)
        fSW = isVisited (x - 1, y - 1, z)
        v  = if not f0                                then [mask, mask] else [blank, blank]
        nw = if not f0 && not fN && not fW && not fNW then mask         else blank
        n  = if not f0 && not fN                      then [mask, mask] else [blank, blank]
        ne = if not f0 && not fN && not fE && not fNE then mask         else blank
        e  = if not f0 && not fE                      then mask         else blank
        se = if not f0 && not fE && not fS && not fSE then mask         else blank
        s  = if not f0 && not fS                      then [mask, mask] else [blank, blank]
        sw = if not f0 && not fW && not fS && not fSW then mask         else blank
        w' = if not f0 && not fW                      then mask         else blank



showMaze :: (Int, Int) -> Position -> Maze -> [String]
showMaze (_, 0) p _ = []
showMaze (w, h) p m = showMazeRow h w p m ++ showMaze (w, h - 1) p m

  where
    showMazeRow :: Int -> Int -> Position -> Maze -> [String]
    showMazeRow 1 w p m = [showMazeCol 1 1 w p m
                          ,showMazeCol 2 1 w p m
                          ,showMazeCol 3 1 w p m]
    showMazeRow y w p m = [showMazeCol 1 y w p m
                          ,showMazeCol 2 y w p m]
    
    showMazeCol :: Int -> Int -> Int -> Position -> Maze -> String
    showMazeCol n y 1 p m = showMaze' (1, y) p m !! (n - 1)
    showMazeCol n y x p m = showMazeCol n y (x - 1) p m ++ tail (showMaze' (x, y) p m !! (n - 1))
    
    
    showMaze' :: (Int, Int) -> Position -> Maze -> [String]
    showMaze' (x, y) p m = [[nw] ++ n ++ [ne]
                           ,[w]  ++ v ++ [e]
                           ,[sw] ++ s ++ [se]]
      where
        (x', y', _) = coordOf p
        v  = if x' == x && y' == y then "@" ++ sm (direction p)
                                   else "  "
        sm d | d == N    = "^"
             | d == E    = ">"
             | d == S    = "v"
             | d == W    = "<"
             | otherwise = show d
        g0 = m (x, y)
        gW = m (x - 1, y)
        gE = m (x + 1, y)
        gN = m (x, y + 1)
        gS = m (x, y - 1)
        gNW = m (x - 1, y + 1)
        gNE = m (x + 1, y + 1)
        gSE = m (x + 1, y - 1)
        gSW = m (x - 1, y - 1)
        dN = faceOf g0 N == Door || faceOf g0 N == SecretDoor ||
             faceOf gN S == Door || faceOf gN S == SecretDoor
        wN = not dN && (faceOf g0 N == Wall || faceOf gN S == Wall)
    
        dS = faceOf g0 S == Door || faceOf g0 S == SecretDoor ||
             faceOf gS N == Door || faceOf gS N == SecretDoor
        wS = not dS && (faceOf g0 S == Wall || faceOf gS N == Wall)
    
        dE = faceOf g0 E == Door || faceOf g0 E == SecretDoor ||
             faceOf gE W == Door || faceOf gE W == SecretDoor
        wE = not dE && (faceOf g0 E == Wall || faceOf gE W == Wall)
    
        dW = faceOf g0 W == Door || faceOf g0 W == SecretDoor ||
             faceOf gW E == Door || faceOf gW E == SecretDoor
        wW = not dW && (faceOf g0 W == Wall || faceOf gW E == Wall)
    
        fWN = faceOf gW N /= Passage || faceOf gNW S /= Passage
        fNW = faceOf gN W /= Passage || faceOf gNW E /= Passage
        nw | (dN || wN) && (dW || wW) = '+'
           | fNW && fWN               = '+'
           | (dN || wN) && fWN        = '-'
           | (dN || wN) && fNW        = '+'
           | (dW || wW) && fNW        = '|'
           | (dW || wW) && fWN        = '+'
           | otherwise                = ' '
        n  | wN        = "--"
           | dN        = "#-"
           | otherwise = "  "
    
        fNE = faceOf gN E /= Passage || faceOf gNE W /= Passage
        fEN = faceOf gE N /= Passage || faceOf gNE S /= Passage
        ne | (dN || wN) && (dE || wE) = '+'
           | fNE && fEN               = '+'
           | (dN || wN) && fEN        = '-'
           | (dN || wN) && fNE        = '+'
           | (dE || wE) && fNE        = '|'
           | (dE || wE) && fEN        = '+'
           | otherwise                = ' '
        e  | wE        = '|'
           | dE        = '#'
           | otherwise = ' '
    
        fES = faceOf gE S /= Passage || faceOf gSE N /= Passage
        fSE = faceOf gS E /= Passage || faceOf gSE W /= Passage
        se | (dS || wS) && (dE || wE) = '+'
           | fSE && fES               = '+'
           | (dS || wS) && fES        = '-'
           | (dS || wS) && fSE        = '+'
           | (dE || wE) && fSE        = '|'
           | (dE || wE) && fES        = '+'
           | otherwise                = ' '
        s  | wS        = "--"
           | dS        = "#-"
           | otherwise = "  "
    
        fSW = faceOf gS W /= Passage || faceOf gSW E /= Passage
        fWS = faceOf gW S /= Passage || faceOf gSW N /= Passage
        sw | (dS || wS) && (dW || wW) = '+'
           | fSW && fWS               = '+'
           | (dS || wS) && fWS        = '-'
           | (dS || wS) && fSW        = '+'
           | (dW || wW) && fSW        = '|'
           | (dW || wW) && fWS        = '+'
           | otherwise                = ' '
        w  | wW        = '|'
           | dW        = '#'
           | otherwise = ' '
    



-- ==========================================================================

testMaze :: Maze
testMaze = fromText (lines txt) (4, 5)
  where
    txt = "+--------+--+\n" ++
          "|     K  |[]|\n" ++
          "|  +^-+  +--+\n" ++
          "|  |_ >  |##|\n" ++
          "|  |   ^^|  |\n" ++
          "|  D     =##|\n" ++
          "|++|     |  |\n" ++
          "|  |     |  |\n" ++
          "|  +v-+vv+  |\n" ++
          "|~ <  =  |_ |\n" ++
          "+-----+-----+\n"
testMaze2 :: Maze
testMaze2 = fromText (lines txt) (6, 5)
  where
    txt = "+--------+--+--+--+\n" ++
          "|     K  |[]|[]|[]|\n" ++
          "|  +^-+  +--+--+  +\n" ++
          "|  |~ >  |## ##   |\n" ++
          "|  |   ^^|        |\n" ++
          "|  D     =## ##   |\n" ++
          "|++|     |        |\n" ++
          "|  |     |        |\n" ++
          "|  +v-+vv+        |\n" ++
          "|  <  =  |_       |\n" ++
          "+-----+-----+--+--+\n"

