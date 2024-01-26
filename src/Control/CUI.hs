module Control.CUI
where

import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import System.Console.ANSI


-- | (1,1) means top-left position.
type Point = (Int, Int)

-- | (width, height) of graphic.
type Size  = (Int, Int)

data Dot = Blank
         | Draw Char
         | DrawSGR Char [SGR]
         | NoDraw -- ^ post non-half character 
  deriving (Eq)

instance Show Dot where
    show Blank         = " "
    show (Draw c)      = [c]
    show (DrawSGR c _) = [c]
    show NoDraw        = ""

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
        let dot = at v (c, r)
        case dot of DrawSGR _ sgrs -> setSGR sgrs
                    _              -> setSGR [Reset]
        putStr $ show dot
        drawRow r cs v


text :: Point -> String -> Craphic
text (c, r) txt = Craphic $ \(c', r') -> if r' /= r then Blank
                                                    else dotAt (const False) txt (c' - c)

textSGR :: Point -> String -> String -> Craphic
textSGR (c, r) txt ss = Craphic $ \(c', r') ->
    if r' /= r then Blank
    else let d = dotAt (const False) txt (c' - c) in
      case d of Draw t      -> draw c' t
                DrawSGR t _ -> draw c' t
                _           -> d
  where
    draw c' t = let sgr = if length ss <= c' - c - 1 then Nothing else toSGR (ss !! (c' - c - 1))
                in case sgr of Nothing -> Draw t
                               Just sg -> DrawSGR t sg



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


toSGR :: Char -> Maybe [SGR]
toSGR c | c == 'B' = Just [SetColor Foreground Vivid Black]
        | c == 'R' = Just [SetColor Foreground Vivid Red]
        | c == 'G' = Just [SetColor Foreground Vivid Green]
        | c == 'Y' = Just [SetColor Foreground Vivid Yellow]
        | c == 'L' = Just [SetColor Foreground Vivid Blue]
        | c == 'M' = Just [SetColor Foreground Vivid Magenta]
        | c == 'C' = Just [SetColor Foreground Vivid Cyan]
        | c == 'W' = Just [SetColor Foreground Vivid White]
        | c == 'b' = Just [SetColor Foreground Dull Black]
        | c == 'r' = Just [SetColor Foreground Dull Red]
        | c == 'g' = Just [SetColor Foreground Dull Green]
        | c == 'y' = Just [SetColor Foreground Dull Yellow]
        | c == 'l' = Just [SetColor Foreground Dull Blue]
        | c == 'm' = Just [SetColor Foreground Dull Magenta]
        | c == 'c' = Just [SetColor Foreground Dull Cyan]
        | c == 'w' = Just [SetColor Foreground Dull White]

        | c == '0' = Just [SetColor Background Vivid Black]
        | c == '1' = Just [SetColor Background Dull Red]
        | c == '2' = Just [SetColor Background Dull Green]
        | c == '3' = Just [SetColor Background Dull Yellow]
        | c == '4' = Just [SetColor Background Dull Blue]
        | c == '5' = Just [SetColor Background Dull Magenta]
        | c == '6' = Just [SetColor Background Dull Cyan]
        | c == '7' = Just [SetColor Background Dull White]

        | otherwise = Nothing


-- | initialize Craphic from text lines.
fromTexts :: Char     -- ^ character that treat as blank.
          -> [String] -- ^ target text lines.
          -> Craphic  -- ^ created graphic
fromTexts blank s = fromTextsSGR blank s []

fromTextsA :: Char     -- ^ character that treat as blank.
           -> Char     -- ^ character to specify SGR.
           -> [String] -- ^ target text lines.
           -> Craphic  -- ^ created graphic
fromTextsA blank sgr s = fromTextsSGR blank s $ replicate (length s) (replicate (maximum $ length <$> s) sgr)

-- | initialize Craphic from text lines.
fromTextsSGR :: Char     -- ^ character that treat as blank.
             -> [String] -- ^ target text lines.
             -> [String] -- ^ sgr specify
             -> Craphic  -- ^ created graphic
fromTextsSGR blank s cs = Craphic $ \(c, r) -> at (lineOf s r) (lineOf cs r) c
  where
    lineOf tl r = if length tl < r || r < 1 then [] else tl !! (r - 1)
    at txt sgrs c = case dotAt (==blank) txt c of
                      Blank  -> Blank
                      NoDraw -> NoDraw
                      Draw t -> case dotAt (==blank) sgrs c of
                                  Draw s -> case toSGR s of Just ss -> DrawSGR t ss
                                                            Nothing -> Draw t
                                  _      -> Draw t
                      _      -> undefined

-- | Dot from text at specified index.
dotAt :: (Char -> Bool) -- ^ judge method that chara is blank.
      -> String         -- ^ text of base.
      -> Int            -- ^ target index.
      -> Dot            -- ^ picked Dot.
dotAt isBlank [] _                              = Blank
dotAt isBlank (t:cs) c | c < 1                  = Blank
                       | c == 1                 = if isBlank t then Blank else Draw t
                       | c == 2 && len [t] == 2 = NoDraw
                       | otherwise              = dotAt isBlank cs (c - len [t])
                    -- | otherwise              = dotAt isBlank cs (c - 1)


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

trim :: Point -> Size -> Craphic -> Craphic
trim (x, y) (w, h) c = Craphic $ \(x', y') -> 
    if x' < x || x + w <= x' || y' < y || y + h <= y' then Blank
                                                      else at c (x', y')


changeSGR :: Char -> Filter
changeSGR sgrs v = Craphic $ \(x, y) -> let o = at v (x, y) in
    case o of Draw c      -> DrawSGR c s
              DrawSGR c _ -> DrawSGR c s
              Blank       -> Blank
              NoDraw      -> NoDraw
  where
    s = fromMaybe [Reset] (toSGR sgrs)

addSGR :: Char -> Filter
addSGR sgrs v = Craphic $ \(x, y) -> let o = at v (x, y) in
    case o of Draw c       -> DrawSGR c s
              DrawSGR c s' -> DrawSGR c s'
              Blank        -> Blank
              NoDraw       -> NoDraw
  where
    s = fromMaybe [Reset] (toSGR sgrs)

-- ========================================================================
-- | length of string.(count non-half character as 2)
len :: String -> Int
len s = length s + (length . filter (not . isHalfChar) $ s)

isHalfChar :: Char -> Bool
--isHalfChar c = 0xff61 <= n && n <= 0xff9f -- utf8
isHalfChar c = n <= 0xdf -- cp932
  where n = ord c

-- ========================================================================

