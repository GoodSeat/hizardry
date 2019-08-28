module CuiRender where

import Control.Monad

import Characters
import Cui


render :: Craphic -> IO ()
--render = draw (70, 36)
render c = ts `seq` forM_ ts putStrLn
  where
    ts = txts c

txts :: Craphic -> [String]
txts c = do
  l <- [1..36]
  return $ concat [show $ at c (col, l) | col <- [1..70]]

msgBox m = foldl1 (<>) $ fmap toText (zip [1..] ls) ++
    [rect (5, 5) (62, length ls + 2) (Draw ' ')]
  where
    ls = lines m
    toText (n, t) = text (6, 5 + n) t

status' p = foldl1 (<>) $ fmap toStatusLine (zip [1..] p) ++
    [text (4, 29) "#--CHARACTER NAME---------------CLASS-----AC----HITS---STATUS"
    ,rect (3, 29) (68, 8) (Draw ' ')]
  where
    toStatusLine (n, c) =  text (4,  29 + n) (show n ++ "  " ++ name c)
                        <> text (35, 29 + n) " G-Sam"
                        <> text (46, 29 + n) (show ac)
                        <> text (54, 29 + n) (show $ hp c)
                        <> text (61, 29 + n) (show $ maxhp c)
    ac = -7
    

