module Main (main) where

import Test.DocTest
import System.IO

main :: IO ()
main = do
  doctest ["src/Data/RomajiToHiragana.hs"]
