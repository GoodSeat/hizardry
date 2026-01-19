module Main (main) where

import Prelude
import Test.DocTest
import System.IO

main :: IO ()
main = do
  doctest ["src/Data/RomajiToHiragana.hs", "src/Data/Formula.hs"]
