module Main (main) where

import Prelude
import Test.Hspec
import qualified Spec
import qualified Engine.CharacterActionSpec

main :: IO ()
main = hspec $ do
    Spec.spec
    Engine.CharacterActionSpec.spec
