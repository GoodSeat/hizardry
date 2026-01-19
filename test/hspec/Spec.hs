module Spec (spec) where

import Prelude
import Data.RomajiToHiragana (romajiToHiragana)
import Test.Hspec

spec :: Spec
spec = do
  describe "romajiToHiragana" $ do
    it "converts basic romaji to hiragana" $
      romajiToHiragana "ohayou" `shouldBe` Just "おはよう"

    it "handles 'nn' correctly" $
      romajiToHiragana "konnnitiha" `shouldBe` Just "こんにちは"

    it "handles sokuon (double consonants)" $
      romajiToHiragana "kekka" `shouldBe` Just "けっか"

    it "handles chouon (long vowels)" $
      romajiToHiragana "ra-men" `shouldBe` Just "らーめん"

    it "handles youon (contracted sounds)" $
      romajiToHiragana "chotto" `shouldBe` Just "ちょっと"

    it "handles 'n' followed by a consonant" $
      romajiToHiragana "anta" `shouldBe` Just "あんた"

    it "returns Nothing for invalid romaji sequences" $
      romajiToHiragana "x" `shouldBe` Nothing

    it "handles mixed cases" $
      romajiToHiragana "arigato-gozaimasu" `shouldBe` Just "ありがとーございます"

    it "converts another complex case" $
      romajiToHiragana "sinnyuu" `shouldBe` Just "しんゆう"

    
