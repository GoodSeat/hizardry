module Data.RomajiToHiragana (romajiToHiragana) where

import           Data.Char (isAscii, isControl)
import           Data.Function (on)
import           Data.List (find, isPrefixOf, sortBy)
import           Data.Ord  (comparing)

romajiToHiraganaMap :: [(String, String)]
romajiToHiraganaMap = sortBy (flip (comparing length) `on` fst) $
    [ ("a", "あ"), ("i", "い"), ("u", "う"), ("e", "え"), ("o", "お")
    , ("ka", "か"), ("ki", "き"), ("ku", "く"), ("ke", "け"), ("ko", "こ")
    , ("sa", "さ"), ("shi", "し"), ("si", "し"), ("su", "す"), ("se", "せ"), ("so", "そ")
    , ("ta", "た"), ("chi", "ち"), ("ti", "ち"), ("tsu", "つ"), ("tu", "つ"), ("te", "て"), ("to", "と")
    , ("na", "な"), ("ni", "に"), ("nu", "ぬ"), ("ne", "ね"), ("no", "の")
    , ("ha", "は"), ("hi", "ひ"), ("fu", "ふ"), ("hu", "ふ"), ("he", "へ"), ("ho", "ほ")
    , ("ma", "ま"), ("mi", "み"), ("mu", "む"), ("me", "め"), ("mo", "も")
    , ("ya", "や"), ("yu", "ゆ"), ("yo", "よ")
    , ("ra", "ら"), ("ri", "り"), ("ru", "る"), ("re", "れ"), ("ro", "ろ")
    , ("wa", "わ"), ("wo", "を"), ("n", "ん"), ("-", "ー")
    , ("ga", "が"), ("gi", "ぎ"), ("gu", "ぐ"), ("ge", "げ"), ("go", "ご")
    , ("za", "ざ"), ("ji", "じ"), ("zi", "じ"), ("zu", "ず"), ("ze", "ぜ"), ("zo", "ぞ")
    , ("da", "だ"), ("di", "ぢ"), ("du", "づ"), ("de", "で"), ("do", "ど")
    , ("ba", "ば"), ("bi", "び"), ("bu", "ぶ"), ("be", "べ"), ("bo", "ぼ")
    , ("pa", "ぱ"), ("pi", "ぴ"), ("pu", "ぷ"), ("pe", "ぺ"), ("po", "ぽ")
    , ("kya", "きゃ"), ("kyu", "きゅ"), ("kyo", "きょ")
    , ("gya", "ぎゃ"), ("gyu", "ぎゅ"), ("gyo", "ぎょ")
    , ("sha", "しゃ"), ("sya", "しゃ"), ("shu", "しゅ"), ("syu", "しゅ"), ("sho", "しょ"), ("syo", "しょ")
    , ("ja", "じゃ"), ("ju", "じゅ"), ("jo", "じょ")
    , ("cha", "ちゃ"), ("tya", "ちゃ"), ("chu", "ちゅ"), ("tyu", "ちゅ"), ("cho", "ちょ"), ("tyo", "ちょ")
    , ("nya", "にゃ"), ("nyu", "にゅ"), ("nyo", "にょ")
    , ("hya", "ひゃ"), ("hyu", "ひゅ"), ("hyo", "ひょ")
    , ("bya", "びゃ"), ("byu", "びゅ"), ("byo", "びょ")
    , ("pya", "ぴゃ"), ("pyu", "ぴゅ"), ("pyo", "ぴょ")
    , ("mya", "みゃ"), ("myu", "みゅ"), ("myo", "みょ")
    , ("rya", "りゃ"), ("ryu", "りゅ"), ("ryo", "りょ")
    , ("jya", "じゃ"), ("jyu", "じゅ"), ("jyo", "じょ")
    , ("wi", "ゐ"), ("we", "ゑ")
    ]

-- | Converts a romaji string to a hiragana string.
-- Returns 'Nothing' if the input contains unconvertible sequences.
--
-- Examples:
--
-- >>> romajiToHiragana "x"
-- Nothing
romajiToHiragana :: String -> Maybe String
romajiToHiragana = fmap concat . sequence . convert
  where
    convert :: String -> [Maybe String]
    convert [] = []
    convert s@(c:cs) =
        case s of
            'n':'n':rest -> Just "ん" : convert rest
            't':'c':'h':rest -> Just "っち" : convert rest
            c1:c2:rest | c1 == c2 && c1 `elem` "bcdfghjklmpqrstvwxyz" ->
                Just "っ" : convert (c2:rest)
            _ -> go s

    go :: String -> [Maybe String]
    go [] = []
    go str =
        case find (\(p, _) -> p `isPrefixOf` str) romajiToHiraganaMap of
            Just (p, h) -> Just h : convert (drop (length p) str)
            Nothing ->
                let (c, rest) = (head str, tail str)
                in if isAscii c && isControl c
                       then Just [c] : convert rest
                       else if isAscii c
                                then [Nothing]
                                else Just [c] : convert rest
