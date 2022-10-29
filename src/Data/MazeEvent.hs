module Data.MazeEvent
where

import qualified Data.Map as Map
import Data.Maze
import Data.Primitive
import qualified Data.Characters as Character
import qualified Data.Spells as Spell
import Data.Formula

newtype ID = ID {
    num :: Int
} deriving (Show, Eq, Ord)

data Define =
            -- moving
              ReturnCastle
            | MoveTo Coord
            | StairsToUpper Coord
            | StairsToLower Coord

            -- interactive
            | Message String (Maybe PictureID)
            | Select String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.
            | Ask String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.

            -- happens
            | Switch [(Condition, Define)]
            | GetItem       TargetType Formula Bool -- item id, is wheter determined.
            | LostItem      TargetType Formula      -- item id.
            | GetGold       TargetType Formula
            | LostGold      TargetType Formula
            | ChangeHP      TargetType Formula
            | ChangeMP      TargetType Bool [Int] Formula -- target kind, Lv, heal point
            | LearningSpell TargetType Formula

            -- others
            | Reference ID
            | End    -- ^ if there is another event, it start.
            | Escape -- ^ end event with ignore event on there.
            | Events [Define]

type DB = Map.Map ID Define

data Condition = PartyHasItem        ItemID
               | PartyExistAlignment [Character.Alignment]
               | LeaderKnowSpell     Spell.ID
               | LeaderIsJobOf       [String]
               | ForumlaCheckParty   Formula
               | ForumlaCheckLeader  Formula
               | And [Condition]
               | Or  [Condition]
               | Otherwise

data TargetType = Leader | All | Front | Back

formulaMapParty :: [Character.Character] -> Map.Map String Int
formulaMapParty os = Map.fromList [
     ("partynum", length os )
    ,("ac"      , sum $ acOf <$> os)
    ,("lv"      , sum $ lvOf <$> os)
    ,("hp"      , sum $ hpOf <$> os)
    ,("maxhp"   , sum $ maxhpOf <$> os)
    ,("str"     , sum $ strength.paramOf <$> os)
    ,("iq"      , sum $ iq      .paramOf <$> os)
    ,("pie"     , sum $ piety   .paramOf <$> os)
    ,("vit"     , sum $ vitality.paramOf <$> os)
    ,("agi"     , sum $ agility .paramOf <$> os)
    ,("luc"     , sum $ luck    .paramOf <$> os)

    ,("age"     , sum $ Character.age  <$> os)
    ,("exp"     , sum $ Character.exp  <$> os)
    ,("gold"    , sum $ Character.gold <$> os)
    ,("marks"   , sum $ Character.marks <$> os)
    ,("rips"    , sum $ Character.rips <$> os)
    ]

formulaMap1 :: Int -> Int -> Character.Character -> Map.Map String Int
formulaMap1 i n o = Map.fromList [
     ("order"   , i)
    ,("partynum", n)
    ,("ac"      , acOf o)
    ,("lv"      , lvOf o)
    ,("hp"      , hpOf o)
    ,("maxhp"   , maxhpOf o)
    ,("str"     , strength.paramOf $ o)
    ,("iq"      , iq      .paramOf $ o)
    ,("pie"     , piety   .paramOf $ o)
    ,("vit"     , vitality.paramOf $ o)
    ,("agi"     , agility .paramOf $ o)
    ,("luc"     , luck    .paramOf $ o)

    ,("age"     , Character.age  o)
    ,("exp"     , Character.exp  o)
    ,("gold"    , Character.gold o)
    ,("marks"   , Character.marks o)
    ,("rips"    , Character.rips o)
    ]


