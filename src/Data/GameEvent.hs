module Data.GameEvent
where

import qualified Data.Map as Map
import Data.Maze
import Data.Primitive
import Data.Formula
import qualified Data.Characters as Character

data Define =
            -- moving
              ReturnCastle
            | MoveTo Coord
            | StairsToUpper Coord
            | StairsToLower Coord

            -- interactive
            | Message     String (Maybe PictureID)
            | MessageTime String (Maybe PictureID) Int
            | Select      String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.
            | Ask         String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.

            | MessageT     Int String (Maybe PictureID)
            | MessageTimeT Int String (Maybe PictureID) Int
            | SelectT      Int String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.
            | AskT         Int String (Maybe PictureID) [(String, Define)] -- ^ using empty text when no match.

            -- in battle
            | AsSpell SpellID

            -- happens
            | Switch [(Condition, Define)]
            | GetItem       TargetType Formula Bool -- item id, is wheter determined.
            | LostItem      TargetType Formula      -- item id.
            | GetGold       TargetType Formula
            | LostGold      TargetType Formula
            | ChangeHP      TargetType Formula
            | ChangeMP      TargetType Bool [Int] Formula -- target kind, Lv, heal point
            | LearningSpell TargetType Formula
            | ChangeEventFlag Int Formula -- change index, post changed value

            -- others
            | Reference GameEventID
            | End    -- ^ if there is another event, it start.
            | Escape -- ^ end event with ignore event on there.
            | Events [Define]

instance Semigroup Define where
    Events []  <> e2         = e2
    e1         <> Events []  = e1
    Events es1 <> Events es2 = Events $ es1 ++ es2
    Events es  <> e2         = Events $ es ++ [e2]
    e1         <> Events es2 = Events $ e1:es2
    e1         <> e2         = Events [e1, e2]

instance Monoid Define where
    mempty  = Events []
    mappend = (<>)

containsEvent :: (Define -> Bool) -> Define -> Bool
containsEvent f e@(Select _ _ ns) = f e || f (Events $ snd <$> ns)
containsEvent f e@(Ask    _ _ ns) = f e || f (Events $ snd <$> ns)
containsEvent f e@(Switch ns)     = f e || f (Events $ snd <$> ns)
containsEvent f e@(Events es)     = f e || any f es
containsEvent f e                 = f e


isInMazeOnly :: Define -> Bool
isInMazeOnly = containsEvent isInMazeOnly'
  where
    isInMazeOnly' ReturnCastle      = True
    isInMazeOnly' (MoveTo _)        = True
    isInMazeOnly' (StairsToUpper _) = True
    isInMazeOnly' (StairsToLower _) = True
    isInMazeOnly' _                 = False


type DB = Map.Map GameEventID Define

data Condition = PartyHasItem        ItemID
               | PartyExistAlignment [Character.Alignment]
               | LeaderKnowSpell     SpellID
               | LeaderIsJobOf       [String]
               | FormulaCheckParty   Formula -- ^ probablity happens(0~100).
               | FormulaCheckLeader  Formula -- ^ probablity happens(0~100).
               | And [Condition]
               | Or  [Condition]
               | Otherwise

data TargetType = Leader | All deriving (Show, Read, Eq)


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


