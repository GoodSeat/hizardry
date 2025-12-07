module Data.GameEvent
where

import qualified Data.Map as Map
import Data.Maze
import Data.Primitive
import Data.Formula
import qualified Data.Spells as Spell
import qualified Data.Characters as Character

data Define =
            -- moving
              ReturnCastle
            | MoveTo Coord
            | StairsToUpper Coord
            | StairsToLower Coord

            -- interactive
            | Message     String (Maybe PictureInf)
            | MessageTime String (Maybe PictureInf) Int
            | Select      String (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".
            | Ask         String (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".

            | MessageT     Int String (Maybe PictureInf)
            | MessageTimeT Int String (Maybe PictureInf) Int
            | SelectT      Int String (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".
            | AskT         Int String (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".

            -- happens
            | Switch [(Condition, Define)]
            | GetItem       TargetType Formula Bool -- item id, is wheter determined.
            | LostItem      TargetType Formula      -- item id.
            | GetGold       TargetType Formula
            | LostGold      TargetType Formula
            | ChangeHP      TargetType Formula
            | ChangeMP      TargetType Spell.Kind [Int] Formula -- target kind, Lv, heal point
            | ChangeJob     TargetType String -- job name.
            | LearningSpell TargetType Formula
            | ChangeEventFlag Int Formula -- change index, post changed value
            | ChangeLeader  PartyPos -- temporary change leader in this event.

            -- others
            | AsSpell SpellID
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

data Condition = PartyHasItem           ItemID Bool -- ^ itemID, must determined
               | PartyExistAlignment    [Character.Alignment]
               | PartyNotExistAlignment [Character.Alignment]
               | PartyPositionIs        [Position]
               | LeaderKnowSpell        SpellID
               | AnyOneKnowSpell        SpellID
               | LeaderIsJobOf          [String]
               | AnyOneIsJobOf          [String]
               | FormulaCheckParty      Formula -- ^ probablity happens(0~100).
               | FormulaCheckLeader     Formula -- ^ probablity happens(0~100).
               | And [Condition]
               | Or  [Condition]
               | Otherwise

data TargetType  = Leader | All deriving (Show, Read, Eq)


