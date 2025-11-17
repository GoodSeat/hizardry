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
            | Message     String (Maybe PictureInf)
            | MessageTime String (Maybe PictureInf) Int
            | Select      String (Maybe PictureInf) [(String, Define)] -- ^ using empty text when no match.
            | Ask         String (Maybe PictureInf) [(String, Define)] -- ^ using empty text when no match.

            | MessageT     Int String (Maybe PictureInf)
            | MessageTimeT Int String (Maybe PictureInf) Int
            | SelectT      Int String (Maybe PictureInf) [(String, Define)] -- ^ using empty text when no match.
            | AskT         Int String (Maybe PictureInf) [(String, Define)] -- ^ using empty text when no match.

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

data TargetType  = Leader | Any | All deriving (Show, Read, Eq)


