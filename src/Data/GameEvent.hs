module Data.GameEvent
where

import PreludeL
import qualified Data.Map as Map
import Data.Maze
import Data.Primitive
import Data.Formula
import Data.PlayEvent (SEType, BGMType)
import qualified Data.Spells as Spell
import qualified Data.Characters as Character

data Define =
            -- moving
              ReturnCastle
            | MoveTo Coord
            | StairsToUpper Coord
            | StairsToLower Coord

            -- interactive
            | Message     (LanguageSet String) (Maybe PictureInf)
            | MessageTime (LanguageSet String) (Maybe PictureInf) Int
            | Select      (LanguageSet String) (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".
            | Ask         (LanguageSet String) (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".
            | SelectC     (LanguageSet String) (LanguageSet String) (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".
            | AskC        (LanguageSet String) (LanguageSet String) (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".

            | MessageT     Int (LanguageSet String) (Maybe PictureInf)
            | MessageTimeT Int (LanguageSet String) (Maybe PictureInf) Int
            | SelectT      Int (LanguageSet String) (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".
            | AskT         Int (LanguageSet String) (Maybe PictureInf) [(String, Define)] -- ^ use "" when no match. use "\n" for empty input. "hoge\nfoo" matches "hoge" or "foo".

            | FlashMessage     (LanguageSet String)
            | FlashMessageTime (LanguageSet String) Int

            | SelectItem (LanguageSet String) (Maybe PictureInf) [(Maybe Formula, Define)] -- item id (Nothing mean other items, must be last one)

            -- happens
            | Switch [(Condition, Define)]
            | GetItem       TargetType Formula Bool [Define] -- item id, is wheter determined, if failed, go to second define.
            | LostItem      TargetType Formula [Define]      -- item id, if failed, go to second define.
            | GetGold       TargetType Formula
            | LostGold      TargetType Formula [Define]
            | ChangeHP      TargetType Formula
            | ChangeMP      TargetType Spell.Kind [Int] Formula -- target kind, Lv, heal point
            | ChangeJob     TargetType String -- job id.
            | LearningSpell TargetType Formula
            | ChangeEventFlag Int Formula -- change index, post changed value
            | ChangeLeader  PartyPos -- temporary change leader in this event.

            | PlaySoundEffect SEType
            | PlayBGM         BGMType

            | StartBattle Formula Define Define -- enemy id, when win, when run

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


