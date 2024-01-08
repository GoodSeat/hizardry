module Data.Primitive
where

import GHC.Stack (HasCallStack)
import Data.List (delete, find)

-- ==========================================================================
-- ID
-- --------------------------------------------------------------------------

newtype CharacterID = CharacterID {
    characterId :: Int
} deriving (Show, Eq, Ord)


newtype ItemID = ItemID {
      itemId     :: Int  -- ^ identify number.
} deriving (Show, Eq, Ord)

data ItemInf = ItemInf {
      itemID     :: ItemID
    , identified :: Bool -- ^ already identified or not.
} deriving (Show, Eq, Ord)


newtype EnemyID = EnemyID {
    enemyID :: Int
} deriving (Show, Eq, Ord)


newtype SpellID = SpellID {
    spellId :: Int -- ^ identify number.
} deriving (Show, Eq, Ord)


newtype GameEventID = GameEventID {
    mazeEventID :: Int
} deriving (Show, Eq, Ord)


newtype PictureID = PictureID {
    pictureID :: Int
} deriving (Show, Eq, Ord)


-- ==========================================================================
-- object
-- --------------------------------------------------------------------------

-- | type of status error.
data StatusError = Silence
                 | Paralysis
                 | Stoned
                 | Poison Int
                 | Fear
                 | Sleep
                 | Rigor
                 | Drain Int
                 | Dead
                 | Ash
                 | Lost
    deriving (Show, Eq)

-- | define of character's parameter.
data Parameter = Parameter {
      strength :: !Int -- ^ strength
    , iq       :: !Int -- ^ I.Q.
    , piety    :: !Int -- ^ piety
    , vitality :: !Int -- ^ vitality
    , agility  :: !Int -- ^ agility
    , luck     :: !Int -- ^ luck
} deriving (Show, Eq)

-- ==========================================================================

-- | position in party.
data PartyPos = F1 | F2 | F3 | B4 | B5 | B6
  deriving (Show, Ord, Eq)

toPartyPos :: HasCallStack => Int -> PartyPos
toPartyPos 1 = F1
toPartyPos 2 = F2
toPartyPos 3 = F3
toPartyPos 4 = B4
toPartyPos 5 = B5
toPartyPos 6 = B6
toPartyPos _ = error "invalid index of PartyPos"

partyPosToNum :: PartyPos -> Int
partyPosToNum F1 = 1
partyPosToNum F2 = 2
partyPosToNum F3 = 3
partyPosToNum B4 = 4
partyPosToNum B5 = 5
partyPosToNum B6 = 6

nextPartyPos :: PartyPos -> Maybe PartyPos
nextPartyPos F1 = Just F2
nextPartyPos F2 = Just F3
nextPartyPos F3 = Just B4
nextPartyPos B4 = Just B5
nextPartyPos B5 = Just B6
nextPartyPos B6 = Nothing

prevPartyPos :: PartyPos -> Maybe PartyPos
prevPartyPos F1 = Nothing
prevPartyPos F2 = Just F1
prevPartyPos F3 = Just F2
prevPartyPos B4 = Just F3
prevPartyPos B5 = Just B4
prevPartyPos B6 = Just B5


-- | position in enemy line groups.
data EnemyLine = L1 | L2 | L3 | L4
  deriving (Show, Ord, Eq)

toEnemyLine :: HasCallStack => Int -> EnemyLine
toEnemyLine 1 = L1
toEnemyLine 2 = L2
toEnemyLine 3 = L3
toEnemyLine 4 = L4
toEnemyLine _ = error "invalid index of EnemyLine"

enemyLineToNum :: EnemyLine -> Int
enemyLineToNum L1 = 1
enemyLineToNum L2 = 2
enemyLineToNum L3 = 3
enemyLineToNum L4 = 4

nextEnemyLine :: EnemyLine -> Maybe EnemyLine
nextEnemyLine L1 = Just L2
nextEnemyLine L2 = Just L3
nextEnemyLine L3 = Just L4
nextEnemyLine L4 = Nothing

prevEnemyLine :: EnemyLine -> Maybe EnemyLine
prevEnemyLine L1 = Nothing
prevEnemyLine L2 = Just L1
prevEnemyLine L3 = Just L2
prevEnemyLine L4 = Just L3


type SpellTarget = Either PartyPos EnemyLine

-- ==========================================================================

class Object o where
  nameOf          :: o -> String
  hpOf            :: o -> Int
  maxhpOf         :: o -> Int
  paramOf         :: o -> Parameter
  acOf            :: o -> Int
  lvOf            :: o -> Int
  statusErrorsOf  :: o -> [StatusError]  -- ^ status errors.

  setHp           :: Int -> o -> o
  setAc           :: Int -> o -> o
  setStatusErrors :: [StatusError] -> o -> o

addStatusError :: Object o => StatusError -> o -> o
addStatusError s o = setStatusErrors (s : statusErrorsOf o) o

removeStatusError :: Object o => StatusError -> o -> o
removeStatusError s o = setStatusErrors (delete s $ statusErrorsOf o) o

addPoison :: Object o => Int -> o -> o
addPoison d s = let ss = statusErrorsOf s in
    case find isPoison ss of Just (Poison n) -> addStatusError (Poison $ n + d) . removeStatusError (Poison n) $ s
                             _               -> addStatusError (Poison d) s
  where
    isPoison (Poison _) = True
    isPoison _          = False
        
damageHp :: Object o => Int -> o -> o
damageHp dmg s = setHp (hpOf s - dmg) s




whenReturnCastle :: Object o => StatusError -> o -> o
whenReturnCastle (Poison n) o = setStatusErrors (filter (/= Poison n) $ statusErrorsOf o) o
whenReturnCastle _ o = o

whenToNextTurn :: Object o => Int -> StatusError -> o -> o
whenToNextTurn _ (Poison n) o = setHp (hpOf o - n) o
whenToNextTurn n (Sleep   ) o = if n < 50 then o else whenBattleEnd Sleep o
whenToNextTurn _ _ o = o

whenWalking :: Object o => StatusError -> o -> o
whenWalking (Poison n) o = setHp (hpOf o - n) o
whenWalking _ o = o

whenBattleEnd :: Object o => StatusError -> o -> o
whenBattleEnd Silence o = setStatusErrors (filter (/= Silence) $ statusErrorsOf o) o
whenBattleEnd Fear    o = setStatusErrors (filter (/= Fear) $ statusErrorsOf o) o
whenBattleEnd Sleep   o = setStatusErrors (filter (/= Sleep) $ statusErrorsOf o) o
whenBattleEnd Rigor   o = setStatusErrors (filter (/= Rigor) $ statusErrorsOf o) o
whenBattleEnd _ o = o

hasStatusError :: Object o => o -> StatusError -> Bool
hasStatusError o = (`elem` statusErrorsOf o)

isCantFight :: Object o => o -> Bool
isCantFight o = any (hasStatusError o) cantFightStatus

cantFightStatus :: [StatusError]
cantFightStatus = [ Paralysis
                  , Stoned
                  , Fear
                  , Sleep
                  , Rigor
                  , Dead
                  , Ash
                  , Lost]

cantSpellStatus :: [StatusError]
cantSpellStatus = [ Silence
                  , Paralysis
                  , Stoned
                  , Fear
                  , Sleep
                  , Rigor
                  , Dead
                  , Ash
                  , Lost]

