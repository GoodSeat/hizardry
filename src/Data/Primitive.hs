module Data.Primitive
where

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
    , determined :: Bool -- ^ already determined or not.
} deriving (Show, Eq, Ord)


newtype EnemyID = EnemyID {
    enemyID :: Int
} deriving (Show, Eq, Ord)


newtype SpellID = SpellID {
    spellId :: Int -- ^ identify number.
} deriving (Show, Eq, Ord)


newtype PictureID = PictureID {
    num :: Int
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


whenToNextCastle :: Object o => StatusError -> o -> o
whenToNextCastle (Poison n) o = setStatusErrors (filter (/= Poison n) $ statusErrorsOf o) o
whenToNextCastle _ o = o

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

