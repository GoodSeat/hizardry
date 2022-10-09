module Primitive
where



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


class Object o where
  hpOf            :: o -> Int
  maxhpOf         :: o -> Int
  paramOf         :: o -> Parameter
  acOf            :: o -> Int
  lvOf            :: o -> Int
  statusErrorsOf  :: o -> [StatusError]  -- ^ status errors.

  setHp           :: Int -> o -> o
  setAc           :: Int -> o -> o
  setStatusErrors :: [StatusError] -> o -> o


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

