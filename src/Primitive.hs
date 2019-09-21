module Primitive
where



-- | type of status error.
data StatusError = Silence
                 | Paralysis
                 | Stoned
                 | Poison
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
  lvOf            :: o -> lv
  statusErrorsOf  :: o -> [StatusError]  -- ^ status errors.

  setHp           :: o -> Int -> o
  setParam        :: o -> Parameter -> o
  setAc           :: o -> Int -> o
  setStatusErrors :: o -> [StatusError] -> o

