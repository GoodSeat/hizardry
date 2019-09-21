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
