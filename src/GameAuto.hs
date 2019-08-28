{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameAuto
where

import Control.Monad
import Control.Monad.Trans.State


data Input = Key String
           | Clock
           | Abort
    deriving (Show, Eq)

data Event = None
           | Message String
           | And Event Event
    deriving (Show, Eq)


newtype Auto i o = Auto (o, i -> Auto i o)

instance Functor (Auto i) where
  fmap f (Auto (o, next)) = Auto (f o, fmap f . next)


newtype GameAuto w i e = GameAuto { run :: State w (e, i -> GameAuto w i e) }
--  deriving (Functor, Applicative, Monad)

    
runGame :: (e -> w -> IO a) -> IO i -> (GameAuto w i e, w) -> IO ()
runGame render cmd (game, w) = do
    let ((e, next), w') = runState (run game) w
    render e w'
    i <- cmd
    runGame render cmd (next i, w')

events :: [e] -> GameAuto w i e -> GameAuto w i e
events [] l     = l
events (e:es) l = GameAuto $ return (e, \_ -> events es l)


selectWhen :: Eq i => e -> [(i, GameAuto w i e, Bool)] -> State w (e, i -> GameAuto w i e)
selectWhen e ns = return (e, select' ns)
  where
    select' ((i1, s1, enable):ns) i = if i == i1 && enable then s1 else select' ns i
    select' [] _ = GameAuto $ selectWhen e ns

select :: Eq i => e -> [(i, GameAuto w i e)] -> State w (e, i -> GameAuto w i e)
select e ns = selectWhen e $ map (\(i, g) -> (i, g, True)) ns 

selectNext :: Eq i => e -> [(i, GameAuto w i e)] -> GameAuto w i e
selectNext e ns = GameAuto $ select e ns



