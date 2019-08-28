{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameAuto
where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import World
import qualified Characters as Chara
import System.Random

newtype GameState o = GameState { runS :: StateT World (Reader Option) o }
    deriving (Functor, Applicative, Monad, MonadReader Option, MonadState World)

newtype Auto w o i e = Auto { run :: GameState (e, i -> Auto w o i e) }


data Input = Key String
           | Clock
           | Abort
    deriving (Show, Eq)

data Event = None
           | Exit
           | Message String
           | And Event Event
    deriving (Show, Eq)

data Option = Option String

type GameAuto    = Auto World Option Input Event
type GameMachine = GameState (Event, Input -> GameAuto)
    
runGame :: (Event -> World -> IO a) -> IO Input -> (GameAuto, World, Option) -> IO ()
runGame render cmd (game, w, o) = do
    let r = runStateT (runS $ run game) w
    let ((e, next), w') = runReader r o
    if e == Exit then return ()
    else do
        render e w'
        i <- cmd
        runGame render cmd (next i, w', o)

events :: [Event] -> GameAuto -> GameAuto
events [] l     = l
events (e:es) l = Auto $ return (e, \_ -> events es l)


selectWhen :: Event -> [(Input, GameAuto, Bool)] -> GameMachine
selectWhen e ns = return (e, select' ns)
  where
    select' ((i1, s1, enable):ns) i = if i == i1 && enable then s1 else select' ns i
    select' [] _ = Auto $ selectWhen e ns

select :: Event -> [(Input, GameAuto)] -> GameMachine
select e ns = selectWhen e $ map (\(i, g) -> (i, g, True)) ns 

selectNext :: Event -> [(Input, GameAuto)] -> GameAuto
selectNext e ns = Auto $ select e ns



getOption :: GameState String
getOption = do
  Option t <- ask
  return t


movePlace :: Place -> GameState ()
movePlace p = do
    w <- get
    put w { place = p }


toParty :: Chara.Character -> GameState ()
toParty c = do
    w <- get
    let w' = w { party           = party w ++ [c]
               , inTarvernMember = filter (/= c) $ inTarvernMember w
               , inMazeMember    = filter (\(c', _) -> c' /= c) $ inMazeMember w
               }
    put w'

updateCharacter :: Chara.Character -> GameState ()
updateCharacter c = do
    w <- get
    let w' = w { party           = replaceWith c <$> party w
               , inTarvernMember = replaceWith c <$> inTarvernMember w
               , inMazeMember    = replaceWith' c <$> inMazeMember w
               }
    put w'
  where
    replaceWith c c' = if Chara.name c /= Chara.name c' then c' else c
    replaceWith' c (c', p) = (replaceWith c c', p)

randomNext :: Int -> Int -> GameState Int
randomNext min max = do
    w <- get
    let (v, g') = randomR (min, max) $ randomGen w
    put w { randomGen = g' }
    return v

