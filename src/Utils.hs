module Utils
where

import Control.Monad.State
import Control.Monad.Reader

import GameAuto

import World
import qualified Characters as Chara
import System.Random



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


