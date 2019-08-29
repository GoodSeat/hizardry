module Utils
where

import Control.Monad.State
import Control.Monad.Reader

import Data.Map hiding (filter)
import System.Random

import GameAuto
import World
import qualified Characters as Character


movePlace :: Place -> GameState ()
movePlace p = do
    w <- world
    put w { place = p }

-- =================================================================================

characterOf :: Character.ID -> GameState Character.Character
characterOf id = do
    db <- allCharacters <$> get
    return $ db ! id

toParty :: Character.ID -> GameState ()
toParty id = do
    w <- get
    let w' = w { party           = party w ++ [id]
               , inTarvernMember = filter (/= id) $ inTarvernMember w
               , inMazeMember    = filter (\(id', _) -> id' /= id) $ inMazeMember w
               }
    put w'

updateCharacter :: Character.ID -> Character.Character -> GameState ()
updateCharacter id c = do
    w  <- get
    let db = allCharacters w
        w' = w { allCharacters = insert id c db }
    put w'

updateCharacterWith :: Character.ID -> (Character.Character -> Character.Character) -> GameState ()
updateCharacterWith id f = do
    db <- allCharacters <$> get
    updateCharacter id (f $ db ! id)

-- =================================================================================

randomNext :: Int -> Int -> GameState Int
randomNext min max = do
    w <- get
    let (v, g') = randomR (min, max) $ randomGen w
    put w { randomGen = g' }
    return v


