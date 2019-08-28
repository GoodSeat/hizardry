module World
where

import System.Random
import qualified Data.Map as Map
import Control.Monad.Trans.State
import qualified Characters as Chara
import Maze
import Items

-- | test function.
main' :: IO ()
main' = do
    gen <- getStdGen
    let (r1, g') = next gen
    print r1


data World = World {
      randomGen       :: StdGen

    , party           :: [Chara.Character]
    , place           :: Place

    , inTarvernMember :: [Chara.Character]
    , inMazeMember    :: [(Chara.Character, Position)]
    , shopItems       :: Map.Map Item Int
} deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern (Maybe Chara.Character)
            | Adventure'sInn
            | Boltac'sTradingPost (Maybe Chara.Character)
            | TempleOfCant (Maybe Chara.Character)
            | EdgeOfTown
            | TrainingGrounds
            | InMaze Position
    deriving (Show, Eq)


movePlace :: Place -> State World ()
movePlace p = do
    w <- get
    put w { place = p }


toParty :: Chara.Character -> State World ()
toParty c = do
    w <- get
    let w' = w { party           = party w ++ [c]
               , inTarvernMember = filter (/= c) $ inTarvernMember w
               , inMazeMember    = filter (\(c', _) -> c' /= c) $ inMazeMember w
               }
    put w'

updateCharacter :: Chara.Character -> State World ()
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

randomNext :: Int -> Int -> State World Int
randomNext min max = do
    w <- get
    let (v, g') = randomR (min, max) $ randomGen w
    put w { randomGen = g' }
    return v

