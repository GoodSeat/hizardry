module World
where

import System.Random
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Characters
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

    , party           :: [Character]
    , place           :: Place

    , inTarvernMember :: [Character]
    , inMazeMember    :: [(Character, Position)]
    , shopItems       :: Map.Map Item Integer
} deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern (Maybe Character)
            | Adventure'sInn (Maybe Character)
            | Boltac'sTradingPost (Maybe Character)
            | TempleOfCant (Maybe Character)
            | EdgeOfTown
            | TrainingGrounds
            | InMaze Position
    deriving (Show, Eq)


movePlace :: Place -> State World ()
movePlace p = do
    w <- get
    put w { place = p }


toParty :: Character -> State World ()
toParty c = do
    w <- get
    let w' = w { party           = party w ++ [c]
               , inTarvernMember = filter (/= c) $ inTarvernMember w
               , inMazeMember    = filter (\(c', _) -> c' /= c) $ inMazeMember w
               }
    put w'
