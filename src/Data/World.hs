module Data.World
where

import System.Random
import qualified Data.Map as Map

import Data.Primitive
import qualified Data.Characters as Character
import qualified Data.Enemies as Enemy
import Data.Maze

import Control.CUI (Filter)


data World = World {
      randomGen       :: StdGen
    , guideOn         :: !Bool   -- ^ visible guidw window in maze.
    , statusOn        :: !Bool   -- ^ visible status window in maze.

    , party           :: ![CharacterID]
    , place           :: !Place   -- ^ current party position.
    , roomBattled     :: ![Coord] -- ^ already room battled in current mazing.
    , visitHitory     :: !(Map.Map Coord Bool)

    , inTarvernMember :: ![CharacterID]
    , inMazeMember    :: ![(CharacterID, Position)]
    , shopItems       :: !(Map.Map ItemID Int)

    , allCharacters   :: !Character.DB

    , sceneTrans      :: Filter
} -- deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern 
            | Adventure'sInn
            | Boltac'sTradingPost 
            | TempleOfCant 
            | InEdgeOfTown
            | TrainingGrounds
            | InMaze            Position
            | InBattle          Position [[Enemy.Instance]]
            | FindTreasureChest Position Bool -- ^ chest is opend.
            | Camping           Position
    deriving (Show, Eq)



-- TODO!:explicit saving.
--       belows contents are not save target.
--         * sceneTrans (always restore as "id")
--       and when classic mode, belows contents also not target.
--         * party        (always [])
--         * place        (always InCastle)
--         * roomBattled  (always [])
--
--        NOTE:
--         * when saving "randomGen", save rondom int.
--         * when loading "randomGen", restore by "mkStdGen :: Int -> RandomGen")
saveWorld :: World -> FilePath -> IO ()
saveWorld = undefined


loadWorld :: FilePath -> IO (Either String World)
loadWorld = undefined

