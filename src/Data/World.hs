module Data.World
where

import System.Random
import qualified Data.Map as Map

import Data.Primitive
import Data.Maze
import qualified Data.Characters as Character
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell

import Control.CUI (Filter)


data World = World {
      randomGen       :: StdGen
    , guideOn         :: !Bool   -- ^ visible guidw window in maze.
    , statusOn        :: !Bool   -- ^ visible status window in maze.
    , worldOption     :: !WorldOption

    , party           :: ![CharacterID]
    , place           :: !Place   -- ^ current party position.
    , roomBattled     :: ![Coord] -- ^ already room battled in current mazing.
    , partyLight      :: !Int     -- ^ last time milwa effect.
    , partyLight'     :: !Int     -- ^ last time super milwa effect.(ignore dark zone)

    , visitHitory     :: !(Map.Map Coord Bool)

    , inTarvernMember :: ![CharacterID]
    , inMazeMember    :: ![(CharacterID, Position)]
    , shopItems       :: !(Map.Map ItemID Int)

    , allCharacters   :: !Character.DB

    , sceneTrans      :: Filter
    , eventFlags      :: [Int] -- ^ global flag for event.

    , debugMode       :: !Bool
    , debugMessage    :: ![String]
} -- deriving (Show)

data Place  = InCastle
            | Gilgamesh'sTarvern 
            | Adventure'sInn
            | Boltac'sTradingPost 
            | TempleOfCant 
            | InEdgeOfTown
            | TrainingGrounds
            | EnteringMaze
            | InMaze            Position
            | InBattle          Position [[Enemy.Instance]]
            | FindTreasureChest Position Bool -- ^ chest is opend.
            | Camping           Position
    deriving (Show, Eq)

data MiniMapType = Disable | Normal | AlwaysN deriving (Show, Eq, Read)
      
data WorldOption = WorldOption {
      effectDumapic :: !Spell.CheckLocationType
    , minimapType   :: !MiniMapType
    }


-- TODO!:explicit saving(only in Edge of Town, or Castle. Auto?).
--       belows contents are not save target.
--         * sceneTrans (always restore as "id")
--       and when classic mode, belows contents also not target.
--         * party        (always [])
--         * place        (always InCastle)
--         * roomBattled  (always [])
--         * partyLight   (always 0)
--
--        NOTE:
--         * when saving "randomGen", save random int(with random by getStdGen), and replace randomGen by made StdGen using mkStdGen it.
--         * when loading "randomGen", restore by "mkStdGen :: Int -> RandomGen")
saveWorld :: World -> FilePath -> IO ()
saveWorld = undefined


loadWorld :: FilePath -> IO (Either String World)
loadWorld = undefined

