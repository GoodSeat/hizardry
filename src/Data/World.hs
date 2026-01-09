module Data.World
where

import PreludeL
import System.Random (StdGen, mkStdGen, randomIO)
import qualified Data.Map as Map
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)
import Data.List (isPrefixOf, isSuffixOf, dropWhileEnd, groupBy)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)

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
    , partyLight'     :: !Int     -- ^ last time super milwa effect.(delete dark zone)
    , partyParamDelta :: ![(Term, ParamChange)]

    , visitHitory     :: !(Map.Map Coord Bool)

    , inTavernMember  :: ![CharacterID]
    , inMazeMember    :: ![(CharacterID, Position)]
    , shopItems       :: !(Map.Map ItemID Int)

    , allCharacters   :: !Character.DB
    , globalTime      :: !Int

    , sceneTrans      :: Filter
    , enemyTrans      :: Filter
    , frameTrans      :: Filter
    , eventFlags      :: [Int] -- ^ global flag for event.

    , debugMode       :: !Bool
    , debugMessage    :: ![String]

    , backUpSlotInfo  :: ![String]
} -- deriving (Show)

data InitWorld = InitWorld {
      initGuideOn         :: !Bool   -- ^ visible guidw window in maze.
    , initStatusOn        :: !Bool   -- ^ visible status window in maze.
    , initWorldOption     :: !WorldOption

    , initParty           :: ![CharacterID]
    , initPlace           :: !Place   -- ^ current party position.
    , initRoomBattled     :: ![Coord] -- ^ already room battled in current mazing.
    , initPartyLight      :: !Int     -- ^ last time milwa effect.
    , initPartyLight'     :: !Int     -- ^ last time super milwa effect.(delete dark zone)
    , initPartyParamDelta :: ![(Term, ParamChange)]

    , initInTavernMember  :: ![CharacterID]
    , initInMazeMember    :: ![(CharacterID, Position)]
    , initShopItems       :: !(Map.Map ItemID Int)

    , initAllCharacters   :: !Character.DB
} deriving (Show)

initWorld :: InitWorld -> Seed -> Bool -> World
initWorld i seed debugMode = World {
      randomGen       = mkStdGen seed
    , guideOn         = initGuideOn         i
    , statusOn        = initStatusOn        i
    , worldOption     = initWorldOption     i

    , party           = initParty           i
    , place           = initPlace           i
    , roomBattled     = initRoomBattled     i
    , partyLight      = initPartyLight      i
    , partyLight'     = initPartyLight'     i
    , partyParamDelta = initPartyParamDelta i

    , visitHitory     = Map.empty

    , inTavernMember  = initInTavernMember  i
    , inMazeMember    = initInMazeMember    i
    , shopItems       = initShopItems       i

    , allCharacters   = initAllCharacters   i
    , globalTime      = 0

    , sceneTrans      = id
    , enemyTrans      = id
    , frameTrans      = id
    , eventFlags      = repeat 0

    , debugMode       = debugMode
    , debugMessage    = []
    , backUpSlotInfo  = []
}

data Place  = InCastle
            | Gilgamesh'sTavern 
            | Adventure'sInn
            | Boltac'sTradingPost 
            | TempleOfCant 
            | InEdgeOfTown
            | TrainingGrounds
            | EnteringMaze
            | TotalAnnihilation
            | InMaze            Position
            | InBattle          Position [[Enemy.Instance]]
            | FindTreasureChest Position Bool -- ^ chest is opend.
            | Camping           Position String -- ^ title
    deriving (Show, Eq, Read)

data MiniMapType = Disable | Normal | AlwaysN deriving (Show, Eq, Read)

data HPHealType = Classic
                | CureWhenInn          -- ^ full cured when sleep at Inn.
                | CureWhenReturnCastle -- ^ full cured when return castle.
    deriving (Eq, Show, Read)

data WorldOption = WorldOption {
      effectDumapic    :: !Spell.CheckLocationType
    , minimapType      :: !MiniMapType
    , hpHealType       :: !HPHealType  -- TODO
    , ignoreAlignment  :: !Bool        -- TODO
    , switchSE         :: !Bool
    , switchBGM        :: !Bool
    , waitTimeInBattle :: !Int  -- ^ wait time in battle message (ms). 0 means infinity.
    } deriving (Eq, Show, Read)

defaultWorldOption :: WorldOption
defaultWorldOption = WorldOption {
      effectDumapic    = Spell.ViewMap
    , minimapType      = Normal
    , hpHealType       = Classic
    , ignoreAlignment  = False
    , switchSE         = True
    , switchBGM        = True
    , waitTimeInBattle = 1000
    }

type Seed = Int

-- | Explicit saving world.
--   belows contents are not save target.
--     * sceneTrans (always restore as "id")
--     * enemyTrans (always restore as "id")
--     * frameTrans (always restore as "id")
--   and when classic mode, belows contents also not target.
--     * party        (always [])
--     * place        (always InCastle)
--     * roomBattled  (always [])
--     * partyLight   (always 0)
--
--    NOTE:
--     * when saving "randomGen", save random int(with random by getStdGen), and replace randomGen by made StdGen using mkStdGen it.
--     * when loading "randomGen", restore by "mkStdGen :: Int -> RandomGen")
--    TODO:save as json for enable to load data of different version.
saveWorld :: World -> FilePath -> IO (World, Seed)
saveWorld w path = do
    r <- randomIO
    writeFile path (txt r)
    return (w { randomGen = mkStdGen r }, r)
  where
    txt :: Int -> String
    txt r = unlines [
        "### randomGen ###"
      , show r

      , "### guideOn ###"
      , show $ guideOn         w
      , "### statusOn ###"
      , show $ statusOn        w
      , "### worldOption ###"
      , show $ worldOption     w

      , "### party ###"
      , show $ party           w
      , "### place ###"
      , show $ place           w
      , "### roomBattled ###"
      , show $ roomBattled     w
      , "### partyLight ###"
      , show $ partyLight      w
      , "### partyLight' ###"
      , show $ partyLight'     w
      , "### partyParamDelta ###"
      , show $ partyParamDelta w

      , "### visitHitory ###"
      , show $ visitHitory     w

      , "### inTavernMember ###"
      , show $ inTavernMember  w
      , "### inMazeMember ###"
      , show $ inMazeMember    w
      , "### shopItems ###"
      , show $ shopItems       w

      , "### allCharacters ###"
      , show $ allCharacters   w
      , "### globalTime ###"
      , show $ globalTime      w

      , "### eventFlags ###"
      , show $ take 100000 (eventFlags w)

      , "### debugMode ###"
      , show $ debugMode       w
      ]


loadWorld :: FilePath -> IO (Either String (World, Seed))
loadWorld path = do
    contentResult <- tryIOError (readFile path)
    case contentResult of
        Left  e -> return $ Left ("Failed to read save file: " ++ show e)
        Right c -> return $ buildWorld (lines c)

buildWorld :: [String] -> Either String (World, Seed)
buildWorld ls = do
    let sections = parseSections ls
    
    rGenInt      <- readSection sections "randomGen"
    guide        <- readSection sections "guideOn"
    status       <- readSection sections "statusOn"
    option       <- readSection sections "worldOption"
    pParty       <- readSection sections "party"
    pPlace       <- readSection sections "place"
    pRoomBattled <- readSection sections "roomBattled"
    pLight       <- readSection sections "partyLight"
    pLight'      <- readSection sections "partyLight'"
    pDelta       <- readSection sections "partyParamDelta"
    pVisit       <- readSection sections "visitHitory"
    pTavern      <- readSection sections "inTavernMember"
    pMaze        <- readSection sections "inMazeMember"
    pShop        <- readSection sections "shopItems"
    pChars       <- readSection sections "allCharacters"
    gTime        <- readSection sections "globalTime"
    pFlags       <- readSection sections "eventFlags"
    pDebug       <- readSection sections "debugMode"

    return (World {
        randomGen       = mkStdGen rGenInt,
        guideOn         = guide,
        statusOn        = status,
        worldOption     = option,
        party           = pParty,
        place           = pPlace,
        roomBattled     = pRoomBattled,
        partyLight      = pLight,
        partyLight'     = pLight',
        partyParamDelta = pDelta,
        visitHitory     = pVisit,
        inTavernMember  = pTavern,
        inMazeMember    = pMaze,
        shopItems       = pShop,
        allCharacters   = pChars,
        globalTime      = gTime,
        eventFlags      = pFlags,
        debugMode       = pDebug,
        sceneTrans      = id,
        enemyTrans      = id,
        frameTrans      = id,
        debugMessage    = [],
        backUpSlotInfo  = []  -- MEMO:this is auto created in playing.
    }, rGenInt)

parseSections :: [String] -> Map.Map String String
parseSections ls = Map.fromList $ mapMaybe processGroup (groupBy (\_ b -> not $ isHeader b) ls)
  where
    isHeader :: String -> Bool
    isHeader l = "### " `isPrefixOf` l && " ###" `isSuffixOf` l
    processGroup :: [String] -> Maybe (String, String)
    processGroup [] = Nothing
    processGroup (header:content) =
      if isHeader header then 
        let sectionName = take (length header - 8) $ drop 4 header
        in Just (sectionName, unlines content)
      else Nothing

readSection :: Read a => Map.Map String String -> String -> Either String a
readSection sections key = do
    contentStr <- Map.lookup key sections `orError` ("Save data corrupted: missing section " ++ key)
    readMaybe (trim contentStr) `orError` ("Save data corrupted: cannot parse section " ++ key)
  where
    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace
    orError :: Maybe a -> String -> Either String a
    orError Nothing  msg = Left msg
    orError (Just a) _   = Right a


