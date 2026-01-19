module Main where

import PreludeL
import Data.Version (showVersion, versionBranch)
import Paths_hizardry (version)

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, listDirectory)
import System.Console.ANSI (clearScreen, clearLine, hideCursor, showCursor, setCursorPosition, cursorUp)
import System.Random (randomIO)
import Control.Exception (try, SomeException(..), bracket_, finally)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void, when, forM)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Data.List (isSuffixOf)
import Data.Char (ord, chr)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Text.Read (readMaybe)
import qualified Data.Bits as Bits

import Engine.GameAuto
import Engine.InCastle (inCastle)
import Engine.InEdgeOfTown (inEdgeOfTown)
import Data.World (World(..), Place(..), place, saveWorld, loadWorld, initWorld, Seed)
import Data.PlayEvent

import Control.CUI
import UI.CuiRender (cuiRender, renderWithCache)
import UI.SoundControl

import qualified SampleScenario.Home as SampleScenario


-- note
-- * classic secret door
-- * other spells
-- * other events
-- * make backup data / load backup data
-- * reset data(=delete auto save data, and reload).
--
-- * item encyclopedia
-- * enemy encyclopedia

-- * scenario parser, save data parser.
-- *   hashable-1.4.1.0 [Data.Hashable] hash:: a -> Int
-- *   zip compression with secret keyword. using another exe? deflate?

inputLogPath = "rtsd.iks" -- path of "real time save data(input keys)"
backupDirectory = "savedata"

crypt :: IORef Int -> String -> String -> IO String
crypt indx key text = if null key then return text else do
    n <- readIORef indx
    modifyIORef indx (+ length text)
    return $ crypt' (drop n $ cycle key) text

crypt' :: String -> String -> String
crypt' key text = zipWith (\c k -> chr $ ord c `Bits.xor` ord k) text (cycle key)

main :: IO ()
main = bracket_ initSound quitSound $ do
--  let seed0 = 0
    seed0 <- randomIO

    (is, iw) <- SampleScenario.initScenario
    let s' = initScenario is inCastle 
    let w0 = initWorld iw seed0 True
        s  = SampleScenario.modScenario s' -- TODO:WIP

    let currentVersion  = versionBranch version -- if isn't match with major/minor/build version, invalid save data.
        currentVersionS = scenarioVersion s     -- if isn't match with major/minor/build version, invalid save data.

    -- restore from save data.
    infData0 <- findData s 0
    (w, seed) <- case infData0 of
        Nothing        -> return (w0, seed0)
        Just (path, _) -> do
            res <- loadWorld path
            case res of Right ws -> return ws
                        Left  _  -> return (w0, seed0)

    -- restore from input log.
    existInputLog <- doesFileExist inputLogPath
    indx <- newIORef 0
    let ekey = encKey s
--  let ekey = ""
    
    (run, needResetInputLog) <- if not existInputLog then return (runGame, True) else do
        c  <- readFile inputLogPath
        ls <- lines <$> crypt indx ekey c
        if length ls > 3 &&
           (take 3 <$> (readMaybe (ls !! 0) :: Maybe [Int])) == Just (take 3 currentVersion)  &&  -- 1:check app version.
           (take 3 <$> (readMaybe (ls !! 1) :: Maybe [Int])) == Just (take 3 currentVersionS) &&  -- 2:check scenario version.
           (readMaybe (ls !! 2) :: Maybe Int) == Just seed                                        -- 3:check if seed is match.
        then do
          let is  = read <$> filter (not . null) (drop 3 ls)
              is' = foldl (\acc i -> if i == Abort then tail acc else i:acc) [] is
          return (loadGame (reverse is'), False)
        else
          return (runGame, True)

    let resetInputLog sd = writeIORef indx 0 >> (writeFile inputLogPath =<< crypt indx ekey (
                            show currentVersion  ++ "\n"    -- 1:save app version.
                         ++ show currentVersionS ++ "\n"    -- 2:save scenario version.
                         ++ show sd ++ "\n"                 -- 3:save seed for check data.
                        ))

    when needResetInputLog (resetInputLog seed)

    let start | place w == InEdgeOfTown = inEdgeOfTown
              | otherwise               = inCastle
             
    -- setting for CUI
    cacheSound <- newIORef (EnteringMaze, "")
    let picOf = maybe mempty SampleScenario.pic
        seOf  = SampleScenario.seOf
        bgmOf = SampleScenario.bgmOf
    drawCache <- newDrawCache
    let renderMethod = renderWithCache drawCache
        display      = cuiRender renderMethod picOf s
        display' e w = playSound cacheSound seOf bgmOf e w
                    >> setCursorPosition 0 0 >> display e w
    let cmd          = getKey indx ekey (clearCache drawCache)

    clearScreen
    hideCursor
    w' <- run display' cmd updateBackUpList (savingGame resetInputLog) (loadingGame resetInputLog) s w start
    showCursor

    appendFile inputLogPath =<< crypt indx ekey (show Abort ++ "\n")

-- ==========================================================================

nameOfData :: String -> Scenario -> Int -> String
nameOfData tag s slot = tag ++ "_" ++ scenarioName s ++ "." ++ show slot

pathOfData :: String -> Scenario -> Int -> FilePath
pathOfData tag s slot = backupDirectory ++ "/" ++ nameOfData tag s slot

findData :: Scenario -> Int -> IO (Maybe (FilePath, String))
findData s slot = do
    createDirectoryIfMissing True backupDirectory
    ls <- listDirectory backupDirectory
    let ns = filter (isSuffixOf $ nameOfData "" s slot) ls
    return $ if null ns then Nothing else let n = head ns in
        Just ( backupDirectory ++ "/" ++ n, take (length n - length (nameOfData "" s slot)) n)

updateBackUpList :: UpdateBackUpList
updateBackUpList s = do
    createDirectoryIfMissing True backupDirectory
    forM [1..9] $ \slot -> do
        dat <- findData s slot
        return $ case dat of Nothing     -> ""
                             Just (_, n) -> n

savingGame :: (Seed -> IO ()) -> SavingGame
savingGame resetInputLog slot tag s w = do
    dat <- findData s slot
    case dat of Nothing        -> return ()
                Just (path, _) -> removeFile path
    (w', sed) <- saveWorld w (pathOfData tag s slot)
    resetInputLog sed
    return (Just w')
  where tag' = if null tag then "data" ++ show slot else tag

loadingGame :: (Seed -> IO ()) -> LoadingGame
loadingGame resetInputLog slot s = do
    dat <- findData s slot
    case dat of
      Nothing -> return Nothing
      Just (path, _) -> do
        res <- loadWorld path
        case res of Right (w, sed) -> resetInputLog sed >> return (Just w)
                    Left  _        -> return Nothing

-- ==========================================================================

getKey :: IORef Int -> String -> IO () -> InputIO
getKey indx encKey refresh itype = do
    i <- getKey' itype
    appendFile inputLogPath =<< crypt indx encKey (show i ++ "\n")
    return i
  where
    getKey' SingleKey = do
        hSetBuffering stdin NoBuffering
        x <- getChar
        when (x == '\ESC') refresh
        return $ Key [x]
    getKey' SequenceKey = do
        hSetBuffering stdin LineBuffering
        showCursor
        let mod s = let s' = filter (/= '\n') . filter (/= '\r') $ s in if s' == "" then "\n" else s'
        (Key . mod <$> getLine) <* (cursorUp 1 >> clearLine >> hideCursor >> refresh)
    getKey' (WaitClock n)
      | n > 0     = race (threadDelay $ n * 1000) ignoreKey >> return Clock
      | otherwise = do
          x <- race (threadDelay $ n * (-1000)) waitKey
          return $ case x of Left  _ -> Clock
                             Right c -> Key [c]

waitKey :: IO Char
waitKey = do
    hSetBuffering stdin NoBuffering
    buf <- hReady stdin
    if buf then getChar
           else threadDelay 50000 >> waitKey

ignoreKey :: IO ()
ignoreKey = do
    hSetBuffering stdin NoBuffering
    buf <- hReady stdin
    when buf $ void getChar
    threadDelay 50000 >> ignoreKey

-- ==========================================================================



-- ==========================================================================
