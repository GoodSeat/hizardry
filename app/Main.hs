module Main where

import PreludeL
import Data.Version (showVersion, versionBranch)
import Paths_hizardry (version)

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, listDirectory)
import System.Console.ANSI (clearScreen, clearLine, hideCursor, showCursor, setCursorPosition, cursorUp)
import System.Random
import Control.Exception (try, SomeException(..), bracket_, finally)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void, when, forM)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Data.List (isSuffixOf)
import Data.Char (ord, chr)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import qualified Data.Bits as Bits

import Engine.GameAuto
import Engine.InCastle (inCastle)
import Engine.InEdgeOfTown (inEdgeOfTown)
import Data.World (World(..), Place(..), place, saveWorld, loadWorld, initWorld)
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
--
-- * config (worldOption)
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
    let currentVersion = versionBranch version -- if isn't match with major/minor/build version, invalid save data.
    --gen <- getStdGen
    let gen = mkStdGen 0 
    (is, iw) <- SampleScenario.initScenario
    let s' = initScenario is inCastle 
    let w1 = initWorld iw gen True
        s  = SampleScenario.modScenario s' -- TODO:WIP

    -- restore from save data.
    infData0 <- findData s 0
    w <- case infData0 of
        Nothing        -> return w1
        Just (path, _) -> do
            res <- loadWorld path
            case res of Right w -> return w
                        Left  _ -> return w1

    -- restore from input log.
    existInputLog <- doesFileExist inputLogPath
    indx <- newIORef 0
    let ekey = encKey s
--  let ekey = ""
    
    (run, needResetInputLog) <- if not existInputLog then return (runGame, True) else do
        c  <- readFile inputLogPath
        ls <- lines <$> crypt indx ekey c
        if length ls > 1 && take 3 (read $ head ls :: [Int]) == take 3 currentVersion then do
          let is  = read <$> filter (not . null) (tail ls)
              is' = foldl (\acc i -> if i == Abort then tail acc else i:acc) [] is
          return (loadGame (reverse is'), False)
        else
          return (runGame, True)

    let resetInputLog = writeIORef indx 0 >> (writeFile inputLogPath =<< crypt indx ekey (show currentVersion ++ "\n"))

    when needResetInputLog resetInputLog

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
    void $ saveWorld w' "world.dat"

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

savingGame :: IO () -> SavingGame
savingGame resetInputLog slot tag s w = do
    dat <- findData s slot
    case dat of Nothing        -> return ()
                Just (path, _) -> removeFile path
    resetInputLog >> (Just <$> saveWorld w (pathOfData tag s slot))
  where tag' = if null tag then "data" ++ show slot else tag

loadingGame :: IO () -> LoadingGame
loadingGame resetInputLog slot s = do
    dat <- findData s slot
    case dat of
      Nothing -> return Nothing
      Just (path, _) -> do
        res <- loadWorld path
        case res of Right w -> resetInputLog >> return (Just w)
                    Left  _ -> return Nothing

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
