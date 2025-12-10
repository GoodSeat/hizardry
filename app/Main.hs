module Main where

import Data.Version (showVersion, versionBranch)
import Paths_hizardry (version)

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Console.ANSI (clearScreen, clearLine, hideCursor, showCursor, setCursorPosition, cursorUp)
import System.Directory
import System.Random
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void, when)
import Data.Char (ord, chr)
import qualified Data.Bits as Bits

import Engine.GameAuto
import Engine.InCastle
import Engine.Utils
import Data.Primitive
import Data.Characters (ItemPos(..))
import Data.World
import Data.Formula
import Data.Maze
import Data.IORef
import qualified Data.Enemies as Enemy

import Control.CUI
import UI.CuiRender (cuiRender, renderWithCache)

import qualified SampleScenario.Home as SampleScenario


-- note
-- * game over
-- * classic secret door
-- * other spells
-- * other events
-- * make backup data / load backup data
--
-- * config
-- *   all cure when sleep / when return castle(-> delete INN)
--
-- * item encyclopedia
-- * enemy encyclopedia

-- * scenario parser, save data parser.
-- *   hashable-1.4.1.0 [Data.Hashable] hash:: a -> Int
-- *   zip compression with secret keyword. using another exe? deflate?

encKey :: String
encKey = "hizardry-secret-key"

saveDataPath = "rtsd.iks" -- path of "real time save data(input keys)"

crypt :: IORef Int -> String -> String -> IO String
crypt indx key text = do
    n <- readIORef indx
    modifyIORef indx (+ length text)
    return $ crypt' (drop n $ cycle key) text
--crypt indx key text = return text

crypt' :: String -> String -> String
crypt' key text = zipWith (\c k -> chr $ ord c `Bits.xor` ord k) text (cycle key)


main :: IO ()
main = do
    let currentVersion = versionBranch version -- if isn't match with major/minor/build version, invalid save data.
    --gen <- getStdGen
    let gen = mkStdGen 0 
    (is, iw) <- SampleScenario.initScenario
    let s'= initScenario is inCastle 
    let w = initWorld iw gen True
        s = SampleScenario.modScenario s' -- TODO:WIP

    existSaveData <- doesFileExist saveDataPath
    indx <- newIORef 0
    
    (run, reset) <- if not existSaveData then return (runGame, True) else do
        c  <- readFile saveDataPath
        ls <- lines <$> crypt indx encKey c
        if length ls > 1 && take 3 (read $ head ls :: [Int]) == take 3 currentVersion then do
          let is  = read <$> filter (not . null) (tail ls)
              is' = foldl (\acc i -> if i == Abort then tail acc else i:acc) [] is
          return (loadGame (reverse is'), False)
        else
          return (runGame, True)

    when reset $ writeFile saveDataPath =<< crypt indx encKey (show currentVersion ++ "\n")
             
    -- setting for CUI
    let picOf = maybe mempty SampleScenario.pic
    drawCache <- newDrawCache
    let renderMethod   = renderWithCache drawCache
        display        = cuiRender renderMethod picOf
        display' s e w = setCursorPosition 0 0 >> display s e w
    let cmd            = getKey indx (clearCache drawCache)

    clearScreen
    hideCursor
    w' <- run (display' s) cmd s w inCastle
    showCursor

    appendFile saveDataPath =<< crypt indx encKey (show Abort ++ "\n")
    void $ saveWorld w' "world.dat"


type DisplayIO = Scenario -> Event -> World -> IO()
type InputIO   = InputType -> IO Input

-- ==========================================================================

getKey :: IORef Int -> IO () -> InputIO
getKey indx refresh itype = do
    i <- getKey' itype
    appendFile saveDataPath =<< crypt indx encKey (show i ++ "\n")
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
