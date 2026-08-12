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
import Data.List (isSuffixOf, intersperse)
import Data.Char (ord, chr)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Text.Read (readMaybe)
import qualified Data.Bits as Bits

import Engine.GameAuto
import Engine.InCastle (inCastle)
import Engine.InEdgeOfTown (inEdgeOfTown)
import Data.World (World(..), Place(..), place, saveWorld, loadWorld, initWorld, Seed, switchBGM)
import Data.PlayEvent

import Control.CUI
import Control.Sound (playBGM, stopBGM)
import UI.CuiRender (cuiRender, renderWithCache)
import UI.SoundControl

import qualified SampleScenario.Home as SampleScenario


-- note
-- * support Japanese in Edge of Town.
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
    (wOrg, seedOrg, canLoad) <- case infData0 of
        Nothing        -> return (w0, seed0, False)
        Just (path, _) -> do
            res <- loadWorld path
            case res of Right ws -> return (fst ws, snd ws, True)
                        Left  _  -> return (w0, seed0, False)

    -- restore from input log.
    existInputLog <- doesFileExist inputLogPath
    indx <- newIORef 0
    let ekey = encKey s
--  let ekey = ""
    
    (runOrg, canResume) <- if not existInputLog then return (runGame, True) else do
        c  <- readFile inputLogPath
        ls <- lines <$> crypt indx ekey c
        if length ls > 3 &&
           (take 3 <$> (readMaybe (ls !! 0) :: Maybe [Int])) == Just (take 3 currentVersion)  &&  -- 1:check app version.
           (take 3 <$> (readMaybe (ls !! 1) :: Maybe [Int])) == Just (take 3 currentVersionS) &&  -- 2:check scenario version.
           (readMaybe (ls !! 2) :: Maybe Int) == Just seedOrg                                     -- 3:check if seed is match.
        then do
          let is  = read <$> filter (not . null) (drop 3 ls)
              is' = foldl (\acc i -> if i == Abort then tail acc else i:acc) [] is
          return (loadGame (reverse is'), True)
        else
          return (runGame, False)

    -- show title
    when (switchBGM $ worldOption wOrg) $ playBGM "res/openingTitle.mp3"
    startOp <- title (scenarioName s) canLoad canResume
    stopBGM
    case startOp of
      Nothing -> return ()
      Just op -> do
        let resetInputLog sd = writeIORef indx 0 >> (writeFile inputLogPath =<< crypt indx ekey (
                                show currentVersion  ++ "\n"    -- 1:save app version.
                             ++ show currentVersionS ++ "\n"    -- 2:save scenario version.
                             ++ show sd ++ "\n"                 -- 3:save seed for check data.
                            ))
        
        let (w, seed) | op == NewGame = (w0  , seed0  )
                      | otherwise     = (wOrg, seedOrg)
        let run | op == ResumeGame = runOrg
                | otherwise        = runGame

        when (op /= ResumeGame) (resetInputLog seed)

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

data GameStart = NewGame | Loading | ResumeGame deriving Eq

title :: String -> Bool -> Bool -> IO (Maybe GameStart) -- ^ resume or not
title name canLoad canResume = do
    clearScreen
    hideCursor
    setCursorPosition 0 0
    draw (90, 40) (title' name canLoad canResume)
    hSetBuffering stdin NoBuffering
    ask
  where
    ask = do
      x <- getChar
      if      x == 'r' && canResume then showCursor >> return (Just ResumeGame)
      else if x == 'l' && canLoad   then checkLoad name canLoad canResume
      else if x == 'n'              then checkNew  name canLoad canResume
      else if x == 'q'              then showCursor >> return Nothing
      else ask

checkLoad :: String -> Bool -> Bool -> IO (Maybe GameStart)
checkLoad name canLoad canResume =
    if not canResume then showCursor >> return (Just Loading)
    else do
      setCursorPosition 0 0
      draw (90, 40) (inform <> title' name canLoad canResume)
      ask
  where
    inform =  text (17,31) "オートセーブのデータを読み込むと、継続データは失われます。"
           <> text (17,32) "           よろしいですか？  ( Y / N )                    "
           <> rect (15,29) (63,  6) (Draw ' ')
    ask = do
      x <- getChar
      if      x == 'y' then showCursor >> return (Just Loading)
      else if x == 'n' then title name canLoad canResume
      else ask
    
checkNew :: String -> Bool -> Bool -> IO (Maybe GameStart)
checkNew name canLoad canResume =
    if not canLoad then showCursor >> return (Just NewGame)
    else do
      setCursorPosition 0 0
      draw (90, 40) (inform <> title' name canLoad canResume)
      ask
  where
    inform =  text (17,31) "新規ゲームを開始すると、オートセーブのデータは失われます。"
           <> text (17,32) "  (スロット1～9にバックアップしているデータは残ります)    "
           <> text (17,33) "             よろしいですか？  ( Y / N )                  "
           <> rect (15,29) (63,  7) (Draw ' ')
    ask = do
      x <- getChar
      if      x == 'y' then showCursor >> return (Just NewGame)
      else if x == 'n' then title name canLoad canResume
      else ask
    

title' :: String -> Bool -> Bool -> Craphic
title' name canLoad canResume = translate (2, 5) (fromTexts ' ' (lines logo))
                             <> text ((90 - len name) `div` 2,15) name
                             <> (if canResume then text (33,34) "R) e s u m e   G a m e" else mempty)
                             <> (if canLoad   then text (33,35) "L) o a d       G a m e" else mempty)
                             <> text (33,36 - dlt) "N) e w         G a m e"
                             <> text (33,37 - dlt) "Q) u i t       G a m e"
                             <> text (14,32) (intersperse ' ' "Welcome to the world of Hizardry")
                             <> rect (11,31) (71,  8) (Draw ' ')
                             <> rect ( 1, 1) (89, 40) (Draw ' ')
  where dlt = if canLoad then 0 else 1


logo :: String
logo = "Hb      dM                                                                ...         \n"
    ++ "@b      dM .Qmk XQQQmp  .mp   QkQa,  QkQQJ, .mQQa, .NNNp,WQQ&    .gQQ%    dMF         \n"
    ++ "Hb      dM  ,N    .dY  .H%H[  M) .M` M)  .M[.@_ .#   TMMN,4HHh. .HH#'     dMF         \n"
    ++ "Hb .+QQ dM  ,N >.JY`,C dNQdM, MYTM,  M).=.H%.HYTN,(QQm?MMMe/M@MH@@Y.Qkkk) dMMNNNNNNNN \n"
    ++ "@b  ?77 dM .UMk.MMWWWSJ@   (N.M|  TN,MHWWY^ .M~ .Th.?77 MMMb.MH@M^ ?????! dM#!!!!!!!! \n"
    ++ "Mb      dM                                            .dMM@(dH@P(d@ggggg[ dMF         \n"
    ++ "Mb      dM                                           .MMMt.MHM^           ?'t         \n"
    ++ "                                                   .jMM#_dHH@`                        "

-- ==========================================================================
