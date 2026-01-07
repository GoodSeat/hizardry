module Control.Sound (
    initAudio,
    quitAudio,
    stopBGM,
    playBGM,
    playMusicOnce,
    playSoundEffect,
    playBGMIfNoMusic
) where

import PreludeL
import Control.Concurrent (forkIO)
import Control.Monad (void, when, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, terminateProcess, ProcessHandle, waitForProcess, getPid, StdStream(NoStream), CreateProcess(..))
import System.Directory (findExecutable, doesFileExist)
import System.IO (IOMode(WriteMode), openFile, Handle)
import Data.Maybe (isJust, isNothing)
import Control.Exception (SomeException, try)

-- Player Definition
data PlayerType = FFPLAY | VLC | WMP deriving (Eq, Show)
data Player = Player { playerType :: PlayerType, playerPath :: FilePath }

activePlayer :: IORef (Maybe Player)
{-# NOINLINE activePlayer #-}
activePlayer = unsafePerformIO (newIORef Nothing)

bgmHandle :: IORef (Maybe ProcessHandle)
{-# NOINLINE bgmHandle #-}
bgmHandle = unsafePerformIO (newIORef Nothing)

isLooping :: IORef Bool
{-# NOINLINE isLooping #-}
isLooping = unsafePerformIO (newIORef False)

reservation :: IORef (Maybe FilePath)
{-# NOINLINE reservation #-}
reservation = unsafePerformIO (newIORef Nothing)

-- | Find an available media player (ffplay > vlc > wmp).
initAudio :: IO ()
initAudio = do
    mPlayer <- findPlayer
    case mPlayer of
        Just player -> do
            writeIORef activePlayer (Just player)
            putStrLn $ "Sound player found: " ++ show (playerType player)
        Nothing     -> putStrLn "Warning: ffplay, VLC, or WMP not found. Sound will not be played."
  where
    findPlayer :: IO (Maybe Player)
    findPlayer = do
        let candidates = [ (FFPLAY, "ffplay"), (VLC, "vlc"), (WMP, "wmplayer") ]
        let standardPaths = [ (FFPLAY, ".\\bin\\ffplay.exe"), -- A common user choice
                              (VLC, "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe"),
                              (VLC, "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe"),
                              (WMP, "C:\\Program Files\\Windows Media Player\\wmplayer.exe"),
                              (WMP, "C:\\Program Files (x86)\\Windows Media Player\\wmplayer.exe") ]

        foundInPath <- findFirstM (\(pt, name) -> fmap (Player pt) <$> findExecutable name) candidates
        case foundInPath of
            Just player -> return (Just player)
            Nothing -> findFirstM (\(pt, path) -> do
                exists <- doesFileExist path
                return $ if exists then Just (Player pt path) else Nothing
                ) standardPaths

    findFirstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
    findFirstM _ [] = return Nothing
    findFirstM f (x:xs) = do
        mRes <- f x
        case mRes of
            Just res -> return (Just res)
            Nothing  -> findFirstM f xs

-- | Stop the current music and clear all related states.
quitAudio :: IO ()
quitAudio = do
    mHandle <- atomicModifyIORef' bgmHandle (\h -> (Nothing, h))
    maybe (return ()) terminateProcess mHandle
    writeIORef isLooping False
    writeIORef reservation Nothing

-- | Alias for quitAudio for semantic clarity.
stopBGM :: IO ()
stopBGM = quitAudio

-- | Internal function to play music, handling looping and player differences.
playMusicInternal :: Bool -> FilePath -> IO ()
playMusicInternal isLoopingRequested file = do
    mPlayer <- readIORef activePlayer
    case mPlayer of
        Nothing -> return ()
        Just player -> do
            quitAudio -- Stop any currently playing music
            writeIORef isLooping isLoopingRequested
            
            let (path, args) = buildMusicArgs player isLoopingRequested file
            let effectiveLooping = isLoopingRequested && playerType player /= WMP

            (_, _, _, ph) <- createSilentProcess path args
            writeIORef bgmHandle (Just ph)
            
            unless effectiveLooping $ void . forkIO $ do
                _ <- waitForProcess ph
                mMyPid <- getPid ph
                mCurrentBgm <- readIORef bgmHandle
                case mCurrentBgm of
                    Just currentPh -> do
                        mCurrentPid <- getPid currentPh
                        when (mMyPid == mCurrentPid) $ writeIORef bgmHandle Nothing
                    Nothing -> return ()
                
                mReservedFile <- atomicModifyIORef' reservation (\r -> (Nothing, r))
                case mReservedFile of
                    Just reservedFile -> playBGM reservedFile
                    Nothing           -> return ()
  where
    buildMusicArgs player loop f = 
        let pPath = playerPath player
        in case playerType player of
            FFPLAY -> (pPath, if loop then ["-hide_banner", "-nodisp", "-loop", "0", "-loglevel", "quiet", f] else ["-hide_banner", "-nodisp", "-autoexit", "-loglevel", "quiet", f])
            VLC    -> (pPath, if loop then ["--intf", "dummy", "--loop", f] else ["--intf", "dummy", "--play-and-exit", f])
            WMP    -> (pPath, ["/play", "/close", f]) -- WMP loop is not supported, plays once. 
    
-- | Public function to play looping BGM.
playBGM :: FilePath -> IO ()
playBGM = playMusicInternal True

-- | Public function to play music once.
playMusicOnce :: FilePath -> IO ()
playMusicOnce = playMusicInternal False

-- | Play a looping BGM based on the current playback state.
playBGMIfNoMusic :: FilePath -> IO ()
playBGMIfNoMusic file = do
    looping <- readIORef isLooping
    unless looping $ do
        mHandle <- readIORef bgmHandle
        case mHandle of
            Nothing -> playBGM file
            Just _  -> void $ atomicModifyIORef' reservation $ \r -> 
                if isNothing r then (Just file, ()) else (r, ()) 

-- | Play a sound effect once.
playSoundEffect :: FilePath -> IO ()
playSoundEffect file = do
    mPlayer <- readIORef activePlayer
    case mPlayer of
        Nothing -> return ()
        Just player -> void . forkIO $ do
            let (path, args) = buildEffectArgs player file
            void $ createSilentProcess path args
  where
    buildEffectArgs player f = 
        let pPath = playerPath player
        in case playerType player of
            FFPLAY -> (pPath, ["-hide_banner", "-nodisp", "-autoexit", "-loglevel", "quiet", f])
            VLC    -> (pPath, ["--intf", "dummy", "--play-and-exit", f])
            WMP    -> (pPath, ["/play", "/close", f])

-- | Helper to create a process with stdout and stderr redirected to null.
createSilentProcess :: FilePath -> [String] -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createSilentProcess path args = do
    createProcess (proc path args) {
        std_out = NoStream,
        std_err = NoStream
    }
