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
import System.IO (Handle, hPutStrLn, hFlush, hGetContents)
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

-- For sound effects, we keep a player instance running in slave mode
soundEffectProcess :: IORef (Maybe (ProcessHandle, Handle))
{-# NOINLINE soundEffectProcess #-}
soundEffectProcess = unsafePerformIO (newIORef Nothing)


-- State for tracking looping and reservations
isLooping :: IORef Bool
{-# NOINLINE isLooping #-}
isLooping = unsafePerformIO (newIORef False)

reservation :: IORef (Maybe FilePath)
{-# NOINLINE reservation #-}
reservation = unsafePerformIO (newIORef Nothing)

-- | Find an available media player and launch a dedicated slave process for sound effects.
initAudio :: IO ()
initAudio = do
    mPlayer <- findPlayer
    case mPlayer of
        Just player -> do
            writeIORef activePlayer (Just player)
            putStrLn $ "Sound player found: " ++ show (playerType player)
            launchSESlave player
        Nothing     -> putStrLn "Warning: ffplay or VLC not found. Sound will not be played."
  where
    findPlayer :: IO (Maybe Player)
    findPlayer = do
        let candidates = [ (FFPLAY, "ffplay"), (VLC, "vlc") ]
        let standardPaths = [ (FFPLAY, ".\\bin\\ffplay.exe")
                            , (VLC, "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe")
                            , (VLC, "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe")
                            ]

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
    
    launchSESlave :: Player -> IO ()
    launchSESlave player = 
        when (playerType player == FFPLAY) $ do
            let (path, args) = buildSlaveArgs player
            eres <- try (createProcess (proc path args) { std_out = NoStream, std_err = NoStream })
                 :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
            case eres of
                Right (Just stdin_h, Just stdout_h, Just stderr_h, ph) -> do
                    -- Drain the output pipes to prevent the process from blocking
                    void $ forkIO $ void $ hGetContents stdout_h
                    void $ forkIO $ void $ hGetContents stderr_h
                    writeIORef soundEffectProcess (Just (ph, stdin_h))
                _ -> putStrLn "Failed to launch sound effect slave process."

    buildSlaveArgs player = (playerPath player, ["-nodisp", "-hide_banner", "-loglevel", "quiet", "-slave", "-"])


-- | Stop the current music and sound effect processes, and clear all related states.
quitAudio :: IO ()
quitAudio = do
    -- Stop BGM
    stopBGM
    -- Stop SE slave process
    mSeHandle <- atomicModifyIORef' soundEffectProcess (\h -> (Nothing, h))
    case mSeHandle of
        Just (ph, h) -> terminateProcess ph
        Nothing -> return ()

-- | Alias for quitAudio for semantic clarity.
stopBGM :: IO ()
stopBGM = do
    mHandle <- atomicModifyIORef' bgmHandle (\h -> (Nothing, h))
    maybe (return ()) terminateProcess mHandle
    writeIORef isLooping False
    writeIORef reservation Nothing


-- | Internal function to play music, handling looping and player differences.
playMusicInternal :: Bool -> FilePath -> IO ()
playMusicInternal isLoopingRequested file = do
    mPlayer <- readIORef activePlayer
    case mPlayer of
        Nothing -> return ()
        Just player -> do
            stopBGM -- Stop only the BGM process
            writeIORef isLooping isLoopingRequested
            
            let (path, args) = buildMusicArgs player isLoopingRequested file
            let effectiveLooping = isLoopingRequested && playerType player /= WMP

            -- BGM doesn't need its output managed as it's not interactive
            (_, _, _, ph) <- createProcess (proc path args) { std_out = NoStream, std_err = NoStream }
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
    mSeHandle <- readIORef soundEffectProcess
    case mSeHandle of 
        Just (_, stdin_h) -> do
            hPutStrLn stdin_h file
            hFlush stdin_h
        -- Fallback to creating a new process if slave is not running (e.g., player is not ffplay)
        Nothing -> do
            mPlayer <- readIORef activePlayer
            case mPlayer of 
                Nothing -> return ()
                Just player -> void . forkIO $ do
                    let (path, args) = buildEffectArgs player file
                    void $ createProcess (proc path args) { std_out = NoStream, std_err = NoStream }
  where
    buildEffectArgs player f = 
        let pPath = playerPath player
        in case playerType player of 
            FFPLAY -> (pPath, ["-hide_banner", "-nodisp", "-autoexit", "-loglevel", "quiet", f])
            VLC    -> (pPath, ["--intf", "dummy", "--play-and-exit", f])
            WMP    -> (pPath, ["/play", "/close", f])
