module Engine.Sound (
    initAudio,
    quitAudio,
    stopBGM,
    playBGM,
    playMusicOnce,
    playSoundEffect,
    playBGMIfNoMusic
) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, terminateProcess, ProcessHandle, waitForProcess, getPid)
import System.Directory (findExecutable)
import Data.Maybe (isJust)
import Data.Foldable (forM_)

-- IORef to hold the process handle of the currently playing BGM/Music
bgmProcessHandle :: IORef (Maybe ProcessHandle)
{-# NOINLINE bgmProcessHandle #-}
bgmProcessHandle = unsafePerformIO (newIORef Nothing)

-- IORef to track if the current music is looping
isLooping :: IORef Bool
{-# NOINLINE isLooping #-}
isLooping = unsafePerformIO (newIORef False)

-- IORef for reserving the next BGM
reservation :: IORef (Maybe FilePath)
{-# NOINLINE reservation #-}
reservation = unsafePerformIO (newIORef Nothing)

vlcPath :: IORef (Maybe FilePath)
{-# NOINLINE vlcPath #-}
vlcPath = unsafePerformIO (newIORef Nothing)

-- | Find VLC executable and store its path.
initAudio :: IO ()
initAudio = do
    path <- findExecutable "vlc"
    writeIORef vlcPath path
    unless (isJust path) $
        putStrLn "Warning: VLC executable not found. Sound will not be played."

-- | Stop the BGM and clear all states.
quitAudio :: IO ()
quitAudio = do
    mHandle <- atomicModifyIORef' bgmProcessHandle (\h -> (Nothing, h))
    maybe (return ()) terminateProcess mHandle
    writeIORef isLooping False
    writeIORef reservation Nothing

-- | Stop the currently playing BGM/music. Alias for quitAudio.
stopBGM :: IO ()
stopBGM = quitAudio

-- | Stop the old BGM and play a new one, looping.
playBGM :: FilePath -> IO ()
playBGM file = do
    readIORef vlcPath >>= maybe (return ()) handlePlay
  where
    handlePlay vlc = do
        quitAudio -- Stop previous music and clear reservations
        writeIORef isLooping True
        (_, _, _, ph) <- createProcess (proc vlc ["--intf", "dummy", "--loop", file])
        writeIORef bgmProcessHandle (Just ph)

-- | Stop the old BGM/music and play a new one once.
playMusicOnce :: FilePath -> IO ()
playMusicOnce file = do
    readIORef vlcPath >>= maybe (return ()) handlePlay
  where
    handlePlay vlc = do
        quitAudio -- Stop previous music and clear reservations
        writeIORef isLooping False
        (_, _, _, ph) <- createProcess (proc vlc ["--intf", "dummy", "--play-and-exit", file])
        writeIORef bgmProcessHandle (Just ph)
        -- Fork a thread to wait for the music to finish and then play the reserved BGM
        void . forkIO $ do
            _ <- waitForProcess ph
            mMyPid <- getPid ph
            -- Check if this process was the one we were waiting for, and not a new one.
            mCurrentHandle <- readIORef bgmProcessHandle
            case mCurrentHandle of
                Just currentPh -> do
                    mCurrentPid <- getPid currentPh
                    -- If the PID matches, it means no new music has started.
                    when (mMyPid == mCurrentPid) $ do
                        writeIORef bgmProcessHandle Nothing
                Nothing -> return ()

            -- After waiting and potentially clearing the handle, check for reservations.
            mReservedFile <- atomicModifyIORef' reservation (\r -> (Nothing, r))
            forM_ mReservedFile playBGM

-- | Play a looping BGM based on the current playback state.
playBGMIfNoMusic :: FilePath -> IO ()
playBGMIfNoMusic file = do
    looping <- readIORef isLooping
    -- If a BGM is already looping, do nothing.
    if looping
    then return ()
    else do
        mHandle <- readIORef bgmProcessHandle
        case mHandle of
            -- If no music is playing, play immediately.
            Nothing -> playBGM file
            -- If a non-looping music is playing, reserve the new BGM.
            Just _ -> void $ atomicModifyIORef' reservation $ \m ->
                case m of
                    -- Only reserve if there is no prior reservation.
                    Nothing -> (Just file, ())
                    Just _  -> (m, ())

-- | Play a sound effect once.
playSoundEffect :: FilePath -> IO ()
playSoundEffect file = do
    readIORef vlcPath >>= maybe (return ()) handlePlay
  where
    handlePlay vlc = void . forkIO $ do
        -- Fire and forget.
        (_, _, _, _) <- createProcess $ proc vlc ["--intf", "dummy", "--play-and-exit", file]
        return ()
