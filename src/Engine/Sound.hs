module Engine.Sound (
    initAudio,
    quitAudio,
    playBGM,
    playMusicOnce,
    playSoundEffect,
    stopBGM
) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, terminateProcess, ProcessHandle)
import System.Directory (findExecutable)
import Data.Maybe (isJust)

-- IORef to hold the process handle of the currently playing BGM
bgmProcessHandle :: IORef (Maybe ProcessHandle)
{-# NOINLINE bgmProcessHandle #-}
bgmProcessHandle = unsafePerformIO (newIORef Nothing)

vlcPath :: IORef (Maybe FilePath)
{-# NOINLINE vlcPath #-}
vlcPath = unsafePerformIO (newIORef Nothing)

-- | Find VLC executable and store its path.
initAudio :: IO ()
initAudio = do
    path <- findExecutable "vlc"
    writeIORef vlcPath path
    when (not . isJust $ path) $
        putStrLn "Warning: VLC executable not found. Sound will not be played."

-- | Stop the BGM if it's playing.
quitAudio :: IO ()
quitAudio = do
    mHandle <- atomicModifyIORef' bgmProcessHandle (\h -> (Nothing, h))
    maybe (return ()) terminateProcess mHandle

-- | Stop the currently playing BGM/music.
stopBGM :: IO ()
stopBGM = quitAudio

-- | Stop the old BGM and play a new one, looping.
playBGM :: FilePath -> IO ()
playBGM file = do
    readIORef vlcPath >>= maybe (return ()) handlePlay
  where
    handlePlay vlc = do
        quitAudio -- Stop previous BGM
        -- We are not closing the stdin/stdout/stderr handles, which might lead to resource leaks in some cases,
        -- but for a long-running BGM process that is only terminated, it's generally acceptable.
        (_, _, _, ph) <- createProcess (proc vlc ["--intf", "dummy", "--loop", file])
        writeIORef bgmProcessHandle (Just ph)

-- | Stop the old BGM/music and play a new one once.
playMusicOnce :: FilePath -> IO ()
playMusicOnce file = do
    readIORef vlcPath >>= maybe (return ()) handlePlay
  where
    handlePlay vlc = do
        quitAudio -- Stop previous BGM/music
        (_, _, _, ph) <- createProcess (proc vlc ["--intf", "dummy", "--play-and-exit", file])
        writeIORef bgmProcessHandle (Just ph)

-- | Play a sound effect once.
playSoundEffect :: FilePath -> IO ()
playSoundEffect file = do
    readIORef vlcPath >>= maybe (return ()) handlePlay
  where
    handlePlay vlc = void . forkIO $ do
        -- Fire and forget. We don't manage the process handle.
        -- VLC with --play-and-exit will terminate itself.
        (_, _, _, _) <- createProcess $ proc vlc ["--intf", "dummy", "--play-and-exit", file]
        return ()