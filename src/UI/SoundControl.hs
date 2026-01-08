module UI.SoundControl (
      initSound
    , quitSound
    , playSound
    , SETypeToFilePath
    , BGMTypeToFilePath
) where
    
import PreludeL
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef)
import System.Directory (doesFileExist)

import Engine.GameAuto
import Data.World
import Control.Sound (initAudio, quitAudio, playBGM, stopBGM, playMusicOnce, playBGMIfNoMusic, playSoundEffect)

type SETypeToFilePath  = SEType -> Maybe FilePath
type BGMTypeToFilePath = (Place, FilePath)                -- ^ previous step condition.
                      -> Place                            -- ^ place.
                      -> BGMType                          -- ^ trigger for bgm.
                      -> Maybe (Either FilePath FilePath) -- ^ one time BGM, or loop BGM.
                                                          --   Nothing -> resume BGM, Left "" -> stopBGM, Right "" -> resume BGM.

initSound = initAudio
quitSound = quitAudio

playSound :: IORef (Place, FilePath)
          -> SETypeToFilePath
          -> BGMTypeToFilePath
          -> Event -> World -> IO ()
playSound cp st bt (General d)        w = when (validSE w) (playSE d st) >> playBGM' cp (typeBGM d) w bt
playSound cp st bt (ShowStatus _ _ d) w = when (validSE w) (playSE d st) >> playBGM' cp (typeBGM d) w bt
playSound cp st bt _                  w = playBGM' cp Ambient w bt

playSE :: Display -> SETypeToFilePath -> IO ()
playSE d st = case st (typeSE d) of
  Nothing   -> return ()
  Just path -> do
    existSE <- doesFileExist path
    when existSE $ playSoundEffect path

playBGM' :: IORef (Place, FilePath) -> BGMType -> World -> BGMTypeToFilePath -> IO ()
playBGM' cp typeB w bt
  | not (validBGM w) = stopBGM
  | otherwise        = do
    let np = place w
    (op, opath) <- readIORef cp
    let bgm = bt (op, opath) np typeB
    case bgm of
      Nothing           -> writeIORef cp (np, "")
      Just (Left path)  -> do
        existBGM <- doesFileExist path
        if existBGM then playMusicOnce path else stopBGM
        writeIORef cp (np, "")
      Just (Right path) ->  do
        existBGM <- doesFileExist path
        when (existBGM && opath /= path) $ (if opath == "" then playBGMIfNoMusic else playBGM) path
        writeIORef cp (np, path)


validSE :: World -> Bool
validSE = switchSE . worldOption

validBGM :: World -> Bool
validBGM = switchBGM . worldOption

