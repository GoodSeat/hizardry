module Engine.Sound (
    initAudio,
    quitAudio,
    playBGM,
    playSoundEffect
) where

import Control.Monad (void)
import qualified SDL
import qualified SDL.Mixer as Mixer

-- | オーディオデバイスを初期化する
initAudio :: IO ()
initAudio = do
    SDL.initialize [SDL.InitAudio]
    Mixer.openAudio Mixer.defaultAudio 256

-- | オーディオデバイスを終了する
quitAudio :: IO ()
quitAudio = do
    Mixer.closeAudio
    SDL.quit

-- | BGMを再生する
playBGM :: FilePath -> IO ()
playBGM musicFile = do
    music <- Mixer.load musicFile
    Mixer.playMusic Mixer.Forever music

-- | 効果音を再生する
playSoundEffect :: FilePath -> IO ()
playSoundEffect soundFile = do
    chunk <- Mixer.load soundFile
    void $ Mixer.play chunk
