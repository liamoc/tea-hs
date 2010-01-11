module Tea.Sound 
   ( loadSound
   , maxVolume
   , play
   , playLoop
   , pause
   , resume
   , stop
   , setVolume
   , getVolume
   , isPlaying
   , isPaused
   , pauseAll
   , resumeAll
   , stopAll
   , setMasterVolume
   , getMasterVolume
   ) where

import qualified Graphics.UI.SDL.Mixer as Mixer
import Control.Monad.Trans
import Control.Monad.State
import Tea.Types
import Tea.Monad
import Data.Map as Map

data Sound = Sound Mixer.Chunk 

maxVolume = 128

noChannel :: Mixer.Channel
noChannel = -1

loadSound :: FilePath -> Tea Sound
loadSound f = liftIO (Mixer.loadWAV f) >>= return . Sound

play :: Sound -> Tea ()
play snd = playLoop snd 0

playLoop :: Sound -> Int -> Tea ()
playLoop s@(Sound chun) loops = do stop s
                                   chan <- liftIO $ Mixer.playChannel (-1) chun loops
                                   modify $ \s@TS{_channels = c} -> s { _channels = insert ((read $ show chun) :: Int) chan c } 

withChannel = withChannelRet ()

withChannelRet ret chun f = let key = (read $ show chun) :: Int 
                            in do c <- fmap _channels get 
                                  case Map.lookup key c of
                                     Just channel -> f channel
                                     Nothing      -> return ret

pause :: Sound -> Tea ()                      
pause (Sound chun) = withChannel chun $ \chan -> liftIO $ Mixer.pause chan

resume :: Sound -> Tea ()
resume (Sound chun) = withChannel chun $ \chan -> liftIO $ Mixer.resume chan

stop :: Sound -> Tea ()
stop (Sound chun) = withChannel chun $ \chan -> liftIO $ Mixer.haltChannel chan

setVolume :: Sound -> Int -> Tea ()
setVolume (Sound chun) vol = withChannel chun $ \chan -> liftIO $ do Mixer.volume chan vol
                                                                     return ()
getVolume :: Sound -> Tea Int
getVolume (Sound chun) = withChannelRet (-1) chun $ \chan -> liftIO $ Mixer.volume chan (-1)

isPlaying :: Sound -> Tea Bool
isPlaying (Sound chun) = withChannelRet False chun $ \chan -> liftIO $ Mixer.isChannelPlaying chan

isPaused :: Sound -> Tea Bool
isPaused (Sound chun) = withChannelRet False chun $ \chan -> liftIO $ Mixer.isChannelPaused chan

pauseAll :: Tea ()
pauseAll = liftIO $ Mixer.pause noChannel

resumeAll :: Tea ()
resumeAll = liftIO $ Mixer.resume noChannel

stopAll :: Tea ()
stopAll = liftIO $ Mixer.haltChannel noChannel

setMasterVolume :: Int -> Tea ()
setMasterVolume v = liftIO $ Mixer.volume noChannel v >> return ()

getMasterVolume :: Tea Int
getMasterVolume = liftIO $ Mixer.volume noChannel (-1)

