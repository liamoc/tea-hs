module Tea.Sound ( loadSound
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
                 , Sound
                 ) where

import Prelude hiding (lookup)
import qualified Graphics.UI.SDL.Mixer as Mixer
import Control.Monad.State
import Data.Map(insert, lookup)
import Tea.TeaState
import Tea.Tea

data Sound = Sound Mixer.Chunk 

maxVolume = 128

noChannel :: Mixer.Channel
noChannel = -1

loadSound :: FilePath -> Tea s Sound
loadSound f = liftIO (Mixer.loadWAV f) >>= return . Sound

play :: Sound -> Tea s ()
play snd = playLoop snd 0

playLoop :: Sound -> Int -> Tea s ()
playLoop s@(Sound chun) loops = do stop s
                                   chan <- liftIO $ Mixer.playChannel (-1) chun loops
                                   modifyT $ \s@TS{_channels = c} -> s { _channels = insert ((read $ show chun) :: Int) chan c } 

withChannel = withChannelRet ()

withChannelRet ret chun f = let key = (read $ show chun) :: Int 
                            in do c <- fmap _channels getT 
                                  case lookup key c of
                                     Just channel -> f channel
                                     Nothing      -> return ret

pause :: Sound -> Tea s ()                      
pause (Sound chun) = withChannel chun $ \chan -> liftIO $ Mixer.pause chan

resume :: Sound -> Tea s ()
resume (Sound chun) = withChannel chun $ \chan -> liftIO $ Mixer.resume chan

stop :: Sound -> Tea s ()
stop (Sound chun) = withChannel chun $ \chan -> liftIO $ Mixer.haltChannel chan

setVolume :: Sound -> Int -> Tea s ()
setVolume (Sound chun) vol = withChannel chun $ \chan -> liftIO $ do Mixer.volume chan vol
                                                                     return ()
getVolume :: Sound -> Tea s Int
getVolume (Sound chun) = withChannelRet (-1) chun $ \chan -> liftIO $ Mixer.volume chan (-1)

isPlaying :: Sound -> Tea s Bool
isPlaying (Sound chun) = withChannelRet False chun $ \chan -> liftIO $ Mixer.isChannelPlaying chan

isPaused :: Sound -> Tea s Bool
isPaused (Sound chun) = withChannelRet False chun $ \chan -> liftIO $ Mixer.isChannelPaused chan

pauseAll :: Tea s ()
pauseAll = liftIO $ Mixer.pause noChannel

resumeAll :: Tea s ()
resumeAll = liftIO $ Mixer.resume noChannel

stopAll :: Tea s ()
stopAll = liftIO $ Mixer.haltChannel noChannel

setMasterVolume :: Int -> Tea s ()
setMasterVolume v = liftIO $ Mixer.volume noChannel v >> return ()

getMasterVolume :: Tea s Int
getMasterVolume = liftIO $ Mixer.volume noChannel (-1)

