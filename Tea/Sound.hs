-- | Includes the Sound type and associated operations, for audio playback
--   in Tea.
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
import Control.Applicative((<$>))
import Data.Map(insert, lookup)
import Tea.TeaState
import Tea.Tea

-- |A data type representing a sound that can be played in Tea.
data Sound = Sound Mixer.Chunk

-- |The maximum volume level
maxVolume :: Int
maxVolume = 128

noChannel :: Mixer.Channel
noChannel = -1

-- | Load a sound from the given filename. Supports CMD, Wav, Trackers
--   such as MOD files, MIDI, Ogg,  MP3, however MIDI and MP3 support
--   is platform dependant and shaky.
loadSound :: FilePath -> Tea s Sound
loadSound f = Sound <$> liftIO (Mixer.loadWAV f)

-- | Play a sound
play :: Sound -> Tea s ()
play snd = playLoop snd 0

-- | Play a sound, looping the provided number of times.
playLoop :: Sound -> Int -> Tea s ()
playLoop s@(Sound chun) loops = do stop s
                                   chan <- liftIO $ Mixer.playChannel (-1) chun loops
                                   modifyT $ \s@TS{_channels = c} -> s { _channels = insert (read $ show chun :: Int) chan c }

withChannel = withChannelRet ()

withChannelRet ret chun f = let key = read $ show chun :: Int
                            in do c <- fmap _channels getT
                                  case lookup key c of
                                     Just channel -> f channel
                                     Nothing      -> return ret

-- |Pause a sound
pause :: Sound -> Tea s ()
pause (Sound chun) = withChannel chun $ liftIO . Mixer.pause

-- |Resume a sound
resume :: Sound -> Tea s ()
resume (Sound chun) = withChannel chun $ liftIO . Mixer.resume

-- |Stop a sound playing
stop :: Sound -> Tea s ()
stop (Sound chun) = withChannel chun $ liftIO . Mixer.haltChannel

-- |Set a sound's playback volume.
setVolume :: Sound -> Int -> Tea s ()
setVolume (Sound chun) vol = withChannel chun $ \chan -> liftIO $ do Mixer.volume chan vol
                                                                     return ()
-- |Get a sound's playback volume
getVolume :: Sound -> Tea s Int
getVolume (Sound chun) = withChannelRet (-1) chun $ \chan -> liftIO $ Mixer.volume chan (-1)

-- |Return if a sound is currently playing
isPlaying :: Sound -> Tea s Bool
isPlaying (Sound chun) = withChannelRet False chun $ liftIO . Mixer.isChannelPlaying

-- |Return if a sound is currently paused.
isPaused :: Sound -> Tea s Bool
isPaused (Sound chun) = withChannelRet False chun $ liftIO . Mixer.isChannelPaused

-- |Pause all currently playing sounds.
pauseAll :: Tea s ()
pauseAll = liftIO $ Mixer.pause noChannel

-- |Resume all sounds that have been paused.
resumeAll :: Tea s ()
resumeAll = liftIO $ Mixer.resume noChannel

-- |Stop playback of all sounds.
stopAll :: Tea s ()
stopAll = liftIO $ Mixer.haltChannel noChannel

-- |Set the master volume of sound playback
setMasterVolume :: Int -> Tea s ()
setMasterVolume v = liftIO $ Mixer.volume noChannel v >> return ()

-- |Get the master volume of sound playback
getMasterVolume :: Tea s Int
getMasterVolume = liftIO $ Mixer.volume noChannel (-1)

