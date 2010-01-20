-- |Includes core Tea monad functions that regulate display and framerate
--  as well as the runTea function that initializes hardware.
module Tea.Display
       ( screen
       , update
       , runTea
       , setFrameRate
       ) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as Mixer
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative((<$>))
import Data.Map (empty)
import Data.Array (listArray)
import Tea.Input (KeyCode)
import Tea.TeaState
import Tea.Screen
import Tea.Tea

-- |Retrieve a buffer handle on the Screen
screen :: Tea s Screen
screen = _screen <$> getT

-- |Sets the frame rate cap. Note that this is merely a cap, and the frame rate may
--  be slower than this if additional processing is required.
setFrameRate :: Int ->  Tea s ()
setFrameRate n = modifyT $ \ts -> ts { _fpsCap = 1000 `div` n }

-- |Flip buffers, causing the hardware display to be updated with changes to
--  the Screen. Note this also waits sufficient time for a frame to have elapsed.
update :: Tea s ()
update = do ts@(TS { _screen = (Screen x), _fpsCap = fps, _lastUpdate = last}) <- getT
            t <- liftIO SDL.getTicks
            liftIO $ do
                     when (fromIntegral t < last + fps) $ SDL.delay $ fromIntegral $ last + fps - fromIntegral t
                     SDL.tryFlip x
            putT $ ts { _lastUpdate = fromIntegral t}

initialEventState = ES { keyCodes    = listArray (minBound :: KeyCode, maxBound :: KeyCode) $ repeat False
                       , keysDown    = 0
                       }

-- |Initialize hardware and run a Tea action with the specified state type.
runTea :: Int      -- ^ Screen Width
       -> Int      -- ^ Screen Height
       -> s        -- ^ State data
       -> Tea s m  -- ^ Tea action
       -> IO ()
runTea w h s m = do
                 SDL.init [SDL.InitEverything]
                 Mixer.openAudio 44100 Mixer.AudioS16Sys 2 1024
                 surf <- SDL.setVideoMode w h 0 [SDL.SWSurface]
                 let initialState = (TS (Screen surf) initialEventState (1000 `div` 60) 0 empty)
                 ((v,s'), st') <- runStateT (runStateT (extractTea m) s) initialState
                 Mixer.closeAudio
                 SDL.quit
                 return ()
