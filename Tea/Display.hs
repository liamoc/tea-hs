module Tea.Display
       ( screen
       , update
       , runTea
       , bitsPerPixel
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


screen :: Tea s Screen
screen = _screen <$> getT

setFrameRate :: Int ->  Tea s ()
setFrameRate n = modifyT $ \ts -> ts { _fpsCap = 1000 `div` n }

update :: Tea s ()
update = do ts@(TS { _screen = (Screen x), _fpsCap = fps, _lastUpdate = last}) <- getT
            t <- liftIO SDL.getTicks
            liftIO $ do
                     when (fromIntegral t < last + fps) $ SDL.delay $ fromIntegral $ last + fps - fromIntegral t
                     SDL.tryFlip x
            putT $ ts { _lastUpdate = fromIntegral t}

bitsPerPixel = 32

initialEventState = ES { keyCodes    = listArray (minBound :: KeyCode, maxBound :: KeyCode) $ repeat False
                       , keysDown    = 0
                       }

runTea :: Int -> Int -> s -> Tea s m -> IO ()
runTea w h s m = do
                 SDL.init [SDL.InitEverything]
                 Mixer.openAudio 44100 Mixer.AudioS16Sys 2 1024
                 surf <- SDL.setVideoMode w h bitsPerPixel [SDL.SWSurface]
                 let initialState = (TS (Screen surf) initialEventState (1000 `div` 60) 0 empty)
                 ((v,s'), st') <- runStateT (runStateT (extractTea m) s) initialState
                 Mixer.closeAudio
                 SDL.quit
                 return ()
