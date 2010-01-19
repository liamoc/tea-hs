module Tea.Grabbing ( Grabbing (grab)
                    , grabM
                    , grabM2
                    ) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Sprig as SPG
import Control.Monad.Trans
import Tea.Tea
import Tea.Screen
import Tea.Bitmap

class Grabbing v where
   grab :: Grabbing s => v -> (Int, Int) -> Int -> Int -> s -> Tea s ()
   grab s (x, y) w h ret = do
                           liftIO $ SPG.pushBlend SPG.CopySrcAlpha
                           liftIO $ SPG.blit src (Just $ SDL.Rect x y w h) (grabbing_buffer ret) (Just $ SDL.Rect 0 0 w h)
                           liftIO SPG.popBlend
                           return ()
                           where src = grabbing_buffer s
   grabbing_buffer :: v -> SDL.Surface

grabM m a b c d = m >>= \m' -> grab m' a b c d
grabM2 m a b c d = m >>= \m' -> d >>= grab m' a b c

instance Grabbing Bitmap where
   grabbing_buffer = buffer

instance Grabbing Screen where
   grabbing_buffer = screenBuffer
