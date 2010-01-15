module Tea.Grabbing ( Grabbing (..)
                    , grabM
                    , grabM2
                    ) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Sprig as SPG
import Tea.Types
import Tea.Blitting
import Tea.Monad
import Control.Monad.Trans
class Grabbing v where
   grab :: Blitting s => v -> (Int, Int) -> Int -> Int -> s -> Teacup s ()
   grab s (x, y) w h ret = do
                           liftIO $ SPG.pushBlend (SPG.CopySrcAlpha)
                           liftIO $ SPG.blit src (Just $ SDL.Rect x y w h) (blitting_buffer ret) (Just $ SDL.Rect 0 0 w h)
                           liftIO $ SPG.popBlend
                           return ()
                           where src = grabbing_buffer s
   grabbing_buffer :: v -> SDL.Surface  

grabM m a b c d = m >>= \m' -> grab m' a b c d
grabM2 m a b c d = m >>= \m' -> d >>= \d' -> grab m' a b c d'
