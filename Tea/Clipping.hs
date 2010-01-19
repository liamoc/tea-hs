module Tea.Clipping ( Clipping (clip)
                    , clipM
                    ) where

import Control.Monad.State
import Control.Monad.Trans
import Graphics.UI.SDL (withClipRect, Surface, Rect (..))
import Tea.Tea
import Tea.Screen
import Tea.Bitmap

withTea m  = runStateT . runStateT $ extractTea m

class Clipping v where
   clip :: v -> (Int, Int) -> Int -> Int -> Tea s z -> Tea s z
   clip surf (x, y) w h m = do
                            scr <- getT
                            s <- get
                            ((v, s'),st') <- liftIO $ withClipRect (clipping_buffer surf) (Just $ Rect x y w h) (withTea m s scr)
                            putT st'
                            put s'
                            return v
   clipping_buffer :: v -> Surface

clipM v a b c d  = v >>= \v' -> clip v' a b c d

instance Clipping Screen where
   clipping_buffer = screenBuffer

instance Clipping Bitmap where
   clipping_buffer = buffer
