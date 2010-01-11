module Tea.Clipping ( Clipping (..)
                    , clipM
                    ) where

import Graphics.UI.SDL
import Tea.Monad
import Tea.Types
import Control.Monad.State
import Control.Monad.Trans

withTea m st = return =<< runStateT (extract m) st

class Clipping v where
   clip :: v -> (Int, Int) -> Int -> Int -> Tea z -> Tea z
   clip s (x, y) w h m = do
                      scr <- get
                      (v, st') <- liftIO $ withClipRect (clipping_buffer s) (Just $ Rect x y w h) (withTea m scr)
                      put st'
                      return v
   clipping_buffer :: v -> Surface   

clipM v a b c d  = v >>= \v' -> clip v' a b c d

