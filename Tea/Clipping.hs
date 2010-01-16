module Tea.Clipping ( Clipping (..)
                    , clipM
                    ) where

import Graphics.UI.SDL
import Tea.Monad
import Tea.Types
import Control.Monad.State
import Control.Monad.Trans

withTea m s st = runStateT (runStateT (extractTea m) s) st

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
