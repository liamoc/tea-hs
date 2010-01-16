module Tea.ImageSaving ( ImageSaving (..)
                       , saveM
                       ) where

import Graphics.UI.SDL as SDL
import Tea.Types
import Tea.Monad
import Control.Monad.Trans

class ImageSaving v where
   save :: v -> String -> Tea s Bool
   save z path = liftIO $ saveBMP (image_saving_buffer z) path
   image_saving_buffer :: v -> SDL.Surface   

saveM m s = m >>= \m' -> save m' s 
