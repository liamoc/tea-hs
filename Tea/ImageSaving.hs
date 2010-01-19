module Tea.ImageSaving ( ImageSaving (save)
                       , saveM
                       ) where

import Control.Monad.Trans
import Graphics.UI.SDL (saveBMP, Surface)
import Tea.Tea
import Tea.Screen
import Tea.Bitmap

class ImageSaving v where
   save :: v -> String -> Tea s Bool
   save z path = liftIO $ saveBMP (image_saving_buffer z) path
   image_saving_buffer :: v -> Surface

saveM m s = m >>= \m' -> save m' s

instance ImageSaving Screen where
   image_saving_buffer = screenBuffer

instance ImageSaving Bitmap where
   image_saving_buffer = buffer
