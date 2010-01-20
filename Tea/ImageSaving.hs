-- |Includes the ImageSaving class, for types with savable image content, its
--  instances, and monadic helper functions.
module Tea.ImageSaving ( ImageSaving (save)
                       , saveM
                       ) where

import Control.Monad.Trans
import Graphics.UI.SDL (saveBMP, Surface)
import Tea.Tea
import Tea.Screen
import Tea.Bitmap

-- |A class instantiated over all types that can save their image content to a
--  BMP file
class ImageSaving v where
   -- |Save a BMP image of the providing ImageSaving type to the filename
   --  specified
   save :: v -> String -> Tea s Bool
   save v = liftIO . saveBMP (image_saving_buffer v)
   image_saving_buffer :: v -> Surface

-- |A monadic convenience function that takes a Tea action instead of a buffer to save.
saveM :: (ImageSaving v) => Tea s v -> String -> Tea s Bool
saveM m s = m >>= flip save s

instance ImageSaving Screen where
   image_saving_buffer = screenBuffer

instance ImageSaving Bitmap where
   image_saving_buffer = buffer
