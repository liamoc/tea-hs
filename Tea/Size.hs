-- | Includes the Size class for types with dimensions, its instances, and
--   monadic convenience functions.
module Tea.Size ( Size (width,height)
                , heightM
                , widthM
                ) where

import Graphics.UI.SDL(Surface, surfaceGetHeight, surfaceGetWidth)
import Tea.Screen
import Tea.Bitmap
import Tea.Tea

-- |A class instantiated over all types that have a width and height.
--  Intended really only for internal use. Define your own Size class
--  if you want similar behavior.
class Size v where
   width  :: v -> Int
   width  = surfaceGetWidth  . size_buffer
   height :: v -> Int
   height = surfaceGetHeight . size_buffer
   size_buffer :: v -> Surface

widthM :: (Size a) => Tea s a -> Tea s Int
widthM  = fmap width
heightM :: (Size a) => Tea s a -> Tea s Int
heightM = fmap height

instance Size Screen where
   size_buffer = screenBuffer

instance Size Bitmap where
   size_buffer = buffer
