module Tea.Size ( Size (width,height)
                , heightM
                , widthM
                ) where

import Graphics.UI.SDL(Surface, surfaceGetHeight, surfaceGetWidth)
import Tea.Screen
import Tea.Bitmap

class Size v where
   width  :: v -> Int
   width  = surfaceGetWidth  . size_buffer
   height :: v -> Int
   height = surfaceGetHeight . size_buffer
   size_buffer :: v -> Surface

widthM  = fmap width
heightM = fmap height

instance Size Screen where
   size_buffer = screenBuffer

instance Size Bitmap where
   size_buffer = buffer
