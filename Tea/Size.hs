module Tea.Size ( Size (..)
                , heightM
                , widthM 
                ) where
import Graphics.UI.SDL

class Size v where
   width  :: v -> Int
   width  = surfaceGetWidth  . size_buffer
   height :: v -> Int
   height = surfaceGetHeight . size_buffer
   size_buffer :: v -> Surface

widthM m = m >>= return . width
heightM m = m >>= return . height
