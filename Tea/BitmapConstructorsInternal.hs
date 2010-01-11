module Tea.BitmapConstructorsInternal where
import Tea.Types
import qualified Graphics.UI.SDL as SDL
import Control.Monad.Trans

_blank :: Int -> Int -> Color -> IO SDL.Surface
_blank w h (Color r g b a)  = do buf <- SDL.createRGBSurface [SDL.SWSurface] w h 32 rmask gmask bmask amask
                                 SDL.fillRect buf (Just (SDL.Rect 0 0 w h)) =<< SDL.mapRGBA (SDL.surfaceGetPixelFormat buf) (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)  
                                 return buf
  where rmask = 0xff000000
        gmask = 0x00ff0000
        bmask = 0x0000ff00
        amask = 0x000000ff
 
