-- | Includes the Bitmap abstract type and its constructors.
module Tea.Bitmap ( Bitmap (..)
               -- * Bitmap Constructors
                  , loadBitmap
                  , blankBitmap
                  ) where
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image (load)
import Control.Monad.Trans
import Tea.Tea
import Tea.Color

_blank :: Int -> Int -> Color -> IO SDL.Surface
_blank w h (Color r g b a)  = do buf <- SDL.createRGBSurface [SDL.SWSurface] w h 32 rmask gmask bmask amask
                                 SDL.fillRect buf (Just (SDL.Rect 0 0 w h)) =<< SDL.mapRGBA (SDL.surfaceGetPixelFormat buf) (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
                                 return buf
  where rmask = 0xff000000
        gmask = 0x00ff0000
        bmask = 0x0000ff00
        amask = 0x000000ff

-- |A Bitmap is a non-screen surface that can do everything the Screen can, except
--  cannot be drawn directly to hardware.
data Bitmap = Bitmap { buffer :: SDL.Surface }

-- |Load a bitmap from an image file. Can be TGA, BMP, PNM, XPM, XCF, PCX, GIF,
--  JPG, TIF, LBM or PNG.
loadBitmap :: String -> Tea s Bitmap
loadBitmap s = liftIO $ do
              buf  <- load s
              buf' <- SDL.displayFormatAlpha buf
              return Bitmap { buffer = buf' }

-- |Create a new bitmap consisting entirely of a single color
blankBitmap :: Int -- ^ Width
            -> Int -- ^ Height
            -> Color -- ^ Color to fill
            -> Tea s Bitmap
blankBitmap w h c = liftIO $ do
              buf <- _blank w h c
              buf' <- SDL.displayFormatAlpha buf
              return (Bitmap buf')
