module Tea.Primitive 
   ( PrimitiveOptions (..)
   , defaults
   , Primitive (..)
   , rectM
   , setPixelM 
   , getPixelM 
   , clearM
   , roundedRectM
   , lineM
   , fadeLineM 
   , bezierM
   , circleM
   , arcM
   , ellipseM
   ) where

import qualified Graphics.UI.SDL  as SDL hiding(Color)
import qualified Graphics.UI.SDL.Sprig as SPG
import Tea.Monad
import Tea.Types
import Foreign.Storable
import Foreign
import Control.Monad.Trans

data PrimitiveOptions = PrimitiveOptions { mix :: BlendMode
                                         , antialias :: Bool 
                                         , filled    :: Bool 
                                         , thickness :: Int
                                         } deriving (Show, Eq)

defaults = PrimitiveOptions { mix = DestAlpha, antialias = True, filled = False, thickness = 1 }                    

colorToPixel surf (Color r g b a) = SDL.mapRGBA (SDL.surfaceGetPixelFormat surf) (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)


alpha' = fromIntegral . alpha 
line' s x1 y1 x2 y2 c _                  = do c' <- colorToPixel s c; SPG.lineBlend s x1 y1 x2 y2 c' $ alpha' c
fadeLine' s x1 y1 x2 y2 c c2 _           = do c2' <- colorToPixel s c2; c1' <- colorToPixel s c; SPG.lineFadeBlend s x1 y1 x2 y2 c1' (alpha' c) c2' $ alpha' c2
bezier' s x1 y1 x2 y2 x3 y3 x4 y4 q c _  = do c' <- colorToPixel s c; SPG.bezierBlend s x1 y1 x2 y2 x3 y3 x4 y4 (fromIntegral q) c' $ alpha' c
rect' s x1 y1 x2 y2 c True               = do c' <- colorToPixel s c; SPG.rectFilledBlend s x1 y1 x2 y2 c' $ alpha' c
rect' s x1 y1 x2 y2 c False              = do c' <- colorToPixel s c; SPG.rectBlend s x1 y1 x2 y2 c' $ alpha' c
roundedRect' s x1 y1 x2 y2 r c True      = do c' <- colorToPixel s c; SPG.rectRoundFilledBlend s x1 y1 x2 y2 r c' $ alpha' c
roundedRect' s x1 y1 x2 y2 r c False     = do c' <- colorToPixel s c; SPG.rectRoundBlend s x1 y1 x2 y2 r c' $ alpha' c
circle' s x y r c True                   = do c' <- colorToPixel s c; SPG.circleFilledBlend s x y r c' $ alpha' c
circle' s x y r c False                  = do c' <- colorToPixel s c; SPG.circleBlend s x y r c' $ alpha' c
arc' s x y r a1 a2 c True                = do c' <- colorToPixel s c; SPG.arcFilledBlend s x y r a1 a2 c' $ alpha' c
arc' s x y r a1 a2 c False               = do c' <- colorToPixel s c; SPG.arcBlend s x y r a1 a2 c' $ alpha' c
ellipse' s x y rx ry c True              = do c' <- colorToPixel s c; SPG.ellipseFilledBlend s x y rx ry c' $ alpha' c
ellipse' s x y rx ry c False             = do c' <- colorToPixel s c; SPG.ellipseBlend s x y rx ry c' $ alpha' c

withOptions :: PrimitiveOptions -> (Bool -> IO v) -> IO v
withOptions (PrimitiveOptions m a o t) g = do SPG.pushBlend (blendModeToSPG m)
                                              SPG.pushAA a
                                              SPG.pushThickness t
                                              ret <- g o
                                              --SPG.popThickness caused arbitrary segfaults.. can't be too bad, can it?
                                              SPG.popAA
                                              SPG.popBlend
                                              return ret

pixelToColor s p = SDL.getRGBA p (SDL.surfaceGetPixelFormat s) >>= \(r,g,b,a) -> return $ Color (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)


class Primitive v where

   primitive_buffer :: v -> SDL.Surface
   clear :: v -> Tea Bool
   clear x = liftIO $ 
             SDL.mapRGBA (SDL.surfaceGetPixelFormat surf) 0 0 0 0 >>= 
             SDL.fillRect surf (Just (SDL.Rect 0 0 
                                          (SDL.surfaceGetWidth  surf) 
                                          (SDL.surfaceGetHeight surf)))
             where surf = (primitive_buffer x) 

   getPixel :: v -> (Int, Int) -> Tea Color 
   getPixel s (x, y) = liftIO $ SPG.getPixel (primitive_buffer s) x y >>= pixelToColor (primitive_buffer s)


   setPixel :: v -> (Int, Int) -> Color -> Tea ()
   setPixel s (x, y) color = liftIO $ SPG.pixel surf x y =<< (colorToPixel surf color)
                        where surf = (primitive_buffer s)

   rect :: v -> (Int, Int) -> Int -> Int -> Color -> PrimitiveOptions -> Tea ()
   rect s (x,y) w h c opts = liftIO $ withOptions opts $ rect' (primitive_buffer s) x y (x+w) (y+h) c

   roundedRect :: v -> (Int, Int) -> Int -> Int -> Float -> Color -> PrimitiveOptions -> Tea ()
   roundedRect s (x, y) w h r c opts = liftIO $ withOptions opts $ roundedRect' (primitive_buffer s) x y (x+w) (y+h) r c

   line :: v -> (Int, Int) -> (Int, Int) -> Color -> PrimitiveOptions -> Tea ()
   line s (x1, y1) (x2, y2) c opts = liftIO $ withOptions opts $ line' (primitive_buffer s) x1 y1 x2 y2 c

   fadeLine :: v -> (Int, Int) -> (Int, Int) -> Color -> Color -> PrimitiveOptions -> Tea ()
   fadeLine s (x1, y1) (x2, y2) c1 c2 opts = liftIO $ withOptions opts $ fadeLine' (primitive_buffer s) x1 y1 x2 y2 c1 c2

   bezier :: v -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Color -> PrimitiveOptions -> Tea()
   bezier s (x1, y1) (x2, y2) (x3, y3) (x4, y4) q c opts = liftIO $ withOptions opts $ bezier' (primitive_buffer s) x1 y1 x2 y2 x3 y3 x4 y4 q c

   circle :: v -> (Int, Int) -> Float -> Color -> PrimitiveOptions -> Tea ()
   circle s (x, y) r c opts = liftIO $ withOptions opts $ circle' (primitive_buffer s) x y r c
   
   arc :: v -> (Int, Int) -> Float -> Float -> Float -> Color -> PrimitiveOptions -> Tea ()
   arc s (x, y) r a1 a2 c opts = liftIO $ withOptions opts $ arc' (primitive_buffer s) x y r a1 a2 c

   ellipse :: v -> (Int, Int) -> Float -> Float -> Color -> PrimitiveOptions -> Tea ()
   ellipse s (x, y) rx ry c opts = liftIO $ withOptions opts $ ellipse' (primitive_buffer s) x y rx ry c


rectM m a b c d e = m >>= \m' -> rect m'  a b c d e
setPixelM m x = m >>= \m' -> setPixel m' x
getPixelM m x = m >>= \m' -> getPixel m' x
clearM m = m >>= clear
roundedRectM m a b c d e f = m >>= \m' -> roundedRect m' a b c d e f
lineM m a b c d = m >>= \m' -> line a b c d 
fadeLineM m a b c d e = m >>= \m' -> fadeLine m' a b c d e
bezierM m a b c d e f g = m >>= \m' -> bezier m' a b c d e f g
circleM m a b c d = m >>= \m' -> circle m' a b c d
arcM m a b c d e f = m >>= \m' -> arc m' a b c d e f
ellipseM m a b c d e = m >>= \m' -> ellipse a b c d e
