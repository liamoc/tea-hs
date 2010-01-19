{-# LANGUAGE NoMonomorphismRestriction #-} -- I don't generally do this, but it really makes life easier if I do in this case
module Tea.Primitive ( PrimitiveOptions (..)
                     , defaults
                     , Primitive ( rect
                                 , setPixel
                                 , getPixel
                                 , clear
                                 , roundedRect
                                 , line
                                 , fadeLine
                                 , bezier
                                 , circle
                                 , arc
                                 , ellipse
                                 )
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

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Sprig as SPG
import Control.Monad.Trans
import Tea.Tea
import Tea.BlendMode
import Tea.Bitmap
import Tea.Screen
import Tea.Color

data PrimitiveOptions = PrimitiveOptions { mix :: BlendMode
                                         , antialias :: Bool
                                         , filled    :: Bool
                                         , thickness :: Int
                                         } deriving (Show, Eq)

defaults = PrimitiveOptions { mix = DestAlpha, antialias = True, filled = False, thickness = 1 }

colorToPixel surf (Color r g b a) = SDL.mapRGBA (SDL.surfaceGetPixelFormat surf) (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

withColor s c f = colorToPixel s c >>= flip f (fromIntegral $ alpha c)

alpha' = fromIntegral . alpha
line' s x1 y1 x2 y2 c _                 = withColor s c $ SPG.lineBlend s x1 y1 x2 y2
bezier' s x1 y1 x2 y2 x3 y3 x4 y4 q c _ = withColor s c $ SPG.bezierBlend s x1 y1 x2 y2 x3 y3 x4 y4 (fromIntegral q)
rect' s x1 y1 x2 y2 c True              = withColor s c $ SPG.rectFilledBlend s x1 y1 x2 y2
rect' s x1 y1 x2 y2 c False             = withColor s c $ SPG.rectBlend s x1 y1 x2 y2
roundedRect' s x1 y1 x2 y2 r c True     = withColor s c $ SPG.rectRoundFilledBlend s x1 y1 x2 y2 r
roundedRect' s x1 y1 x2 y2 r c False    = withColor s c $ SPG.rectRoundBlend s x1 y1 x2 y2 r
circle' s x y r c True                  = withColor s c $ SPG.circleFilledBlend s x y r
circle' s x y r c False                 = withColor s c $ SPG.circleBlend s x y r
arc' s x y r a1 a2 c True               = withColor s c $ SPG.arcFilledBlend s x y r a1 a2
arc' s x y r a1 a2 c False              = withColor s c $ SPG.arcBlend s x y r a1 a2
ellipse' s x y rx ry c True             = withColor s c $ SPG.ellipseFilledBlend s x y rx ry
ellipse' s x y rx ry c False            = withColor s c $ SPG.ellipseBlend s x y rx ry
fadeLine' s x1 y1 x2 y2 c c2 _          = do c2' <- colorToPixel s c2; c1' <- colorToPixel s c; SPG.lineFadeBlend s x1 y1 x2 y2 c1' (alpha' c) c2' $ alpha' c2

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
   clear :: v -> Tea s Bool
   clear x = liftIO $
             SDL.mapRGBA (SDL.surfaceGetPixelFormat surf) 0 0 0 0 >>=
             SDL.fillRect surf (Just (SDL.Rect 0 0
                                          (SDL.surfaceGetWidth  surf)
                                          (SDL.surfaceGetHeight surf)))
             where surf = primitive_buffer x

   getPixel :: v -> (Int, Int) -> Tea s Color
   getPixel s (x, y) = liftIO $ SPG.getPixel (primitive_buffer s) x y >>= pixelToColor $ primitive_buffer s


   setPixel :: v -> (Int, Int) -> Color -> Tea s ()
   setPixel s (x, y) color = liftIO $ SPG.pixel surf x y =<< colorToPixel surf color
                        where surf = primitive_buffer s

   rect :: v -> (Int, Int) -> Int -> Int -> Color -> PrimitiveOptions -> Tea s ()
   rect s (x,y) w h c opts = liftIO $ withOptions opts $ rect' (primitive_buffer s) x y (x+w) (y+h) c

   roundedRect :: v -> (Int, Int) -> Int -> Int -> Float -> Color -> PrimitiveOptions -> Tea s ()
   roundedRect s (x, y) w h r c opts = liftIO $ withOptions opts $ roundedRect' (primitive_buffer s) x y (x+w) (y+h) r c

   line :: v -> (Int, Int) -> (Int, Int) -> Color -> PrimitiveOptions -> Tea s ()
   line s (x1, y1) (x2, y2) c opts = liftIO $ withOptions opts $ line' (primitive_buffer s) x1 y1 x2 y2 c

   fadeLine :: v -> (Int, Int) -> (Int, Int) -> Color -> Color -> PrimitiveOptions -> Tea s ()
   fadeLine s (x1, y1) (x2, y2) c1 c2 opts = liftIO $ withOptions opts $ fadeLine' (primitive_buffer s) x1 y1 x2 y2 c1 c2

   bezier :: v -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Color -> PrimitiveOptions -> Tea s ()
   bezier s (x1, y1) (x2, y2) (x3, y3) (x4, y4) q c opts = liftIO $ withOptions opts $ bezier' (primitive_buffer s) x1 y1 x2 y2 x3 y3 x4 y4 q c

   circle :: v -> (Int, Int) -> Float -> Color -> PrimitiveOptions -> Tea s ()
   circle s (x, y) r c opts = liftIO $ withOptions opts $ circle' (primitive_buffer s) x y r c

   arc :: v -> (Int, Int) -> Float -> Float -> Float -> Color -> PrimitiveOptions -> Tea s ()
   arc s (x, y) r a1 a2 c opts = liftIO $ withOptions opts $ arc' (primitive_buffer s) x y r a1 a2 c

   ellipse :: v -> (Int, Int) -> Float -> Float -> Color -> PrimitiveOptions -> Tea s ()
   ellipse s (x, y) rx ry c opts = liftIO $ withOptions opts $ ellipse' (primitive_buffer s) x y rx ry c


clearM = (>>= clear)
-- s/\(.*\)M m\(.*\)/\0 = m >>= \\m' -> \1 m' \2/g
rectM m c w h l o = m >>= \m' -> rect m'  c w h l o
setPixelM m c l = m >>= \m' -> setPixel m'  c l
getPixelM m c = m >>= flip getPixel c
roundedRectM m c w h r l o = m >>= \m' -> roundedRect m'  c w h r l o
lineM m c1 c2 l o = m >>= \m' -> line m'  c1 c2 l o
fadeLineM m c1 c2 l1 l2 o = m >>= \m' -> fadeLine m'  c1 c2 l1 l2 o
bezierM m c1 c2 c3 c4 q l o = m >>= \m' -> bezier m'  c1 c2 c3 c4 q l o
circleM m c r l o = m >>= \m' -> circle m'  c r l o
arcM m c r s e l o = m >>= \m' -> arc m'  c r s e l o
ellipseM m c rx ry l o = m >>= \m' -> ellipse m'  c rx ry l o

instance Primitive Bitmap where
   primitive_buffer = buffer

instance Primitive Screen where
   primitive_buffer = screenBuffer
