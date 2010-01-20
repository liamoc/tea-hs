{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Includes the Primitive class for drawing primitive concerns, its instances
--   and monadic convenience functions.
module Tea.Primitive ( -- * Primitive Drawing Functions
                       Primitive ( rect
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
                  -- * Drawing options
                     , PrimitiveOptions (..)
                     , defaults
                  -- * Monadic convenience functions
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

-- |A type representing less common options used when drawing Primitives.
--  Typically you would override defaults rather than use this type directly.
data PrimitiveOptions = PrimitiveOptions { mix :: BlendMode   -- ^ Which blendmode to use when drawing (default is DestAlpha)
                                         , antialias :: Bool  -- ^ Whether or not to antialias lines (default is True)
                                         , filled    :: Bool  -- ^ Whether or not to fill the shape (if possible) (default is False)
                                         , thickness :: Int   -- ^ The line thickness in pixels, if the shape is not filled (default is 1)
                                         } deriving (Show, Eq)
-- | A default set of PrimitiveOptions.
defaults :: PrimitiveOptions
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
   -- |Clear the whole buffer with RGBA 0,0,0,0
   clear :: v -> Tea s Bool
   clear x = liftIO $
             SDL.mapRGBA (SDL.surfaceGetPixelFormat surf) 0 0 0 0 >>=
             SDL.fillRect surf (Just (SDL.Rect 0 0
                                          (SDL.surfaceGetWidth  surf)
                                          (SDL.surfaceGetHeight surf)))
             where surf = primitive_buffer x
   -- |Get the pixel color value at the specified coordinates
   getPixel :: v -> (Int, Int) -> Tea s Color
   getPixel s (x, y) = liftIO $ SPG.getPixel (primitive_buffer s) x y >>= pixelToColor (primitive_buffer s)

   -- |Set the pixel color value at the specified coordinates
   setPixel :: v -> (Int, Int) -> Color -> Tea s ()
   setPixel s (x, y) color = liftIO $ SPG.pixel surf x y =<< colorToPixel surf color
                        where surf = primitive_buffer s

   -- |Draw a rectangle
   rect :: v                 -- ^ Buffer to draw to
        -> (Int, Int)        -- ^ Coordinates of the top-left corner of the
                             --   rectangle
        -> Int               -- ^ Width of the rectangle
        -> Int               -- ^ Height of the rectangle
        -> Color             -- ^ Color of the rectangle
        -> PrimitiveOptions  -- ^ Other drawing options
        -> Tea s ()
   rect s (x,y) w h c opts = liftIO $ withOptions opts $ rect' (primitive_buffer s) x y (x+w) (y+h) c

   -- |Same as rectangle, except also takes a Float value for the radius by
   --  which to round the corners
   roundedRect :: v -> (Int, Int) -> Int -> Int -> Float -> Color -> PrimitiveOptions -> Tea s ()
   roundedRect s (x, y) w h r c opts = liftIO $ withOptions opts $ roundedRect' (primitive_buffer s) x y (x+w) (y+h) r c

   -- |Draw a line of the specified color between two coordinates.
   line :: v -> (Int, Int) -> (Int, Int) -> Color -> PrimitiveOptions -> Tea s ()
   line s (x1, y1) (x2, y2) c opts = liftIO $ withOptions opts $ line' (primitive_buffer s) x1 y1 x2 y2 c

   -- |Same as line, except takes an extra color for a gradient effect.
   fadeLine :: v -> (Int, Int) -> (Int, Int) -> Color -> Color -> PrimitiveOptions -> Tea s ()
   fadeLine s (x1, y1) (x2, y2) c1 c2 opts = liftIO $ withOptions opts $ fadeLine' (primitive_buffer s) x1 y1 x2 y2 c1 c2

   -- |Draw a bezier curve
   bezier :: v                 -- ^ Buffer to draw to
          -> (Int, Int)        -- ^ Start coordinate
          -> (Int, Int)        -- ^ First control point
          -> (Int, Int)        -- ^ Second control point
          -> (Int, Int)        -- ^ End coordinate
          -> Int               -- ^ Quality (number of intermediate points).
                               --   4-7 is normal.
          -> Color             -- ^ Color of the line
          -> PrimitiveOptions  -- ^ Other drawing options
          -> Tea s ()
   bezier s (x1, y1) (x2, y2) (x3, y3) (x4, y4) q c opts = liftIO $ withOptions opts $ bezier' (primitive_buffer s) x1 y1 x2 y2 x3 y3 x4 y4 q c

   -- |Draw a circle, at the specified point, with the specified radius, in
   --  the specified colour.
   circle :: v -> (Int, Int) -> Float -> Color -> PrimitiveOptions -> Tea s ()
   circle s (x, y) r c opts = liftIO $ withOptions opts $ circle' (primitive_buffer s) x y r c
   -- |Draw an arc
   arc :: v                 -- ^ Buffer to draw to
       -> (Int, Int)        -- ^ Point of the center of the arc's circle
       -> Float             -- ^ Radius of the arc's circle
       -> Float             -- ^ Start angle (in degrees)
       -> Float             -- ^ Stop angle (in degrees)
       -> Color             -- ^ Color of the arc
       -> PrimitiveOptions  -- ^ Other drawing options
       -> Tea s ()
   arc s (x, y) r a1 a2 c opts = liftIO $ withOptions opts $ arc' (primitive_buffer s) x y r a1 a2 c

   -- |Draw an ellipse
   ellipse :: v                 -- ^ Buffer to draw to
           -> (Int, Int)        -- ^ Coordinates of the center
           -> Float             -- ^ X axis radius
           -> Float             -- ^ Y axis radius
           -> Color             -- ^ Color of the ellipse
           -> PrimitiveOptions  -- ^ Other drawing options
           -> Tea s ()
   ellipse s (x, y) rx ry c opts = liftIO $ withOptions opts $ ellipse' (primitive_buffer s) x y rx ry c

clearM :: (Primitive a) => Tea s a -> Tea s Bool
clearM = (>>= clear)
-- s/\(.*\)M m\(.*\)/\0 = m >>= \\m' -> \1 m' \2/g
rectM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> Int
     -> Int
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
rectM m c w h l o = m >>= \m' -> rect m'  c w h l o
setPixelM
  :: (Primitive a) => Tea s a -> (Int, Int) -> Color -> Tea s ()
setPixelM m c l = m >>= \m' -> setPixel m'  c l
getPixelM :: (Primitive a) => Tea s a -> (Int, Int) -> Tea s Color
getPixelM m c = m >>= flip getPixel c
roundedRectM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> Int
     -> Int
     -> Float
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
roundedRectM m c w h r l o = m >>= \m' -> roundedRect m'  c w h r l o
lineM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> (Int, Int)
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
lineM m c1 c2 l o = m >>= \m' -> line m'  c1 c2 l o
fadeLineM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> (Int, Int)
     -> Color
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
fadeLineM m c1 c2 l1 l2 o = m >>= \m' -> fadeLine m'  c1 c2 l1 l2 o
bezierM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> (Int, Int)
     -> (Int, Int)
     -> (Int, Int)
     -> Int
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
bezierM m c1 c2 c3 c4 q l o = m >>= \m' -> bezier m'  c1 c2 c3 c4 q l o
circleM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> Float
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
circleM m c r l o = m >>= \m' -> circle m'  c r l o
arcM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> Float
     -> Float
     -> Float
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
arcM m c r s e l o = m >>= \m' -> arc m'  c r s e l o
ellipseM
  :: (Primitive a) =>
     Tea s a
     -> (Int, Int)
     -> Float
     -> Float
     -> Color
     -> PrimitiveOptions
     -> Tea s ()
ellipseM m c rx ry l o = m >>= \m' -> ellipse m'  c rx ry l o

instance Primitive Bitmap where
   primitive_buffer = buffer

instance Primitive Screen where
   primitive_buffer = screenBuffer
