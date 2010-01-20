-- | Includes the Blitting class for blittable objects, its instances, and
--   monadic convenience functions.
module Tea.Blitting ( Blitting (blit, blitBlend)
                    , blitBlendM
                    , blitBlendM2
                    , blitM
                    , blitM2
                    ) where

import qualified Graphics.UI.SDL.Sprig as SPG
import qualified Graphics.UI.SDL as SDL
import Control.Monad.Trans
import Tea.Screen
import Tea.Bitmap
import Tea.Tea
import Tea.BlendMode

-- |A class that is instantiated on all types that support blitting operations.
class Blitting v where
   -- |Same as blit, except includes a BlendMode to govern blending method.
   blitBlend :: Blitting s => v -> s -> BlendMode -> (Int, Int) -> Tea z Bool
   blitBlend d s b (x, y) = do
                      liftIO $ SPG.pushBlend $ blendModeToSPG b
                      ret <- blit d s (x,y)
                      liftIO SPG.popBlend
                      return ret
   -- |Blit one Blitting type onto another.
   blit :: Blitting s => v -- ^ Destination buffer
                      -> s -- ^ Source buffer
                      -> (Int, Int) -- ^ Coordinates to blit to
                      -> Tea z Bool
   blit d s (x, y) = liftIO $ SPG.blit src src_rect dst dst_rect
                where src_rect = Just $ SDL.Rect 0 0 src_w src_h
                      dst_rect = Just $ SDL.Rect x y src_w src_h
                      src_w    = SDL.surfaceGetWidth  src
                      src_h    = SDL.surfaceGetHeight src
                      src      = blitting_buffer s
                      dst      = blitting_buffer d
   blitting_buffer :: v -> SDL.Surface

-- |A convenience function for blitBlend that takes a Tea action rather than a
--  Blitting type for the destination.
blitBlendM
  :: (Blitting a, Blitting s) =>
     Tea z a -> s -> BlendMode -> (Int, Int) -> Tea z Bool
blitBlendM  d s a b = d >>= \d' -> blitBlend d' s a b
-- |A convenience function for blitBlend that takes a Tea action rather than a
--  Blitting type for the source and destination
blitBlendM2
  :: (Blitting a, Blitting s) =>
     Tea z a -> Tea z s -> BlendMode -> (Int, Int) -> Tea z Bool
blitBlendM2 d s a b = d >>= \d' -> s >>= \s' -> blitBlend d' s' a b
-- |A convenience function for blit that takes a Tea action rather than a
--  Blitting type for the destination
blitM
  :: (Blitting a, Blitting s) =>
     Tea z a -> s -> (Int, Int) -> Tea z Bool
blitM d s x = d >>= \d' -> blit d' s x
-- |A convenience function for blit that takes a Tea action rather than a
--  Blitting type for the source and destination
blitM2
  :: (Blitting a, Blitting s) =>
     Tea z a -> Tea z s -> (Int, Int) -> Tea z Bool
blitM2 d s x = d >>= \d' -> s >>= flip (blit d') x

instance Blitting Bitmap where
   blitting_buffer = buffer

instance Blitting Screen where
   blitting_buffer = screenBuffer
