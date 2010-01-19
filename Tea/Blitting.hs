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

class Blitting v where
   blitBlend :: Blitting s => v -> s -> BlendMode -> (Int, Int) -> Tea s Bool
   blitBlend d s b (x, y) = do
                      liftIO $ SPG.pushBlend $ blendModeToSPG b
                      ret <- blit d s (x,y)
                      liftIO SPG.popBlend
                      return ret

   blit :: Blitting s => v -> s -> (Int, Int) -> Tea s Bool
   blit d s (x, y) = liftIO $ SPG.blit src src_rect dst dst_rect
                where src_rect = Just $ SDL.Rect 0 0 src_w src_h
                      dst_rect = Just $ SDL.Rect x y src_w src_h
                      src_w    = SDL.surfaceGetWidth  src
                      src_h    = SDL.surfaceGetHeight src
                      src      = blitting_buffer s
                      dst      = blitting_buffer d
   blitting_buffer :: v -> SDL.Surface


blitBlendM  d s a b = d >>= \d' -> blitBlend d' s a b
blitBlendM2 d s a b = d >>= \d' -> s >>= \s' -> blitBlend d' s' a b
blitM d s x = d >>= \d' -> blit d' s x
blitM2 d s x = d >>= \d' -> s >>= flip (blit d') x

instance Blitting Bitmap where
   blitting_buffer = buffer

instance Blitting Screen where
   blitting_buffer = screenBuffer
