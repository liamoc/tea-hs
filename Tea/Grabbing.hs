-- |Includes the Grabbing class for extracting sub-images, its instances, and
--  monadic helper functions.
module Tea.Grabbing ( Grabbing (grab)
                    , grabM
                    , grabM2
                    ) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Sprig as SPG
import Control.Monad.Trans
import Tea.Tea
import Tea.Screen
import Tea.Bitmap

-- |A class instantiated over all types that can be grabbed from
class Grabbing v where
   -- | Blit a section of one Grabbing buffer to another.
   grab :: Grabbing s => v  -- ^ Source buffer
                      -> s  -- ^ Destination buffer
                      -> (Int, Int)  -- ^ Coordinates of region to grab
                      -> Int  -- ^ Width of region to grab
                      -> Int  -- ^ Height of region to grab
                      -> Tea z ()
   grab s ret (x, y) w h = do
                           liftIO $ SPG.pushBlend SPG.CopySrcAlpha
                           liftIO $ SPG.blit src (Just $ SDL.Rect x y w h) (grabbing_buffer ret) (Just $ SDL.Rect 0 0 w h)
                           liftIO SPG.popBlend
                           return ()
                           where src = grabbing_buffer s
   grabbing_buffer :: v -> SDL.Surface

-- | A monadic convenience function for grab that takes a Tea action instead of a source argument
grabM
  :: (Grabbing a, Grabbing s) =>
  Tea z a -> s -> (Int, Int) -> Int -> Int -> Tea z ()
grabM m a b c d = m >>= \m' -> grab m' a b c d
-- | A monadic convenience function for grab that takes a Tea action instead of both the source and destination.
grabM2
  :: (Grabbing a, Grabbing s) =>
  Tea z a -> Tea z s -> (Int, Int) -> Int -> Int -> Tea z ()
grabM2 m a b c d = m >>= \m' -> a >>= \a' -> grab m' a' b c d

instance Grabbing Bitmap where
   grabbing_buffer = buffer

instance Grabbing Screen where
   grabbing_buffer = screenBuffer
