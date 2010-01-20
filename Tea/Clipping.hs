-- | Includes the Clipping class for objects that support clipping rectanges,
--   its instances, and monadic convenience functions.
module Tea.Clipping ( Clipping (clip)
                    , clipM
                    ) where

import Control.Monad.State
import Control.Monad.Trans
import Graphics.UI.SDL (withClipRect, Surface, Rect (..))
import Tea.Tea
import Tea.Screen
import Tea.Bitmap
import Tea.TeaState

withTea :: Tea s a -> s -> TeaState -> IO ((a, s), TeaState)
withTea = (runStateT .) . runStateT . extractTea

-- | A class instantiated over all types that support Clipping rectangles.
class Clipping v where
   -- | Run a specified Tea action where the provided Clipping type has been
   --   clipped to the size provided. The clipping only lasts for the duration
   --   of the action.
   clip :: v          -- ^ Buffer to clip
        -> (Int, Int) -- ^ Left-hand corner of clipping rectangle coordinates
        -> Int        -- ^ Width of clipping rectangle
        -> Int        -- ^ Height of clipping rectangle
        -> Tea s z    -- ^ Tea action to run
        -> Tea s z
   clip surf (x, y) w h m = do
                            scr <- getT
                            s <- get
                            ((v, s'),st') <- liftIO $ withClipRect (clipping_buffer surf) (Just $ Rect x y w h) (withTea m s scr)
                            putT st'
                            put s'
                            return v
   clipping_buffer :: v -> Surface

-- |A convenience version of Clip that takes a Tea action instead of a raw buffer.
clipM :: (Clipping v) => Tea s v -> (Int, Int) -> Int -> Int -> Tea s z
        -> Tea s z
clipM v a b c d  = v >>= \v' -> clip v' a b c d

instance Clipping Screen where
   clipping_buffer = screenBuffer

instance Clipping Bitmap where
   clipping_buffer = buffer
