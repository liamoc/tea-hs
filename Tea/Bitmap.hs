module Tea.Bitmap where
import Tea.BitmapConstructorsInternal
import Tea.Blitting
import Tea.Clipping
import Tea.ImageSaving
import Tea.Size 
import Tea.Primitive
import Tea.Types
import Tea.Grabbing
import Tea.Monad
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Monad.Trans

fromImage :: String -> Tea Bitmap
fromImage s = liftIO $ do
              buf <- SDLI.load s
              buf' <- SDL.displayFormatAlpha buf
              return $ Bitmap { buffer = buf' }              
 

blank :: Int -> Int -> Color -> Tea Bitmap
blank w h c = do
              buf <- liftIO $ _blank w h c
              return (Bitmap buf)
 
instance Blitting Bitmap where
   blitting_buffer = buffer

instance Clipping Bitmap where
   clipping_buffer = buffer

instance Grabbing Bitmap where
   grabbing_buffer = buffer

instance ImageSaving Bitmap where
   image_saving_buffer = buffer

instance Primitive Bitmap where
   primitive_buffer = buffer

instance Size Bitmap where
   size_buffer = buffer

 
