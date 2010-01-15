module Tea.Screen where
import Tea.Blitting
import Tea.Clipping
import Tea.ImageSaving
import Tea.Size 
import Tea.Primitive
import Tea.Types
import Tea.Grabbing
import Tea.Monad
import qualified Graphics.UI.SDL as SDL
import Control.Monad.Trans

instance Blitting Screen where
   blitting_buffer = screenBuffer

instance Clipping Screen where
   clipping_buffer = screenBuffer

instance Grabbing Screen where
   grabbing_buffer = screenBuffer

instance ImageSaving Screen where
   image_saving_buffer = screenBuffer

instance Primitive Screen where
   primitive_buffer = screenBuffer

instance Size Screen where
   size_buffer = screenBuffer

 

