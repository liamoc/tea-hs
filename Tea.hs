module Tea
   ( Color      (..)
   , BlendMode  (..)
   , KeyCode    (..)
   , Mod        (..)
   , Button     (..)
   , Event      (..) 
   , EventQuery (..)
   , Screen 
   , Bitmap 
   , Tea 
   , module Tea.Monad
   , module Tea.Sound
   , module Tea.Bitmap
   , module Tea.Event
   , module Tea.Size
   , module Tea.Font
   , module Tea.Primitive
   , module Tea.Blitting
   , module Tea.Clipping
   , module Tea.Grabbing
   , module Tea.ImageSaving
   ) where

--universally used modules
import Tea.Monad 
import Tea.Types
--specific concerns
import Tea.Font
import Tea.Sound
import Tea.Event
-- graphics object mixins
import Tea.Size        hiding (size_buffer)
import Tea.Primitive   hiding (primitive_buffer)
import Tea.Blitting    hiding (blitting_buffer)
import Tea.Clipping    hiding (clipping_buffer)
import Tea.Grabbing    hiding (grabbing_buffer)
import Tea.ImageSaving hiding (image_saving_buffer)
-- mixin inheritors
import Tea.Bitmap
import Tea.Screen
