-- | Tea is a library that makes it easier to make games in Haskell.
-- See http://liamoc.github.com/tea-hs for more information.
module Tea
   ( module Tea.Tea
   , module Tea.Display
   , module Tea.Color
   , module Tea.BlendMode
   , module Tea.Input
   , module Tea.Font
   , module Tea.Sound
   , module Tea.Event
   , module Tea.Bitmap
   , module Tea.Screen
   , module Tea.Size
   , module Tea.Primitive
   , module Tea.Blitting
   , module Tea.Clipping
   , module Tea.Grabbing
   , module Tea.ImageSaving
   ) where

-- common types
import Tea.Tea (Tea)
import Tea.Display
import Tea.Color
import Tea.BlendMode hiding (blendModeToSPG)
import Tea.Input     hiding (sdlKey, sdlButton, sdlMod)
import Tea.Font      hiding (getSFont, Font(Font))
import Tea.Sound
import Tea.Event
-- graphics object mixins
import Tea.Size
import Tea.Primitive
import Tea.Blitting
import Tea.Clipping
import Tea.Grabbing
import Tea.ImageSaving
import Tea.TextDrawing
-- mixin inheritors
import Tea.Bitmap hiding (buffer)
import Tea.Screen hiding (screenBuffer)
