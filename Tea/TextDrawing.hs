module Tea.TextDrawing ( TextDrawing (drawText)
                       , drawTextM
                       ) where

import Graphics.UI.SDL (Surface)
import Graphics.UI.SDL.SFont (textHeight, write)
import Control.Monad.Trans
import Tea.Tea
import Tea.Screen
import Tea.Bitmap
import Tea.Font

class TextDrawing v where
   drawText :: v -> (Int, Int) -> Font -> String -> Tea s ()
   drawText s (x,y) (Font font) str = do mapM (uncurry drawLine) $ zip (lines str) $ map ((+ y) . (*textHeight font)) [1..]; return ()
        where
           drawLine str y = liftIO $ write (text_drawing_buffer s) font (x,y) str
   text_drawing_buffer :: v -> Surface

drawTextM m c f s = m >>= \m' -> drawText m' c f s

instance TextDrawing Bitmap where
   text_drawing_buffer = buffer

instance TextDrawing Screen where
   text_drawing_buffer = screenBuffer
