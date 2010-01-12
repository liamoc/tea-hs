module Tea
   ( module Tea.Monad
   , module Tea.Types
   , module Tea.Size
   , module Tea.Sound
   , module Tea.Event
   , module Tea.Primitive
   , module Tea.Blitting
   , module Tea.Clipping
   , module Tea.Grabbing
   , module Tea.Bitmap
   , module Tea.ImageSaving
   , module Tea.Font
   ) where

import Tea.Monad
import Tea.Font
import Tea.Types
import Tea.Size
import Tea.Sound
import Tea.Event
import Tea.Bitmap
import Tea.Screen
import Tea.Primitive
import Tea.Blitting
import Tea.Clipping
import Tea.Grabbing
import Tea.ImageSaving
{-
test_anim x y bmap = if x < 255 && y < 480 then do
                   scr <- screen
                   blit scr bmap 0 0
                   update
                   liftIO $ SDL.delay 10
                   test_anim (x+1) (y+1) bmap
                   return ()
                else
                   return ()

test = runTea 640 480 $ do
                        scr <- screen
                        circle scr 50 50 20 (Color 255 0 255 255) defaults { antialias = False, filled = True, mix = SrcAlpha }
                        bmap <- blank 640 480 (Color 0 0 255 64)
                        circle bmap 50 50 50 (Color 0 255 0 0) defaults { antialias = False, filled = True, mix = SrcAlpha }
                        bmap2 <- grab bmap 0 0 640 480
                        test_anim 0 0 bmap2

-}
