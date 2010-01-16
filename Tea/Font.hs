module Tea.Font
  ( Font
  , loadFont
  , wrap
  , drawText
  ) where
import Tea.Blitting
import Tea.Types
import Graphics.UI.SDL.SFont
import Graphics.UI.SDL.Image
import Control.Monad.Trans
import Data.List

data Font = Font SFont

loadFont :: String -> Tea s Font
loadFont str = do a <- liftIO $ load str
                  b <- liftIO $ initFont a
                  return (Font b)


partitionAtWord :: String -> Int -> (String, String)
partitionAtWord str i = partitionAtWord' ' ' i str []
  where
   partitionAtWord' _ _ [] taken = (reverse taken, [])
   partitionAtWord' ' ' 0 rest  (x:xs) = (reverse xs ,x:rest)
   partitionAtWord' ' ' 0 rest  [] = ([],rest)
   partitionAtWord' ' ' x (' ':xs) taken = partitionAtWord' ' ' x xs (' ':taken)
   partitionAtWord' ' ' x (v:xs) taken = partitionAtWord' v (x-1) xs (v:taken)
   partitionAtWord' _   x (v:xs) taken = partitionAtWord' v x xs (v:taken)

wordsRespectingSpaces :: String -> [String]
wordsRespectingSpaces [] = []
wordsRespectingSpaces str = let (f,s) = partitionAtWord str 1 in f:wordsRespectingSpaces s

wrap (Font font) str w = init $ unlines $ map (\v -> unlines $ reverse $ wrap' v []) $ lines $ str
  where  
    wrap' [] accum = accum
    wrap' str accum = let possibilities = inits $ wordsRespectingSpaces $ str
                          sizemap =  zip (map (textWidth font . concat) possibilities) (map length possibilities)
                          amountToTake = case snd $ last $ takeWhile ((< w) . fst) sizemap of
                                           0 -> 1
                                           a -> a
                          (taken, dropped) = partitionAtWord str amountToTake
                      in wrap' dropped $ taken:accum

drawText :: Blitting a => a -> (Int, Int) -> Font -> String -> Tea s ()
drawText s (x,y) (Font font) str = do mapM (uncurry drawLine) $ zip (lines str) $ map ((+ y) . (*textHeight font)) [1..]; return ()
     where
        drawLine str y = liftIO $ write (blitting_buffer s) font (x,y) str

drawTextM m c f s = m >>= \m' -> drawText m' c f s
