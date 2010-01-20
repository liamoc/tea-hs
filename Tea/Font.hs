-- | Includes the Font abstract type, its constructor, and helper functions.
module Tea.Font ( Font (..)
                , loadFont
                , wrap
                ) where

import Graphics.UI.SDL.SFont (initFont, textWidth, SFont)
import Graphics.UI.SDL.Image (load)
import Control.Monad.Trans
import Data.List
import Tea.Tea

-- |A data type to represent Bitmap fonts in Tea.
data Font = Font { getSFont :: SFont }

-- |Load a font from an image file in the SFont format. The same image formats
--  as `loadBitmap' are supported.
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

-- | Wrap a string to fit in the specified width, if it were rendered in the given font.
wrap :: Font -> String -> Int -> String
wrap (Font font) str w = init $ unlines $ map (\v -> unlines $ reverse $ wrap' v []) $ lines str
  where
    wrap' [] accum = accum
    wrap' str accum = let possibilities = inits $ wordsRespectingSpaces str
                          sizemap =  zip (map (textWidth font . concat) possibilities) (map length possibilities)
                          amountToTake = case snd $ last $ takeWhile ((< w) . fst) sizemap of
                                           0 -> 1
                                           a -> a
                          (taken, dropped) = partitionAtWord str amountToTake
                      in wrap' dropped $ taken:accum
