-- |Includes the Color type.
module Tea.Color ( Color (..)
                 ) where
-- |A type to represent RGBA colors in Tea.
data Color = Color { red :: Int, green :: Int, blue :: Int, alpha :: Int}
           deriving (Show, Eq)
