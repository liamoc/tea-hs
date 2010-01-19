module Tea.Color ( Color (..)
                 ) where

data Color = Color { red :: Int, green :: Int, blue :: Int, alpha :: Int}
           deriving (Show, Eq)
