-- |Includes the BlendMode type and internal functions for dealing with Sprig.
module Tea.BlendMode ( BlendMode (..)
                     , blendModeToSPG
                     ) where

import qualified Graphics.UI.SDL.Sprig as SPG
-- |A type to determine how to draw primitives and blit bitmaps, with regards to
--  color and alpha blending.
data BlendMode = DestAlpha
               | SrcAlpha
               | CombineAlpha
               | CopyNoAlpha
               | CopySrcAlpha
               | CopyDestAlpha
               | CopyCombineAlpha
               | CopyAlphaOnly
               | CombineAlphaOnly
               deriving (Show,Eq)

-- |Internal function to convert a Tea BlendMode to a Sprig equivalent. Can only
--  be accessed by directly importing this module.
blendModeToSPG  DestAlpha         = SPG.DestAlpha
blendModeToSPG  SrcAlpha          = SPG.SrcAlpha
blendModeToSPG  CombineAlpha      = SPG.CombineAlpha
blendModeToSPG  CopyNoAlpha       = SPG.CopyNoAlpha
blendModeToSPG  CopySrcAlpha      = SPG.CopySrcAlpha
blendModeToSPG  CopyDestAlpha     = SPG.CopyDestAlpha
blendModeToSPG  CopyCombineAlpha  = SPG.CopyCombineAlpha
blendModeToSPG  CopyAlphaOnly     = SPG.CopyAlphaOnly
blendModeToSPG  CombineAlphaOnly  = SPG.CombineAlphaOnly
