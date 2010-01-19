module Tea.BlendMode ( BlendMode (..)
                     , blendModeToSPG
                     ) where

import qualified Graphics.UI.SDL.Sprig as SPG
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


blendModeToSPG  DestAlpha         = SPG.DestAlpha
blendModeToSPG  SrcAlpha          = SPG.SrcAlpha
blendModeToSPG  CombineAlpha      = SPG.CombineAlpha
blendModeToSPG  CopyNoAlpha       = SPG.CopyNoAlpha
blendModeToSPG  CopySrcAlpha      = SPG.CopySrcAlpha
blendModeToSPG  CopyDestAlpha     = SPG.CopyDestAlpha
blendModeToSPG  CopyCombineAlpha  = SPG.CopyCombineAlpha
blendModeToSPG  CopyAlphaOnly     = SPG.CopyAlphaOnly
blendModeToSPG  CombineAlphaOnly  = SPG.CombineAlphaOnly
