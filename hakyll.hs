#!/usr/bin/env runhaskell

import Text.Hakyll (hakyll)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.Renderables (createCustomPage)
import Text.Hakyll.File (directory)
import Text.Hakyll.Page (readPage, getBody, getValue)
import System.FilePath
import Control.Applicative((<$>))

main = hakyll $ do
         directory css "css"
         directory static "static"
         flip directory "pages" $ \path -> do 
            if takeExtension path == ".pdc" 
              then do
              page <- readPage path
              renderChain ["templates/default.html"] $
                createCustomPage (takeBaseName path <.> "html") 
                                 [ path , path <.> "side" ]
                                 [ ("body", Left $ getBody page)
                                 , ("sidebar", Right $ getBody <$> readPage (path <.> "side"))
                                 , ("title", Left $ getValue "title" page)
                                 ]
              return ()
              else return ()
