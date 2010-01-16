#!/usr/bin/runhaskell
import Text.Hakyll (hakyll)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.Renderables (createPagePath)
import Text.Hakyll.File (directory)

main = hakyll $ do
         directory css "css"
         directory static "static"
         directory render "pages"         
  where render = renderChain ["templates/default.html"] . createPagePath
