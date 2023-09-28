--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Hakyll.Images ( ensureFitCompiler,loadImage)
import Text.Pandoc.Highlighting (Style, tango, styleToCss)
import Text.Pandoc
--------------------------------------------------------------------------------

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

pandocCodeStyle :: Style
pandocCodeStyle = tango

--------------------------------------------------------------------------------
pandocCompilerStyled :: String -> String -> Compiler (Item String)
pandocCompilerStyled cslFileName bibFileName = do
    csl  <- load    $ fromFilePath cslFileName
    bibs <- loadAll $ fromGlob bibFileName
    fmap (writePandocWith wopt)
        (getResourceBody >>= readPandocBiblios ropt csl bibs)
    where ropt = defaultHakyllReaderOptions
            { 
                readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHakyllReaderOptions
            }
          wopt = defaultHakyllWriterOptions
            {
                writerHighlightStyle   = Just pandocCodeStyle
            }

pandocBibCompiler :: Compiler (Item String)
pandocBibCompiler = pandocCompilerStyled "style.csl" "bibliography.bib"

main :: IO ()
main = hakyllWith config $ do

  match "*.bib" $ compile biblioCompiler
  match "*.csl" $ compile cslCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
        makeItem $ styleToCss pandocCodeStyle

  match "logos/**" $ do
    route idRoute
    compile $
      loadImage
        >>= ensureFitCompiler 512 512

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "about.html" $ do
    route idRoute
    compile $ do
      let indexCtx = defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match (fromList ["contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocBibCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Posts"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext
