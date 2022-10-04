--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

-- see https://rebeccaskinner.net/posts/2021-01-31-hakyll-syntax-highlighting.html
pandocCodeStyle :: Style
pandocCodeStyle = tango

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.md"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match (fromList ["tutorials.md", "examples.md"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

  -- match "posts/*" $ do
  --   route $ setExtension "html"
  --   compile $
  --     pandocCompiler
  --       >>= loadAndApplyTemplate "templates/post.html" postCtx
  --       >>= loadAndApplyTemplate "templates/default.html" postCtx
  --       >>= relativizeUrls

  -- create ["archive.html"] $ do
  --   route idRoute
  --   compile $ do
  --     posts <- recentFirst =<< loadAll "posts/*"
  --     let archiveCtx =
  --           listField "posts" postCtx (return posts)
  --             `mappend` constField "title" "Archives"
  --             `mappend` defaultContext

  --     makeItem ""
  --       >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
  --       >>= loadAndApplyTemplate "templates/default.html" archiveCtx
  --       >>= relativizeUrls

  -- match "index.html" $ do
  --   route idRoute
  --   compile $ do
  --     posts <- recentFirst =<< loadAll "posts/*"
  --     let indexCtx =
  --           listField "posts" postCtx (return posts)
  --             `mappend` defaultContext

  -- getResourceBody
  --   >>= applyAsTemplate indexCtx
  --   >>= loadAndApplyTemplate "templates/default.html" indexCtx
  --   >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext
