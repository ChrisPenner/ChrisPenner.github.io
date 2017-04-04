--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Data.Foldable
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
        >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" (postCtx tags) (return posts) <>
              constField "title" "All Posts"                <>
              defaultContext

      makeItem ""
          >>= loadAndApplyTemplate "templates/contents.html" indexCtx
          >>= loadAndApplyTemplate "templates/base.html" indexCtx
          >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <>
                  listField "posts" (postCtx tags) (return posts) <>
                  defaultContext

      makeItem "" >>= loadAndApplyTemplate "templates/contents.html" ctx
                  >>= loadAndApplyTemplate "templates/base.html" ctx
                  >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = fold
  [ tagsField "tags" tags
  , dateField "date" "%B %e, %Y"
  -- , tagsContext
  -- , categoriesContext
  , defaultContext
  ]

listContextWith :: Context String -> String -> Context a
listContextWith ctx s = listField s ctx $ do
    identifier <- getUnderlying
    fieldData <- getMetadataField identifier s
    let metas = maybe [] (fmap trim . splitAll ",")  fieldData
    return $ fmap (\x -> Item (fromFilePath x) x) metas

listContext :: String -> Context a
listContext = listContextWith defaultContext

tagsContext :: Context a
tagsContext = listContext "tags"

categoriesContext :: Context a
categoriesContext = listContext "categories"
