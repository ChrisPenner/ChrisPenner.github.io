--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Data.List
import Data.Foldable
import Data.String
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
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

  tags <- buildTags "posts/*" (fromCapture "tags/*")
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
        >>= loadAndApplyTemplate "templates/base.html" (postCtx tags)
        >>= relativizeUrls
        >>= stripHTMLSuffix

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
          >>= stripHTMLSuffix

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
                  >>= stripHTMLSuffix

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
stripHTMLSuffix :: Item String -> Compiler (Item String)
stripHTMLSuffix = return . fmap (withUrls stripSuffix)
  where stripSuffix x 
        | isSuffixOf ".html" x = reverse . drop 5 . reverse $ x
        | otherwise = x

postCtx :: Tags -> Context String
postCtx tags = fold
  [ --tagsField "tags" tags
  tagsFieldWith getTags makeLink fold "tags" tags
  -- field "tags" $ \i -> getTags (itemIdentifier i) >>= renderTags makeLink concat
  -- tagCloudFieldWith "tags" rendTag (++) 1.0 1.0 tags
                    -- 
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]
    where 
      makeLink tag (Just url) = Just $ H.a (fromString tag) ! A.class_ "tag" ! A.href (fromString ("/" ++ url))
      makeLink _  Nothing = Nothing
