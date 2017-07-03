{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import Data.Text.Lens
import Data.Time.Format
import Data.Time.Clock
import qualified Data.Text as T
import qualified Text.Mustache as MT
import qualified Text.Mustache.Types as MT

author, domain, blogTitle :: T.Text
domain = "http://chrispenner.ca"
author = "Chris Penner"
blogTitle = "Chris Penner"

main :: IO ()
main = siteWithGlobals funcs $ do
  posts <- resourceLoader markdownReader ["posts/*.md"]
  let tags = getTags id posts
  writeTemplate "templates/index.html" [mkIndexEnv posts tags]
  writeTemplate "templates/post.html" (over (key "tags" . _Array . traverse) stripHTMLSuffix <$> posts)
  writeTemplate "templates/tag.html" (stripPostsHTMLSuffix <$> tags)
  staticAssets
  atomRssFeed posts

funcs :: MT.Value
funcs = MT.object
  ["truncate" MT.~> MT.overText (T.take 30)
  ]

stripHTMLSuffix :: Value -> Value
stripHTMLSuffix obj = obj
  & key "url" . _String . unpacked %~ setExt ""

stripPostsHTMLSuffix :: Value -> Value
stripPostsHTMLSuffix tag = tag
  & key "posts" . _Array . traversed . key "url" . _String . unpacked %~ setExt ""

mkIndexEnv :: [Value] -> [Value] -> Value
mkIndexEnv posts tags =
  object [ "posts" .= (stripHTMLSuffix <$> posts)
         , "tags" .= (stripHTMLSuffix <$> tags)
         , "url" .= ("/index.html" :: T.Text)
         ]

staticAssets :: SiteM ()
staticAssets = copyFiles
    [ "css/*.css"
    , "js/"
    , "images/"
    ]

atomRssFeed :: [Value] -> SiteM ()
atomRssFeed posts = do
  now <- liftIO getCurrentTime
  let formatString = iso8601DateFormat (Just "%H:%M:%SZ")
      formattedTime = formatTime defaultTimeLocale formatString now
      atomEnv = object [ "title" .= blogTitle
                       , "domain" .= domain
                       , "author" .= author
                       , "posts" .= posts
                       , "currentTime" .= formattedTime
                       , "url" .= ("/atom.xml" :: T.Text)
                       ]

  writeTemplate "templates/atom.xml" [atomEnv]
