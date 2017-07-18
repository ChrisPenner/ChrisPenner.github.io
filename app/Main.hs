{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import Data.Text.Lens
import Data.Time
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Text as T
import qualified Text.Mustache as MT
import qualified Text.Mustache.Types as MT

author, domain, blogTitle :: T.Text
domain = "http://chrispenner.ca"
author = "Chris Penner"
blogTitle = "Chris Penner"

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%S"

main :: IO ()
main = siteWithGlobals funcs $ do
  posts <- sortByDate . fmap formatDate <$> resourceLoader markdownReader ["posts/*.md"]
  liftIO $ print posts
  let tags = getTags (setExt "html" . addPrefix "/tag/") posts
  writeTemplate "templates/index.html" [mkIndexEnv posts tags]
  writeTemplate "templates/post.html" (over (key "tags" . _Array . traverse) stripHTMLSuffix <$> posts)
  writeTemplate "templates/tag.html" (stripPostsHTMLSuffix <$> tags)
  atomRssFeed posts
  staticAssets

funcs :: MT.Value
funcs = MT.object
  ["truncate" MT.~> MT.overText (T.take 30)
  ]

sortByDate :: [Value] -> [Value]
sortByDate = sortBy (flip compareDates)
  where
    compareDates = compare `on` view (key "date" . _String)

stripHTMLSuffix :: Value -> Value
stripHTMLSuffix obj = obj
  & key "url" . _String . unpacked %~ setExt ""

stripPostsHTMLSuffix :: Value -> Value
stripPostsHTMLSuffix tag = tag
  & key "posts" . _Array . traversed . key "url" . _String . unpacked %~ setExt ""

formatDate :: Value -> Value
formatDate post = post
  & _Object . at "date" ?~ String (T.pack isoDate)
  & _Object . at "humanDate" ?~ String (T.pack humanDate)
    where
      humanDate = post ^?! key "date" . _String . unpacked
      parsedTime = parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime
      isoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339) parsedTime

mkIndexEnv :: [Value] -> [Value] -> Value
mkIndexEnv posts tags =
  object [ "posts" .= (stripHTMLSuffix <$> posts)
         , "tags" .= (stripHTMLSuffix <$> tags)
         , "url" .= ("/index.html" :: T.Text)
         ]

staticAssets :: SiteM ()
staticAssets = copyFiles
    [ "css"
    , "js"
    , "images"
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
