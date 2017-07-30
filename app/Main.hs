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
rfc3339 = Just "%H:%M:%SZ"

main :: IO ()
main = siteWithGlobals funcs $ do
  posts <- sortByDate . fmap formatDate <$> resourceLoader markdownReader ["posts/*.md"]
  drafts <- sortByDate . fmap formatDate <$> resourceLoader markdownReader ["drafts/*.md"]
  let tags = getTags (setExt "html" . addPrefix "/tag/") posts
  writeTemplate "templates/index.html" [mkIndexEnv posts tags]
  writeTemplate "templates/post.html" (over (key "tags" . _Array . traverse) stripHTMLSuffix <$> posts)
  writeTemplate "templates/post.html" (over (key "tags" . _Array . traverse) stripHTMLSuffix <$> drafts)
  writeTemplate "templates/tag.html" (stripPostsHTMLSuffix <$> tags)
  atomRssFeed posts
  staticAssets

funcs :: MT.Value
funcs = MT.object
  ["truncate" MT.~> MT.overText (T.take 30)
  ,"stripExt" MT.~> MT.overText (T.pack . setExt "" . T.unpack)
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
  & _Object . at "date" ?~ String (T.pack (toIsoDate parsedTime))
  & _Object . at "humanDate" ?~ String (T.pack humanDate)
    where
      humanDate = post ^?! key "date" . _String . unpacked
      parsedTime = parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

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
  let atomEnv = object [ "title" .= blogTitle
                       , "domain" .= domain
                       , "author" .= author
                       , "posts" .= posts
                       , "currentTime" .= toIsoDate now
                       , "url" .= ("/atom.xml" :: T.Text)
                       ]

  writeTemplate "templates/atom.xml" [atomEnv]