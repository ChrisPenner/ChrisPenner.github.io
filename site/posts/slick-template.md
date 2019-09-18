---
title: "Slick 1.0 Release - Now with a quick and easy template!"
author: "Chris Penner"
date: "Sep 18, 2019"
tags: ["haskell"]
description: "Build a static site using Haskell, Shake and Pandoc!"
image: "slick.jpg"
---

TLDR; Build a site with [slick 1.0](https://github.com/ChrisPenner/slick): [fork the slick-template](https://github.com/ChrisPenner/slick-template).

Hey folks! Slick has been around for a while already, it's a light wrapper over Shake which allows for blazing fast static site builds! It provides Pandoc helpers to load in pages or posts as markdown, or ANYTHING that Pandoc can read (which is pretty much EVERYTHING nowadays). It offers support for Mustache templates as well!

Shake was always great as a build tool, but its Makefile-style of dependency targets was always a little backwards for building a site. Slick 1.0 switches to recommending using Shake's FORWARD discoverable build style. This means you can basically write normal Haskell code in the Action monad to build and render your site, and Shake will **automagically** cache everything for you with proper and efficient cache-busting! A dream come true.

Slick lets you build and deploy a static website using Github Pages (or literally any static file host) very easily while still maintaining completely open for extensibility. You can use any shake compatible lib, or even just IO if you want; Shake's forward build tools can even detect caching rules when running arbitrary external processes (caveat emptor).

Hope you like it! In case you're curious what a site might be like; this very blog is built with slick!

Here's a full snippet of code for building a simple blog (with awesome caching) from markdown files; check it out:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick
import qualified Data.Text                  as T

outputFolder :: FilePath
outputFolder = "docs/"

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  -- Convert the metadata into a Post object
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  copyStaticFiles

-- | Kick it all off
main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty}
  shakeArgsForward shOpts buildRules
```

See you next time!
