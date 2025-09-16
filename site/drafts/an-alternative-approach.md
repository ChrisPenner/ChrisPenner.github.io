---
title: "Alternative Effect Sequencing Techniques"
author: Chris Penner
date: Aug 14, 2025
tags: [programming, haskell]
description: "Monads are _one_ way to sequence effects, but they're not the only way!"
image: pipes.jpg
---

## The missing piece

There's a place even further towards the expressive end of the continuum, that still
maintains enough structure to perform useful static analysis, it's occupied by a
structure which requires all possible effects to be declared (and statically analyzable) while
still allowing downstream effects to depend on those that come before.

Allow me to demonstrate what I mean. Let's build a teensy tiny command line.
We'll need a few effects:

```haskell
-- | These are the commands the user might enter
data Command =
  Cat FilePath
  Copy FilePath FilePath

-- | Here's the interface we need from any system which wants to implement our DSL
class Monad m => MonadCommand m where
  getCommand :: m Command
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
  print :: String -> m ()

-- | How we run commands.
-- I'll be very verbose with binds here so it's clear where the lambdas are.
runCommand :: MonadCommand m => m ()
runCommand = do
  getCommand >>= \cmd ->
    case cmd of
      Cat fp ->
        readFile fp >>= \contents ->
          print contents
      Copy source dest -> do
        readFile source >>= \contents ->
          writeFile dest contents
```

Let's say we now want to analyze our `runCommand` function, specifically we want to know



Now let's build a static site generator:

```haskell
class Monad m => MonadMake m where
  readFile :: FilePath -> m String
```

Most `make` systems work declaring your dependency tree, then you request that a given resource be built, and the system will traverse the dependency tree to determine which parts of the tree need to be rebuilt to make that happen.
However there are also many systems which work in reverse, you provide a set of resources and the build system will produce a set of output resources _base on_ the inputs to the system.

* We can trim the graph and only recompute the branches which lead to the output we want.
* We can detect that some resource in the middle of our graph changed, and propagate that change forward through the
  graph without needing to recompute the graph prefix.



# An Alternative Approach

What if I told you that we already had a rigorous and mathematically sound set of structures which
span a similar continuum like that between Functor <-> Monad, but which also
provide a structured representation of the flow of data through the system and
even have an _almost_ complete implementation of syntactic sugar in Haskell?

It's the Category hierarchy!

Code speaks louder than words, so please allow me to demonstrate that building our
programs using the Category hierarchy can gain us _at least_ as much expressivity
as Monads do.
How about I prove it by re-implementing the IO monad on top of the Category class.

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Category.IO (IOCat (..), fromIO) where

import Control.Category (Category (..))
import GHC.Base (IO (..), RealWorld, State#)

data IOCat i o = IOCat ((# (State# RealWorld), i #) -> (# State# RealWorld, o #))
  deriving (Functor)

instance Category IOCat where
  id = IOCat (\(# rw, i #) -> (# rw, i #))
  (IOCat f) . (IOCat g) =
    IOCat $ \(# rw, i #) ->
      let !(# rw', o #) = g (# rw, i #)
          !(# rw'', i' #) = f (# rw', o #)
       in (# rw'', i' #)

fromIO :: IO () -> IOCat () ()
fromIO (IO f) = IOCat $ \(# rw, () #) ->
  let !(# rw', () #) = f rw
   in (# rw', () #)

-- Just to stunt on Monadic IO, look, we can even implement Monad on our category version!
instance Applicative (IOCat i) where
  pure x = IOCat $ \(# rw, _ #) -> (# rw, x #)
  liftA2 f (IOCat g) (IOCat h) =
    IOCat $ \(# rw, i #) ->
      let !(# rw', o1 #) = g (# rw, i #)
          !(# rw'', o2 #) = h (# rw', i #)
       in (# rw'', f o1 o2 #)

instance Monad (IOCat i) where
  return = pure
  (IOCat f) >>= g =
    IOCat $ \(# rw, i #) ->
      let !(# rw', a #) = f (# rw, i #)
          (IOCat x) = g a
       in x (# rw', i #)
```

This just one interesting monad I've re-implemented, but you can re-implement most if not all of mtl in this style too.

In fact, the savvy among you may know that IO as it's written in Haskell is just a variant of the `State` monad, so I've already shown an implementation for State as well!

I'll leave the other implementations for another post lest this one get waaaaaay too long.

# The Case for Tracked Inputs

The Category hierarchy is

```haskell
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
```


# A better syntax

I promised you a syntax sugar for this new Category-based hierarchy, ever heard of Arrow notation?

There's a very good chance you haven't, so allow me to explain a bit.

There's a great jumping off point in the [GHC manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html)

Arrow notation is a do-notation-like syntax for working with Arrows, and Arrows are
part of the Category hierarchy!

Unfortunately, Arrow notation hasn't gotten much love in a loooong while, and as Haskell has evolved we've
also discovered that [the Arrow hierarchy isn't quite right](https://github.com/purescript-deprecated/purescript-arrows/issues/9), but I think the notation itself is more than salvageable.

It's come to light that the Arrow class can be broken down into much more granular pieces,
notably Category, Profunctor, Strong, Choice, Plus, etc. However this wasn't know when
Arrow notation was first introduced, so it was designed to work with the Arrow class itself,
so the biggest issue with it as I see it is that it currently _requires_ an `Arrow` instance.
This instance in turn requires an implementation for `arr :: Arrow a => (b -> c) -> a b c`;
which, in the new hierarchy is like requiring at least `Category` and `Strong`, since you can implement `arr` in terms of those two:

```
arr :: (Category a, Profunctor a) => (b -> c) -> a b c
arr f = dimap id f Category.id
```



This unfortunately means that in its current implementation we can't




------------------


The category version is better than Monad and Selective Applicative because you can be precise in your argument routing using `Strong` and `Choice`.
Demo a small build system with that...
(SQL query example?)

