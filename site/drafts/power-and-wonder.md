---
title: "The Power and Wonder of Haskell: example included"
author: "Chris Penner"
date: "Sep 29, 2020"
tags: [haskell, optics]
description: ""
---

This post is open to everyone 😄

Bob Gray one said 

> Writing in C or C++ is like running a chain saw with all the safety guards removed

I interpret this to mean that these languages are powerful and effective, but also carry with them an ominous threat. If you slip up, it'll end up costing you dearly.

As a corollary I'd describe Haskell more like a table saw with one of those [thingies that stops it from cutting through a hot-dog](https://www.youtube.com/watch?v=fq3o0VGUh50). It takes a little know-how to learn to use, and looks pretty scary, and it protects you from making mistakes at all costs, even to the point of preventing you from doing your job sometimes.

I initially approached Haskell with a bit of awe and wonder, eventually I learned enough that some of that mystery began to fade away, but I can thankfully say that I've been writing Haskell long enough that I'm experiencing a whole new dose of power and wonder to marvel at!

To that end, I'd like to show off a toy program I came up with while exploring one of the trickier corners of the programming world: programming with continuations!
This particular program is fascinating to me because its something I can barely even conceive of writing properly in any other language, and even if I could the end result would likely be so hideous as to be no use to anyone.

So what are we building?

Today I'll be using continuations along with concurrency primitives to write a tiny toolset for building computations with incremental execution caching!
Have you ever used Docker and noticed how it'll only rebuild the bits of the docker image starting from the first of any pieces you've changed? We're going to build something like that, but with arbitrary computations and arbitrary dependencies!

I'll be explaining everything I'm doing as I go, and it'll all be a bit "hand-wavy" anyways, so even if you don't know Haskell I'm sure you'll be able to follow along with the principles of it all, come along for the ride!

## Genesis

Okay, let's get started! As with any Haskell post I'll start with a seemingly random assortment of language extension incantations and imports to satisfy GHC:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Lib where

-- The Continuation Monad Transformer
import Control.Monad.Trans.Cont
-- The Software Transactional Memory library
import Control.Concurrent.STM
-- Some other concurency utilities
import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)
-- The WriterT Monad
import Control.Monad.Writer
-- Some additional tools for working with Monoids and Alternative instances.
import Data.Monoid (Monoid, Alt(..))
```

I'll be introducing most of the imports as we need them, so don't worry about it for now, but it's there in case you're following along at home 😀

Okay! So, first thing we should do is outline our expected behaviour. Here's a really really simple program:


```haskell
main :: IO ()
main = do
    putStrLn "About to do a lot of work!"
    -- pretend we're working really hard
    let tool = "Shovel"
    putStrLn ("*Grunt* Working hard with a " <> tool)
    let beverage = "Capri Sun"
    putStrLn ("*Sip* Relaxing with a cool " <> beverage)
```

When we run this brilliant work of art we get this result:

```haskell
>>> main
About to do a lot of work!
*Grunt* Working hard with a Shovel
*Sip* Relaxing with a cool Capri Sun
```

Hopefully that's not too surprising. It may seem a bit strange that I've split off  our `tool` and `beverage` as separate bindings, but those are going to be the _dependencies_ of our program, and when we're done, we should be able to detect when either of them changes and only re-run the bits of the program that come AFTER the changed variable, potentially saving us a ton of work! If we simply want to swap out our refreshing beverage, we want to avoid needing to do all of the work that preceded it!

If you want to be a bit more boring about it, you can imagine that `tool` is a list of files to compile, and that `beverage` is instead a path to copy all the compilation artifacts. We can imagine that we may wish to change the destination path for our compiled artifacts, but we don't need to actually re-compile them unless the list of source files changes!

Now, there are probably dozens of different ways to accomplish this sort of task, but I've chosen a set of two tools, both of which most folks in the functional programming world will likely have heard of, but other folks may be new to: 

* The Continuation Monad Transformer: `ContT`
* Software Transactional Memory a.k.a. `STM`

It's fine if you haven't read up on these yet, we'll see what they can do as we go, and if I manage to pique your interest you can read up on them elsewhere after we're done.

## Tracking Variable Access

The first thing we'll need to do is add a little bit of "smarts" to our dependency variables. Right now we're using Plain Old Haskell Values™ which means the compiler will probably inline them into their usage locations and we'll have no insight into which parts of code depend on which variables. I'm going to define a data structure to hold our variables:


```haskell
-- A "smart" wrapper around dependency variables
data DVar a = DVar a
```

Great! This gives us absolutely nothing as it is, but it gives us a spot for us to collect any additional data we care about regarding each of these dependency variables.

The next thing we'll need is a way to track whenever we **read** one of these variables, so let's write some code that does that:

```haskell
readDVar :: DVar a -> IO a
readDVar (DVar ref) = return ref
```

Right now this just pulls the value out of the wrapper and embeds it into the IO monad for no reason, but don't worry, we'll add a bit more "smarts" to it in a sec.

Now we can replace each of our uses with a dependency variable:

```haskell
main :: IO ()
main = do
    putStrLn "About to do a lot of work!"
    -- pretend we're working really hard
    tool <- readDVar toolVar
    putStrLn ("*Grunt* Working hard with a " <> tool)
    beverage <- readDVar beverageVar
    putStrLn ("*Sip* Relaxing with a cool " <> beverage)
  where
    toolVar = DVar "Shovel"
    beverageVar = DVar "Capri Sun"
```

That was a pretty minor refactor, but it gave us a big benefit; now you need to explicitly access these variables in a specific sequence in your main program! Now we can use this to our advantage! Let's introduce the `ContT` continuation monad transformer.

## Introducing Continuations

So, what is a continuation and what does it allow us to do? The continuation monad gives us a function called `callCC` (**C**all with **C**urrent **C**ontinuation). This function is quite impressive and it allows us to **capture** a **reference** to the **entire** remaining program that we were _just_ about to execute! If we call that continuation it'll skip the rest of the delimited block we're executing and will run the rest of the program instead! Here's what it looks like:


```haskell
hideAndSeek :: ContT () IO ()
hideAndSeek = do
    logMsg "Let's play hide and seek"
    -- Open a new delimited continuation using callCC
    callCC $ \cc -> do
        logMsg "I'm going to find you!"
        cc ()
        -- The rest of this block will never run!
        logMsg "Nah; I'm too stealthy"
    -- Execution picks up here, since this is the 'continuation' after the block
    logMsg "Dang where'd they go?"
```

Notice how we're completely missing the `"Nah; I'm too stealthy"` log line, the continuation popped us out of the nested block when we call it using `cc ()`;

```haskell
>>> evalContT hideAndSeek
Let's play hide and seek
I'm going to find you!
Dang where'd they go?
```

Typically, since calling the continuation **continues execution until the end**, one would use `callCC` to break out of a deeply nested inner computation, for instance we could "pass" the continuation deep down into a response handler for a web request, calling the continuation with a response object would discard the rest of the handler and immediately return the response!


But there are even cooler tricks to try! `Control.Monad.Trans.Cont` from the `transformers` library includes `shiftT` and `resetT` for running **delimited** continuations. This means that instead of always running the **entire** remaining program, we can cordon off a section of our computation to act as the continuation.


```haskell
hopScotch :: ContT () IO ()
hopScotch = do
    logMsg "Before"
    -- Open a new delimited continuation using resetT
    resetT $ do
        -- Capture the continuation up to the reset
        shiftT $ \cc -> do
            logMsg "Calling delimited continuation"
            lift $ cc ()
            -- This bit will be run after the rest of the delimited block!
            logMsg "Run me AFTERWARDS"
        logMsg "Skips up to here next"
    -- This bit is outside of "resetT"s scope, so this is where computation resumes normally
    logMsg "THE END"

>>> evalContT hopScotch
Before
Calling delimited continuation
Skips up to here next
Run me AFTERWARDS
THE END
```

One last mind-bending example before we move on in our project:



---

Other ideas

Continuations for optimistic cache-busting;

```haskell
shiftT $ \cc -> do
  -- start running the program with the old value, but fetch the new value while things are running.
  -- Whenever you get the new value, kill the stale run (iff it hasn't completed yet) and run the program again with the new value
  newValue <- withAsync (cc oldValue) $ \_ -> fetchNewValue
  cc newValue

```

Continuations for UI's (pretty much the same as cache-busting)

Iterations like newton's method? Nah probably not

