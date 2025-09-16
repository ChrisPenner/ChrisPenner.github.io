---
title: "Monads are too powerful"
author: Chris Penner
date: Aug 14, 2025
tags: [programming, haskell]
description: "Monads are a useful tool, but what we gain in power we lose in static analysis."
image: pipes.jpg
---

I've had some version of this blog post sitting in my drafts folder for more 
than 5 years now, I've finally decided it's more important to get it out there
than to get it _perfect_.

First and foremost, this is NOT yet-another-nomad-tutorial. This post is for people who 
already know and love monads. If that's not you, feel free to read on, but know that
it might be a bit terse.

# Motivation

Okay, so we you and I both know Monads are great, they allow us to sequence operations
in a structured way, and are in many ways a super-power in the functional-programming toolkit. 

However my argument is that monads are actually _too_ powerful, or to be more **precise**: Monads are _im_**precise**.

If we boil it down, monads in essence allow us to express a sequence of effects we wish to perform, where notably,
the choice of which effects to perform next can **depend on the results of running the previous effects**.

This is a huge boon, it's hard to imagine any modern Haskell program without the use of monads, they allow us to express such a wide variety of programs!
But too much power can be a bad thing. Why would we ever want to restrict ourselves?
Let's talk about Applicatives!

## The origin of Applicatives

As far as I can determine, the first widespread introduction of Applicatives to programming was 
in [Applicative Programming with Effects](https://www.staff.city.ac.uk/~ross/papers/Applicative.pdf), a 2008 paper by Conor McBride and Ross Paterson.

Take note that this was decidedly _after_ Monads were already in widespread use.
Applicatives are _less powerful_ than Monads, or to be more precise, Applicatives can 
express fewer effectful programs than Monads can, easily proven by the fact that every Monad is also an Applicative, but not every Applicative is a Monad.

Yet Applicatives are still very useful! Not only do they allow us to express programs with effects that aren't valid monads, 
but they also provide us with the ability of runtime static analysis of effectful programs.

Looking at the Applicative interface:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

We can see that there's no way way to sequence effects at _runtime_ such that the chosen sequence of effects is dependent on runtime values.
Unlike `>>=`, `<*>` allows running a haskell function, but the result of the function is completely independent from the sequencing of effects.

This gives us a ton of utility in static analysis, we can do things like print a list
of all the effects that will be run before running any of them, we can ask the end-user for permission before running potentially harmful effects, 
we can parallelize effects, or rewrite our sequence by inserting yields between each effect, we can run effects in parallel or in a different order for greater efficiency and so on.

I hope that's enough ink to convince you that the _expressiveness_ (i.e. the breadth of programs we can express)
is not a matter of "more programs more better", but rather expressiveness exists on a continuum between 
static analysis and expressiveness.

```
Static Analysis <+------------+------------+> Expressiveness
                 |            |            |
                 +> Functor   |            +> Monad
                              +> Applicative
```

Power comes at a cost, specifically the cost of static analysis.


As soon as we hit a bind in some monadic computation, anything could happen next. This
is obvious from the signature of `bind` itself:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

The `(a -> m b)` is the critical bit, the `->` allows us to construct the next 
set of effects using the _full breadth of the Haskell language at runtime_.
Haskell doesn't provide us with any (sane) mechanism for analyzing arbitrary 
chunks of Haskell code we're about to execute, so if we want to know which 
effects might come next we have no choice: Execute the code and see what happens!

## The Sweet Spot

So,  Monads let us do too much, but Applicatives don't let us depend on results of 
previous effects, which is also a deal-breaker. Clearly the sweet spot must be somewhere in between!

Selective Applicatives fit nicely into the continuum between Applicatives and Monads.

If you haven't heard of them, go read up on them [here](https://hackage.haskell.org/package/selective).

The interface for Selective Applicatives allow us to specify efery possible codepath that our program _may_ execute, 
but leaves the actual branching down to runtime.

This is, in my opinion, an abstraction which  gets us _much_ closer to matching the 
level of expressiveness we need for everyday programming while still granting us 
most of the best benefits of static analysis.

With Selective Applicatives we can do things like crawl the expression to see 
the union of all possible effects we may run, we can edit the graph of effects to perform optimizations like memoizing duplicated
effects, we can display a flow chart of all possible program executions to the user, we could check all the static dependencies of a build-system defined in a Selective Applicative such that we can determine whether any of their inputs have changed, the list goes on. All of these are simply impossible with Monads, even with the use of Free Monads, there are some tricks, but they're generally very specific to each specific application.

However, there are unfortunately a lot of problems with Selective Applicatives too:

* We can't express things like loops which contain effects, or recursion
* There's no good syntax for expressing Selective Applicatives like there is with do-notation for Monads and ApplicativeDo.
* None of Functors, Applicative, Selective Applicative Functors, OR Monads have any structured representation of inputs or the flow of data from one effectful computation to the next.

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
In fact, IO is just a variant of the `State` monad, so if you look closely above I've already done that one for free.

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

