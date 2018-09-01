---
title: "Update Monads: Generalizing over Reader/Writer/State"
author: Chris Penner
date: Sept 1, 2018
tags: [programming, haskell]
description: We explore the applications and implementation of a generalized version of Reader/Writer/State monads called UpdateT.
image: typesafe-api-versioning/numbers.jpg
---

Today we're going to take a peek at the Update monad! It's a monad which was
formalized and described in [Update Monads: Cointerpreting Directed
Containers](https://danelahman.github.io/papers/types13postproc.pdf) by Danel
Ahman and Tarmo Uustalu. Most folks probably haven't heard of it before, likely
because most of what you'd use it for is well encompassed by the Reader,
Writer, and State monads. The Update Monad can do everything that Reader,
Writer, and State can do, but as a trade-off tends to be less efficient at each
of those tasks. It's definitely still worth checking out though; not only is it
interesting, there are a few things it handles quite elegantly that might be a
bit awkward to do in other ways.

Heads up; this probably isn't a great post for absolute beginners, you'll want
to have a decent understanding of [monoids](https://wiki.haskell.org/Monoid)
and how [StateT](https://wiki.haskell.org/State_Monad) works before you dive in
here. 

First, what does it mean to say that Update **generalizes** over the `State`
monad? Well, we can make an analogy by saying that `State` generalizes over
both the `Reader` and `Writer` monads! This means ways that `State` can do
EVERYTHING that `Reader` and `Writer` can do, i.e. you can implement either of
them using the State monad if you want to; but as we know, `State` can also do
**more**! State can not only `get` and `put` state, but the combination of
these primitives means we can `modify` state too! Let's take a quick peek at
how we can implement `Reader` and `Writer` using `State`. Again, I assume you
understand State itself is implemented, so I'll just use the version from mtl
in the following examples. I'm going to implement them as newtype wrappers
around `StateT` since `StateT` already has different instances for these
type-classes.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype StateReader r a =
  StateReader (State r a)
  deriving (Functor, Applicative, Monad)

instance MonadReader r (StateReader r) where
  -- ask just returns the state
  ask = StateReader get
  local mod (StateReader m) =
    StateReader $
      -- We want to keep track the initial state so 
      -- we can set it back after we temporarily edit our state.
     do
      before <- get
      modify mod
      a <- m
      put before
      return a

newtype StateWriter w a =
  StateWriter (State w a)
  deriving (Functor, Applicative, Monad)

instance Monoid w => MonadWriter w (StateWriter w) where
  -- tell just mappends onto our state
  tell m = StateWriter (modify (`mappend` m))
  -- listen collects some new state which we return, 
  -- then we apply it to our running tally.
  listen (StateWriter m) =
    let (a, s) = runState m mempty
     in tell s >> return (a, s)
  -- pass runs some action in our monad which 
  -- results in a function which
  -- we allow to modify the new state before 
  -- we append it to the state we've
  -- already collected.
  pass (StateWriter m) =
    let ((a, f), s) = runState m mempty
     in tell (f s) >> return a
```

Hopefully that helps clarify how State is "more powerful" and thus "more
general" than Reader and Writer!

But we're not here for the Measly State monad! Let's get to the meat.

## Structure of the Update Monad

The Update Monad kinda looks like Reader, Writer and State got into a horrific
car accident and are now hopelessly entangled! Each computation receives the
current computation `state` (like reader) and can result in a monoidal action
(like writer). The action is them applied to the state according to a helper
typeclass which I'll call `ApplyAction`; this edited state is passed on to the
next computation and away we go! Something that immediately interested me in
this monad is that there are several distinct places to tweak its behaviour
without even needing to change which implementation of `MonadUpdate` we use! We
can change which action monoid we choose, which state we carry, and even the
function we use to apply the actions to the state! This sort of generality leads
to all sorts of cool behaviour without too much work, and people can build all
sorts of things we didn't initially expect when we wrote the typeclasses!

Let's take a look at my personal version of the class definition of MonadUpdate;
interestingly it actually requires two classes to describe! 


```haskell
{-# LANGUAGE FunctionalDependencies #-}
class ApplyAction p s where
  applyAction :: p -> s -> s


class (ApplyAction s p, Monad m) =>
      -- Because each of our methods only uses p OR m but not both 
      -- we use functional dependencies to assert to the type system that 
      -- both s and p are determined by 'm'; this helps GHC be confident
      -- that we can't end up in spots where types could be ambiguous.
      MonadUpdate m s p | m -> s , m -> p
  where
    putAction :: p -> m ()
    getState :: m s
```

The actual `MonadUpdate` typeclass isn't provided in the paper, so this is just my
take on which methods we'll need to make it useful.

Here are some laws I also just made up:

```haskell
-- Putting an action and then another action should be the same as 
-- putting the combination of the two actions.
putAction a >> putAction b == putAction (a `mappend` b)
-- We expect that when we 'put' an action that it gets applied to the state
-- and that the change is visible immediately
applyAction p <$> getState == putAction p >> getState
```


