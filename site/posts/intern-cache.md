---
title: 'Save memory and CPU with an interning cache'
author: "Chris Penner"
date: "Aug 12, 2025"
tags: [haskell]
description: "Weak Caching for Strong Apps"
image: filing-cabinet.jpg
---

This post will introduce a simple caching strategy, with a small twist, which depending on your app may help you not only improve performance, but might also drastically reduce the memory residency of your program.

I had originally written this post in 2022, but looks like I got busy and failed to release it, so just pretend you're reading this in 2022, okay? It was a simpler time.

In case you're wondering, we continued to optimize storage since and modern UCM uses even less memory than back in 2022 😎.

Spoiler warning, with about 80 lines of code, I was able to reduce both the memory residency and start-up times by a whopping ~95%! 
From 90s -> 4s startup time, and from 2.73GB -> 148MB. All of these gains were realized by tweaking our app to enforce _sharing_ between identical objects in memory.


## Case Study

I help build the [Unison Language](https://www.unison-lang.org/).
One unique thing about the language is that programmers interact with the language through the Unison Codebase Manager (a.k.a. `ucm`), which is an interactive shell.
Some users have started to amass larger codebases, and lately we've been noticing that the memory usage of `ucm` was growing to unacceptable levels.

Loading one specific codebase, which I'll use for testing throughout this article, required **2.73GB** and took about **90 seconds** to load from SQLite. 
This is far larger and slower than we'd like.

There are 2 important facets of how Unison stores code that will be important to know as we go forward, and will help you understand whether this technique might work for you.

* **Unison codebases are append-only, and codebase definitions are referenced by a content-based hash.**

A Unison codebase is a tree with many branches, each branch contains many definitions and also has references its history. In Unison, once a definition is added to the codebase it is immutable, this is similar to how commits work in git; commits can be built upon, and branches can change which commit they point to, but once a commit is created it cannot be changed and is uniquely identified by its hash.

* **A given Unison codebase is likely to refer to subtrees of code like libraries many times across different Unison branches. E.g. most projects contain a reference to the `base` library.**

A Unison project can pull in the libraries it depends on by simply mounting that dependency into its `lib` namespace. Doing so is inexpensive because in effect we simply copy the hash which refers to a given snapshot of the library, we don't need to make copies of any of the underlying code. 
However, when loading the codebase into memory `ucm` was hydrating each and every library reference into a 
full in-memory representation of that code. No good!

## What is sharing and why do I want it?

Sharing is a very simple concept at its core: rather than having multiple copies of the same identical object in memory, we should just have one.
It's dead simple if you say it like that, but there are many ways we can end up with duplicates of values in memory. For example, if I load the same codebase from SQLite several times then SQLite won't know that the object I'm loading already exists in memory and will make a whole new copy.

In a language where data is mutable by default you'll want to think long and hard about whether sharing is sensible or even possible for your use-case, but luckily for me, everything in Haskell is immutable by default so there's absolutely no reason to make copies of identical values.

There's an additional benefit to sharing beyond just saving memory: equality checks may be optimized!
Some Haskell types like `ByteString`s include [an optimization](https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/src/Data.ByteString.Internal.Type.html#eq) in their `Eq` instance which short circuits the whole check if the two values are pointer-equal. 
Typically testing equality on string-like values is actually _most_ expensive when the two strings are actually equal since the check must examine every single byte to see if any of them differ.
By interning our values using a cache we can reduce these checks become a single pointer equality check rather than an expensive byte-by-byte check.

## Implementation

One issue with caches like this is that they can grow to eventually consume unbounded amounts of memory, we certainly don't want every value we've ever cached to stay there forever. 
Haskell is a garbage collected language, so naturally the ideal situation would be for a value to live in the cache up until it is garbage collected, but how can we know that?

GHC implements [weak pointers](https://hackage.haskell.org/package/base-4.21.0.0/docs/System-Mem-Weak.html#t:Weak)!
This nifty feature allows us to do two helpful things: 

1. We can attach a finalizer to the values we return from the cache, such that values will automatically **evict themselves** from the cache when they're no longer reachable.
2. Weak references don't prevent the value they're pointing to from being garbage collected. This means that if a value is _only_ referenced by a weak pointer in a cache then it will still be garbage collected.

As a result, there's really no downside to this form of caching except a very small amount of compute and memory used to maintain the cache itself. Your mileage may vary, but as the numbers show, in our case this cost was **very much worth it** when compared to the gains.

Here's an implementation of a simple _Interning Cache_:

```haskell
module InternCache
  ( InternCache,
    newInternCache,
    lookupCached,
    insertCached,
    intern,
    hoist,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import System.Mem.Weak
import UnliftIO.STM

-- | Parameterized by the monad in which it operates, the key type, 
-- and the value type.
data InternCache m k v = InternCache
  { lookupCached :: k -> m (Maybe v),
    insertCached :: k -> v -> m ()
  }

-- | Creates an 'InternCache' which uses weak references to only 
-- keep values in the cache for as long as they're reachable by 
-- something else in the app.
--
-- This means you don't need to worry about a value not being 
-- GC'd because it's in the cache.
newInternCache :: 
  forall m k v. (MonadIO m, Hashable k) 
  => m (InternCache m k v)
newInternCache = do
  var <- newTVarIO mempty
  pure $
    InternCache
      { lookupCached = lookupCachedImpl var,
        insertCached = insertCachedImpl var
      }
  where
    lookupCachedImpl :: TVar (HashMap k (Weak v)) -> k -> m (Maybe v)
    lookupCachedImpl var ch = liftIO $ do
      cache <- readTVarIO var
      case HashMap.lookup ch cache of
        Nothing -> pure Nothing
        Just weakRef -> do
          deRefWeak weakRef

    insertCachedImpl :: TVar (HashMap k (Weak v)) -> k -> v -> m ()
    insertCachedImpl var k v = liftIO $ do
      wk <- mkWeakPtr v (Just $ removeDeadVal var k)
      atomically $ modifyTVar' var (HashMap.insert k wk)

    -- Use this as a finalizer to remove the key from the map 
    -- when its value gets GC'd
    removeDeadVal :: TVar (HashMap k (Weak v)) -> k -> IO ()
    removeDeadVal var k = liftIO do
      atomically $ modifyTVar' var (HashMap.delete k)

-- | Changing the monad in which the cache operates with a natural transformation.
hoist :: (forall x. m x -> n x) -> InternCache m k v -> InternCache n k v
hoist f (InternCache lookup' insert') =
  InternCache
    { lookupCached = f . lookup',
      insertCached = \k v -> f $ insert' k v
    }
```

Now you can create a cache for any values you like! You can maintain a cache 
within the scope of a given chunk of code, or you can make a global cache for your entire app 
using `unsafePerformIO` like this:


```haskell
-- An in memory cache for interning hashes.
-- This allows us to avoid creating multiple in-memory instances of the same hash bytes;
-- but also has the benefit that equality checks for equal hashes are O(1) instead of O(n), since
-- they'll be pointer-equal.
hashCache :: (MonadIO m) => InternCache m Hash Hash
hashCache = unsafePerformIO $ hoist liftIO <$> IC.newInternCache @IO @Hash @Hash 
{-# NOINLINE hashCache #-}
```

And here's an example of what it looks like to use the cache in practice:

```haskell
expectHash :: HashId -> Transaction Hash
expectHash h =
  -- See if we've got the value in the cache
  lookupCached hashCache h >>= \case
    Just hash -> pure hash
    Nothing -> do
      hash <-
        queryOneCol
          [sql|
              SELECT base32
              FROM hash
              WHERE id = :h
            |]
      -- Since we didn't have it in the cache, add it now
      insertCached hashCache h hash
      pure hash
```

For things like Hashes, the memory savings are more modest, but in the cases of entire subtrees of code the difference for us was substantial.
Not only did we save memory, but we saved a ton of time re-hydrating subtrees of code from SQLite that we already had.

We can even get the benefits of a cache like this when we don't have a separate key for the value, 
as long as the value itself has a `Hashable` or `Ord` instance (if you swap the InternCache to use a regular Map).
We can use it as its own key, this doesn't help us avoid the computational cost of _creating_ the value, but 
it still gives us the memory savings:

```haskell
-- | When a value is its own key, this ensures that the given value 
-- is in the cache and always returns the single canonical in-memory 
-- instance of that value, garbage collecting any others.
intern :: (Hashable k, Monad m) => InternCache m k k -> k -> m k
intern cache k = do
  mVal <- lookupCached cache k
  case mVal of
    Just v -> pure v
    Nothing -> do
      insertCached cache k k
      pure k
```

## Conclusion

An approach like this doesn't work for every app, it's much easier to use when working with immutable values like this, but if there's a situation in your app where it makes sense I recommend giving it a try!
I'll reiterate that for us, we dropped our codebase load times from 90s down to 4s, and our resting memory usage from 2.73GB down to 148MB.
