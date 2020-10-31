---
title: 'Composable filters using Witherable optics'
author: "Chris Penner"
date: "Oct 27, 2020"
tags: [haskell, optics]
description: ""
---

One of my favourite things about Haskell is that its structures and abstractions
are very principled, and they have laws dictating correct behaviour.

In my experience, this means that when you find a new way to piece together those
abstractions it _almost always_ ends up doing something reasonable... or at the very least interesting!

It comes as no surprise to some of you that I think about **optics** (lenses) an awful lot! 
Optics are the most beautiful, composable, expressive, and adaptable abstractions I've
ever had the pleasure of discovering. Optics provide an elegant abstraction
for many data-manipulation strategies that have been around for decades.

In this post we're going to explore a new type of optic which provides composable
and expressive means for filtering complex data structures.
To do so is going to require a base-level understanding of optics, and preferably readers would
understand what a Traversal is. Bonus points if you've implemented one on your own or have some idea of how they work!

## The Background

Let's take a look at the Traversal typeclass, it's where we find Haskell's
all-powerful secret weapon **traverse**! ([BTW The answer is always traverse.](https://impurepics.com/fp-bot/index.html))

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```

Many beginners rightfully stumble on this function because it's just so dang abstract! It's tough to tell what it does for you, and how you could use it (the answer is almost anywhere).


Eventually, folks realized that we could write more "specialized" versions of this type which worked on anything, not just Traversable datatypes, and the Traversal was born!

A traversal is anything that fits this rough shape:

```haskell
forall f. Applicative f => (a -> f b) -> (s -> f t)
```

We can find different optics by specializing the constraints on `f` in interesting ways, for instance lenses require `f` to be a `Functor`, and folds require `f` to have both `Contravariant` and `Applicative` instances (exercise: go see if you can figure out what those two constraints imply!).

Anyways, long story short, if you can make your function fit that shape, it's probably useful as some sort of optic!

This leads us to Witherable!

## Witherable

Ever heard of [Witherable](https://hackage.haskell.org/package/witherable-0.3.5/docs/Data-Witherable.html)?
It's a class which extends from Traversable, that is, Traversable is a superclass, all Witherables are Traversable, but not the other way around.

Here's what it looks like:

```haskell
class (Traversable t, Filterable t) => Witherable t where
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
```

Witherable is a utility class that allows you to update or **remove** items 
from a structure within an effectful context.

As you can see, this type is based on the type of `traverse`! 
Unfortunately, it doesn't **quite** match the shape we need, it's got a pesky extra `Maybe` in the way.

We could maybe get around this using some sort of `Compose f Maybe` type, but at the end of the day, having a concrete type rather than a constraint will make life difficult for us when composing with other optics. Instead, is there some typeclass constraint that can represent the success or failure idea that's inherent to Maybe?

There certainly is! It's called `Alternative`!

Here's what [Alternative](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative) looks like:

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

Alternative is technically a "Monoid over Functors", meaning it has an "empty" (identity) value and an associative binary operation to combine multiple options together.

In practice, we can use this class in Haskell to handle success, failure, and even provide "backups" for failure.

Cool, so here's the function signature we're shooting for:

```haskell
withered :: forall f t a b. (Alternative f, Witherable t) => (a -> f b) -> t a -> f (t b)
```

That's the right shape! Is there any way we can re-use our `Witherable` class rather than writing this from scratch for every type? That would require us to recognize when our Alternative has failed and introduce a `Maybe` into the mix; luckily `Alternative` provides exactly this function, it's called `optional`!

```haskell
optional :: Alternative f => f a -> f (Maybe a)
```

The idea here is that it "lifts" the failure out of the Alternative into the Maybe and returns an Alternative structure that will always "succeed", but will return a `Nothing` if the original alternative would have failed.

We can use this for a trivial implementation of `withered` in terms of `wither` like so:

```haskell
withered :: forall f t a b. (Alternative f, Witherable t) => (a -> f b) -> t a -> f (t b)
withered f = wither (optional . f)
```

Great! In pretty short order we've constructed a new combinator that fits a signature compatible with other optics, based on a typeclass that has a pretty clear semantic meaning. Now for the fun part, let's see how we can use it!

## Withers as Optics

First things first let's get some notation quibbles out of the way:

```haskell
import Control.Lens

type Wither s t a b = forall f. Alternative f => LensLike f s t a b
type Wither' s a = Wither s s a a
```

To start off, we know that (in a Van Laarhoven encoding) we can use `traverse` as an optic directly, the [`%%~`](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Lens.html#v:-37--37--126-) combinator from `lens` allows us to run an optic if we provide an effectful function which matches the optic's focus and constraints.

E.g. for a `Traversal s t a b` we can provide a function `Applicative f => a -> f b` and it will return a `s -> f t` for us.

```haskell
>>> import qualified Data.Map as M
>>> let x = M.fromList [('a', [1, 2, 3]), ('b', [4, 5, 6])]
>>> x & traverse . traverse %%~ (\n -> Identity (n * 10))
Identity (fromList [('a',[10,20,30]),('b',[40,50,60])])
```

As you likely know, traversals can dive deep within a structure, make some edits there, then rebuild the structure around the results!

Many of the combinators in `lens` conveniently hide this fact from you by choosing the correct effects and functions to perform higher level actions. For instance, `(*~)` from `lens` behaves very similarly to what we've written above.

What happens if we do the same with `withered`?

```haskell
>>> x
fromList [('a',[1,2,3]),('b',[4,5,6])]
>>> x & withered . withered %%~ (\n -> Identity (n * 10))
[[10,20,30], [40,50,60]]

• Could not deduce (Alternative Identity)
    arising from a use of ‘withered’
```

Ahh, of course! `withered` **requires** an Alternative instance on whichever effect we choose to use, let's try one like Maybe!


```haskell
>>> x & withered . withered %%~ (\n -> Just (n * 10))
Just (fromList [('a',[10,20,30]),('b',[40,50,60])])
```

That's better! We've effectively recovered the ability to use our `Wither`s as Setters, except that our result has the possibility of failure!

At first, the `Maybe` wrapping our result seems like a disadvantage, it's a pain to have to unwrap it after all, but the possibility of failure was the point all along. We can manipulate `withered` to be smart about failure and how to handle it.

Let's try something more creative and a bit closer to `wither`'s strengths.

First we need some sort of operation that might fail! 
Parsing is a great example there, it's always possible that a string may not match
the format of the result we want. Wither works wonderfully with parser combinators, 
but we'll start with something a little simpler:

```haskell
>>> import Text.Read
>>> :t readMaybe
readMaybe :: Read a => String -> Maybe a
```

Okay, so `readMaybe` will try to parse a string into a value of the provided type, and will fail as a `Nothing` if it doesn't work out.

Remember how `traverse` automatically handles automatically pulling effects all the way out from the inside of a deeply nested structure?
I wonder how `withered` will handle the "extraction" of the concept of failure!

```haskell
>>> let y = M.fromList [('a', ["1", "2", "Tangerine"]), ('b', ["4", "Alpaca", "6"])] 
>>> (y & withered . withered %%~ readMaybe) :: Maybe (M.Map Char [Int])
Just (fromList [('a',[1,2]),('b',[4,6])])
```

Okay, so like, you gotta admit, that's pretty cool! With the wave of a hand we've
gone two levels deep into a complex structure, applied a parsing operation that could fail,
and automatically filtered down the containing list to remove failed parses, then rebuilt the outer structure!

Compare this to what happens if we do the same with traverse:

```haskell
>>> let y = M.fromList [('a', ["1", "2", "Tangerine"]), ('b', ["4", "Alpaca", "6"])] 
>>> (y & traverse . traverse %%~ readMaybe) :: Maybe (M.Map Char [Int])
Nothing
```

...well that's disappointing! `withered` is clearly the better match for this sort of thing.


Let's chat about how this actually works.

## Filtering branches

We can think of traversals as "branching" data explorations, they help you dive down deeply into **many** sections of a data structure at once, apply their transformations, then "re-build" the structure as those branches unwind one by one! Each of those branches carry an **independent** set of effects with them, but as the structure is rebuilt, those branches are merged back together and those effects are combined. In the case of `traverse`, those effects are combined and sequenced using `Applicative`, and the structure is rebuilt within that Applicative context. This is why, when we tried using `traverse` for our parsing the whole result was `Nothing` even though we had a few passing parses. The Applicative instance of `Maybe` dictates that all function applications inside a `Nothing` just keep returning `Nothing` and it clobbered the whole structure!

Our `withered` combinator is **failure-aware**; and the `Witherable` instance knows how interpret it to _filter_ the data structure rather than completely _clobber_ it.

One important thing to notice here is that when `wither` **collects** all the branches of its computation (one for each element of the structure), it **catches** the failure of the `Alternative` structure by using `optional`. This means that failures to the right of a `withered` **will not propagate past it to the left**. The `withered` will catch it and filter out that branch from the structure as it rebuilds it.

A second thing to notice is that a call to `wither` itself will **never "fail"** (i.e. it won't return the `empty` value of the Alternative). This is because the `Witherable` class will simply return an empty structure (rather than the empty effect) if all the elements are filtered out. Take a look at what I mean:

```haskell
>>> withered (const Nothing) [1, 2, 3, 4]
Just []
```

We can see the same behaviour in `wither` if we provide the equivalent:

```haskell
>>> wither (const (Identity Nothing)) [1, 2, 3, 4]
Identity []
```

It doesn't matter if every element "fails", the result will still "succeed" with an empty structure.


This is actually a huge benefit for us. We've seen that `traverse` **propagates** any failures to the left, and that `withered` **catches** any failures and doesn't propagate them at all. By manipulating these facts we can **choose how and when to handle failure**!

## Catching failures

To demonstrate the point, let's stick with parsing using `readMaybe`.

```haskell
>>> let z = M.fromList [('a', ["1", "2", "3"]), ('b', ["4", "Alpaca", "6"])] 
```

I've altered the structure, now it has an outer map with two keys, the 'a' key contains all valid parses for integers. The 'b' key contains 2 good parses and one bad one.

Let's see what happens if we use `withered` to drill down through both structures:

```haskell
>>> (z & withered . withered %%~ readMaybe) :: Maybe (M.Map Char [Int])
Just (fromList [('a',[1,2,3]),('b',[4,6])])
```

Just as expected, it has parsed the valid integers from 'a', in 'b' it has filtered out the bad parse while still keeping the valid parses.

Given our new understanding of how `traverse` **propagates** errors rather than catching them, what do we expect to happen if we replace the second `withered` with `traverse`?

```haskell
>>> (z & withered . traverse %%~ readMaybe) :: Maybe (M.Map Char [Int])
Just (fromList [('a',[1,2,3])])
```

Aha! `traverse` caused the single failure to propagate and kill the branch at the next level up. Now the first `withered` catches the error when rebuilding the Map and it will filter out the entire `b` key from the map!

This can take a bit of getting used to of course, but it ultimately allows **composable** filtering, and allows you to filter complex data structures while using lenses or traversals to base your judgements on their internals.


Until now I've been using `%%~` to pass explicit `Alternative f => a -> f b` functions, but we can build some handy combinators around it to make it a bit easier to use!

For instance, what if we want to filter one of the traversed structures based on a predicate?

```haskell
guarding :: Alternative f => (a -> Bool) -> a -> f a
guarding p a
    | p a = pure a
    | otherwise = empty

filterOf :: (a -> Bool) -> LensLike Maybe s t a a -> s -> Maybe t
filterOf p w s = s & w %%~ guarding p
```

Now we can express computations like this:

```haskell
>>> [[1, 2, 3], [4, 5, 6]] & filterOf even (withered . withered)
Just [[2],[4,6]]
```

Now that we have a combinator for it, here's another example. We can filter a list that's **in the middle of our structure** based on deeply nested values inside by composing our withers with lenses and prisms!

Imagine something like the following:

```haskell
>>> filterOf (== "CA") (users . withered . address . country) account
```

This allows us to look inside a piece of state ("account"), then filter a list or map of users from within, keeping only Canadian users!

## Bonus: Fun examples

At this point I'm just going to post a bunch of cool examples of things we can do! Enjoy!

---

Prisms already capture the idea of success and failure, but they simply skip the traversal if the prism doesn't match, we can lift prisms into withers such that they'll fail in a way that wither can catch!

```haskell
witherPrism :: (Alternative f, Choice p) => Prism s t a b -> Optic p f s t a b
witherPrism prsm =
    withPrism prsm $ \embed match ->
        dimap match (either (const empty) (fmap embed))  . right'
```

Note that unfortunately the result of `witherPrism` will no longer work with most of the prism combinators due to the added Alternative constraint, but that's fine, if you need that behaviour, then simply don't `wither` the prism in those circumstances.

Now we can witherize a prism to turn it into a filter such that if the value fails to match the prism the branch "fails", and if the prism matches, it will run the predicate on the result!

```haskell
>>> [('a', Right 1), ('b', Left 2), ('c', Left 3)] & withered . _2 . witherPrism _Left %%~ guarding odd
[('c',Left 3)]
```

If we didn't lift our prism, it would simply "skip" unmatched values, and thus they wouldn't fail or be filtered:

```haskell
>>> [('a', Right 1), ('b', Left 2), ('c', Left 3)] & withered . _2 . _Left %%~ guarding odd
[('a',Right 1),('c',Left 3)]
```

---

Have you ever used `filtered`? It's a traversal that skips any elements that don't match a predicate. Here's the `Wither` version which "fails" any elements that don't match:

```haskell
guarded :: (a -> Bool) -> Wither a b a b
guarded p f a
  | p a = f a
  | otherwise = empty
```


This allows us to do things like this:

```haskell
>>> [[1, 2, 3, 4], [5, 6]] & filterOf (>2) (withered . guarded ((> 2) . length) . withered)
Just [[3,4]]
```

---

Since IO has an alternative instance which "fails" on IO Errors, we can do this:

```haskell
-- Read in the content of files that exist, filter ones that fail to read!
>>> ["README.md", "nonexistent.txt"] & withered %%~ readFile
["# wither\n"]
```

Note that since the alternative interface will **only** catch IO errors I'm not suggestion you use this in production!

---

STN implements Alternative! A "transaction" is considered to have "failed" if it ever needs to "block", or someone calls "retry".

Check this out:

```haskell
>>> import qualified Data.Map as M
>>> import Control.Concurrent.STM

-- Initialize some new channels
>>> [a, b, c] <- sequenceA $ [newTChanIO, newTChanIO, newTChanIO]
-- Build a map of the channels keyed by their name
>>> chans = M.fromList [('a', a), ('b', b), ('c', c)] :: M.Map Char (TChan Int)
-- Write some data into channels 'a' and 'b'
>>> atomically $ writeTChan a 1 >> writeTChan b 2
-- Get a filtered map of only the channels that have data available!
>>> atomically . withered readTChan $ M.fromList [('a', a), ('b', b), ('c', c)]
fromList [('a',1),('b',2)]
-- Now that we've consumed the values, the channels are all empty, so we get an empty map!
>>> atomically . withered readTChan $ M.fromList [('a', a), ('b', b), ('c', c)]
fromList []
```

---


If we require `Monad` in addition to `Alternative` we get power equivalent to `MonadPlus` and can actually results of computations as they complete the round-trip!

```haskell
type Selector' s a = Selector s s a a
type Selector s t a b = forall f. (Alternative f, Monad f) => LensLike f s t a b

selectResult :: (b -> Bool) -> Selector a b a b
selectResult p f a = do
    f a >>= \case
      b | p b -> pure b
        | otherwise -> empty
```

We can use this to build a combinator that filters out any "empty" lists from a map AFTER we've done the initial filtering.

```haskell
>>> xs = M.fromList [('a', [1, 3, 5]), ('b', [1, 2, 3])]
-- Original version, even though we "wither" we still end up with empty lists!
>>> xs & filterOf even (withered . withered)
Just (fromList [('a',[]),('b',[2])])

-- We can filter the result to clear out the empty lists as well
>>> xs & filterOf even (withered . selectResult (not . null) . withered)
Just (fromList [('b',[2])])
```


TODO: example of custom withers

























Example using a Map of STM channels, wither to get an STM of a map of all the channels with new values!
