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

As it turns out, optics have a lot of different "slots" where we can experiment with different data types and constraints to get new results. 

In this post I'll be exploring one such new combination and the results that follow. To get the most out of this post you'll want an understanding of:

* optics
* Traversable/Traversals
* Alternative

Here's the agenda

1. Introduce an adaptation of an existing typeclass to make it more amenable for optics
2. Discover the semantics behind the new optic and how it works
3. Write some combinators
4. [Throw science at the wall to see what sticks](https://www.youtube.com/watch?v=UM-wKQqBBnY) (a.k.a. lots of examples)

## The Background

First things first, let's go over the fundamentals we'll be working with.

Let's take a look at the Traversable typeclass, it's where we find Haskell's
all-powerful secret weapon **traverse**! ([BTW The answer is always traverse.](https://impurepics.com/fp-bot/index.html))

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```

This class eventually let to the concept of a `Traversal` in Van Laarhoven encoded optics; which looks like this:

```haskell
forall f. Applicative f => (a -> f b) -> (s -> f t)
```

If we clear the constraints we get a `LensLike`, which is just the shape of any combinator that will compose well with optics from the `lens` library:

```haskell
type LensLike f s t a b = (a -> f b) -> (s -> f t)
```

Anyways, long story short, if you can make your function fit that shape, it's probably useful as some sort of optic!

This leads us to **Witherable**!

## Witherable

Ever heard of [Witherable](https://hackage.haskell.org/package/witherable-0.3.5/docs/Data-Witherable.html)?
It's a class which extends from Traversable, that is, Traversable is a superclass, all Witherables are Traversable, but not the other way around.

Here's what it looks like:

```haskell
class (Traversable t, Filterable t) => Witherable t where
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
```

Types which implement Witherable expand on the functionality of Traversable, they add the ability to **filter** items out from
a structure within an effectful context. Although this type isn't yet in the `base` library, it turns out it can be pretty handy!

Examples of witherable types include things like lists and maps, each of their keys or values could potentially be "deleted" from the structure in a sensible way.

My goal is that I'd like to be able to add "filtering" to the list of things optics can do in a nicely composable way! To do that, we need it to look like a `LensLike`.

As you can see, the type of `wither` is pretty similar to the type of a `LensLike`, but 
unfortunately, it doesn't **quite** match the shape we need, it's got a pesky extra `Maybe` in the way.

```haskell
-- We need a shape like this:
(a -> f b) -> t a -> f (t b)

-- But wither looks like this:
(a -> f (Maybe b)) -> t a -> f (t b)
```

We could get rid of that extra Maybe is by specializing the `f` into something like `Compose f Maybe` using `Data.Functor.Compose`. This would get us close, but specializing the `f` type to include a concrete type loses a LOT of the generality of optics and will make it much more difficult to use this type with other optics. It's a non-starter.

We need to find some typeclass constraint which allows for the behaviour of `wither`, but without the concrete requirement of using `Maybe`. As it turns out, if we're looking to express "failure" as an Applicative structure, that's exactly what `Alternative` is for.

`Alternative` provides a concrete representation of "failure" which we can use as a substitute for the `Maybe` value that was ruining our day. As it turns out, `f (Maybe b)` is actually isomorphic to `MaybeT f b`, and `MaybeT` provides an Alternative instance, so we can always regain our previous behaviour if we're able to generalize it this way.

Here's what [Alternative](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative) looks like:

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

In addition to the `MaybeT f` we already mentioned, 
some examples of other `Alternative`s include `Maybe`, `[]`, `IO`, `STM`, `Logic` and most of the available `Parser` variants.
You can of course write your own effects which implement Alternative as well!

Okay, so here's the combinator I want to build, it's a valid "LensLike" so it'll be composable with other optics:

```haskell
withered :: forall f t a b. (Alternative f, Witherable t) => (a -> f b) -> t a -> f (t b)
```

To save us time, I'll define an alias for our `Alternative` `LensLike`:

```haskell
-- NOTE: Wither and Wither' are exported from Data.Witherable, 
-- BUT have they have the unfortunate, less-composable type we're trying to avoid.
-- This post uses the following variants instead (sorry about the naming confusion)
type Wither s t a b = forall f. Alternative f => (a -> f b) -> s -> f t
type Wither' s a = Wither s s a a
```

Unfortunately, the `withered` function isn't provided by the `Witherable` typeclass, but luckily we can write a general implementation for all `Witherables`.

In order to do so, we need a way to "recognize" a failure within our Alternative effect and represent it as a concrete "Maybe". Lucky us, a combinator for this exact purpose exists, it's called `optional`!

```haskell
optional :: Alternative f => f a -> f (Maybe a)
```

When we use `optional` to _lift_ the failure out of the _structure_ into a concrete `Maybe` it also _removes_ that particular failure from the _effect_, yielding an action that will always **succeed** and return either `Just` or `Nothing`.

Let's use it to build a "lensy" combinator in terms of our existing `Witherable` class, this saves us the work of writing a new class and re-implementing all the instances we'd need.

```haskell
withered :: (Alternative f, Witherable t) => (a -> f b) -> t a -> f (t b)
withered f = wither (optional . f)
```

Great! In pretty short order we've constructed a new combinator that fits a signature compatible with other optics, based on a typeclass that has a pretty clear semantic meaning. Now for the fun part, let's see how we can use it!

## Withers as Optics

Any time a new optical structure is discovered we need to find some concrete "actions" which we can run on it.
This usually involves discovering some interesting applications of different concrete types which implement the constraints required by the optic.

To experiment a bit we'll use the most general action available, which works on any optic.
The [`%%~`](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Lens.html#v:-37--37--126-) combinator from `lens` allows us to run any optic if we provide an effectful function which matches the optic's focus and constraints.

```haskell
(%%~) :: LensLike f s t a b -> (a -> f b) -> s -> f t
-- Which expands to:
(%%~) :: ((a -> f b) -> s -> (f t)) -> (a -> f b) -> s -> f t
```

E.g. for a `Traversal s t a b` we can provide a function `Applicative f => a -> f b` and it will return a `s -> f t` for us.

Fun fact, this combinator is actually implemented as just `(%%~) = id`, which in practice just "applies" the optic to the effectful function we provide.
It really does help make things more readable though, so we tend to use it despite the fact that it's really just a glorified `id`.

So what can we do with `withered`? 

At first, the `Maybe` wrapping our result seems like a disadvantage, it's a pain to have to unwrap it after all, but the possibility of failure was the point all along. We can manipulate `withered` to be smart about failure and how to handle it.

Let's try something more creative and a bit closer to `wither`'s strengths.

First we need some sort of operation that might fail.
Parsing is a great use-case, it's always possible that a string may not match
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

Until now I've been using `%%~` to pass explicit `Alternative f => a -> f b` functions, but we can build some handy combinators around it to make it a bit easier to use.

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
-- Filter all odd numbers out from the nested list
>>> [[1, 2, 3], [4, 5, 6]] & filterOf even (withered . withered)
Just [[2],[4,6]]
```

Now that we have a combinator for it, here's another example. We can filter a list that's **in the middle of our structure** based on deeply nested values inside by composing our withers with lenses and prisms!

We'll be running a few examples, so let's set up some data-types:

```haskell
data Address = Address
    { _country :: String
    --  ...
    } deriving (Show, Eq)
data Employee = Employee 
    { _age :: Int
    , _address :: Address
    --  ...
    } deriving (Show, Eq)
data Company = Company 
    { _employees :: [Employee]
    --  ...
    } deriving (Show, Eq)
makeLenses ''Address
makeLenses ''Employee
makeLenses ''Company
```

And here's a company to work with:

```haskell
company :: Company
company = Company
    [ Employee 22 (Address "US")
    , Employee 43 (Address "CA")
    , Employee 35 (Address "NO")
    , Employee 37 (Address "CA")
    ]
```

Check this out:

```haskell
-- Filter our company for all Canadians:
>>> filterOf (== "CA") (employees . withered . address . country) company
Just ( Company 
         [ Employee 43 (Address "CA")
         , Employee 37 (Address "CA")
         ]
     )
```

This appears deceptively simply, but it's pretty impressive what this is all doing for us. Not only does it allow us to filter our employees easily based on **deeply nested state** within each employee, but it allows us to **filter them from a structure that's ALSO nested inside our larger state**! If we were to do this in any way OTHER than using `withered` we'd have to first focus the employees, THEN run a nested filter over the employees separately, and ALSO find a way to filter them based on their nested "country" values.



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
