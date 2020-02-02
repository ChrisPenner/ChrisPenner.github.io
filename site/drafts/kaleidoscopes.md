---
title: "Kaleidoscopes: Applicative convolution using lenses"
author: "Chris Penner"
date: "Dec 19, 2019"
tags: [haskell]
description: "A profunctor implementation of kaleidoscopes"
---

In this article we're going to dig into a brand new type of optic, the Kaleidoscope! The theory of which is described in [this abstract](https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf) by Mario Román, Bryce Clarke, Derek Elkins, Jeremy Gibbons, Bartosz Milewski, Fosco Loregian, and Emily Pillmore.

If you haven't read the previous blog post in this series you should definitely read [that one first](https://chrispenner.ca/posts/algebraic).

## A new profunctor class

Since the last article was released I had a chance to chat with Mario, the author of the abstract, on twitter which has been very helpful! He explained that the Kaleidoscope characterization in the abstract:

* Kaleidoscope: `π n∈N (A^n → B) → (S^n → T)`

Is represented by the following Profunctor class:

```haskell
class MStrong p => Reflector p where
  reflected :: Applicative f => p a b -> p (f a) (f b)


type Kaleidoscope s t a b = forall p. Reflector p => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a
```

It provides an optic which focuses the contents of any Applicative structure! It has a superclass of the `MStrong` profunctor noted in the corrections at the bottom of the previous post. It's trivially witnessed by `msecond' = reflected` since `Monoid m => (m, a)` is an Applicative. 

At first `reflected` seems almost identical to the `traverse'` combinator from the [`Traversing`](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Traversing.html#v:traverse-39-) profunctor class:

```haskell
traverse' :: (Traversing p, Traversable f) => Optic p (f a) (f b) a b
```

But of course `traverse'` works only on Traversables whereas our new optic works on only Applicatives. Although these have significant overlap, the **behaviour** induced by applicatives is distinct from the behaviour of traversables.

Let's implement our new `Reflector` class for a few profunctors so we can actually try it out!

```haskell
-- (->) Allows us to set or update values through a reflector
instance Reflector (->) where
  reflected = fmap

-- Costar allows us to run aggregations over collections like we did in the previous post
instance Traversable f => Reflector (Costar f) where
  reflected (Costar f) = Costar (fmap f . sequenceA)

-- Tagged allows us to "review" through a Reflector
instance Reflector Tagged where
  reflected (Tagged b) = Tagged (pure b)

-- Star allows us to calculate several 'projections' of focuses 
instance Distributive f => Reflector (Star f) where
  reflected (Star f) = Star (collect f)
```

These are the usual suspects, in optics they correspond to the ability to run the following actions:

* `(->)`: `set`/`modify`
* `Costar`: `(?.)`/`(>-)` (from the last post)
* `Tagged`: review
* `Star`: traverseOf

Unfortunately we can't implement this profunctor for `Forget`, so we won't be able to `view` or `fold` over Kaleidoscopes. C'est la vie!

Now that we have a profunctor class we can write out the type of optic this profunctor entails:


```haskell
type Kaleidoscope s t a b = forall p. Reflector p => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a
```

A Kaleidoscope is an optic which transforms 'Reflector' profunctors.

## Grouping with kaleidoscopes

In the previous post we managed to fill out most of the examples from the case study using Algebraic Lenses, but we got stuck when it came to **aggregating** over the individual measurements of the flowers in our data-set. We wanted to somehow aggregate over constituent pieces of our data-set all at once, for instance if we had the following measurements as a silly example:

```haskell
Measurements [1, 2, 3, 4]
Measurements [10, 20, 30, 40]
Measurements [100, 200, 300, 400]
```

We want to average them across each **column**, so we want to end up with:

```haskell
Measurements 
  [ mean [1, 10, 100]
  , mean [2, 20, 200]
  , mean [3, 30, 300]
  , mean [4, 40, 400]
  ]
```

However our Algebraic lenses didn't give us any way to talk about **grouping** or **convolution** of the elements in the container. Depending on the **action** we're running on the resulting optic this has different meanings. When using `set`, `over` for example a kaleidoscope simply updates each value in the collection just like a traversal would; however when running with the `>-` aggregation operator from the previous post a kaleidoscope re-groups elements within the aggregation container! We'll see what this means in a moment. Another interesting case comes from `review`; when `review`ing through a kaleidoscope we use `pure` to embed a value into the Applicative.

Before we get too far, let's try out the 'basic' kaleidoscope. `reflected` from the `Reflector` is an optic as it is, the *pretty* signature looks like this:

```haskell
reflected :: Applicative f => Kaleidoscope (f a) (f b) a b
```

This is the Bizarro universe version of `traversed`, it allows an optic to focus within an Applicative rather than a Traversable. Let's try it out on a few things and see what happens.

```haskell
-- When updating/setting it simply focuses each 'element' of the applicative.
-- It's indistinguishable from `traversed` in this case
>>> [1, 2, 3] & reflected %~ (*10)
[10,20,30]

-- We can compose it to nest deeper of course!
>>> [[1, 2, 3], [4, 5, 6]] & reflected . reflected %~ (*10)
[[10,20,30],[40,50,60]]
>>> [[1, 2, 3], [4, 5, 6]] & reflected . reflected %~ (*10)
[[10,20,30],[40,50,60]]

-- We can review through 'reflected' to embed a value into any Applicative!
>>> review reflected 1 :: Either () Int
Right 1
-- We can compose with prisms
>>> review (_Just . reflected) 1 :: Maybe [Int]
Just [1]
-- It works for any Applicative
>>> review (reflected . reflected) 1 :: Maybe [Int]
Just [1]

-- Unfortunately kaleidoscopes don't allow viewing or folding :'(
>>> [[1, 2, 3], [4, 5, 6]] ^.. reflected . reflected
error:
    • No instance for (Reflector
                         (Data.Profunctor.Types.Forget [Integer]))
```

Okay, so that covers `review`, `set` and `over`, what about `>-`? This is where things start to get fun!

Remember that `>-` has this type:

```haskell
(>-) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
```

So this means it aggregates some **outer** container `f` over the focuses of the provided optic. The hardest part here is keeping track of which container is the collection used by `>-`, and which is the `Applicative` handled by `reflected`. To help keep things separate I'll introduce the simple `Pair` type, defined like so:

```haskell
data Pair a = Pair a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')
```

Now let's run a few simple aggregations to *start* to understand `reflected`.

```haskell
-- Here 'reflected' uses Applicative of the List 
-- to group elements from each of the values in the outer Pair.
-- By using `show` as our aggregation we can see each grouping it produced
>>> Pair [1, 2] [3, 4] & reflected >- show
["Pair 1 3","Pair 1 4","Pair 2 3","Pair 2 4"]
```

A few things to notice here: First, the structure of outer collection gets shifted to the inside where it was aggregated! Secondly we can see that we have a value representing each possible pairing of the two lists in our pair. `reflected` used the Applicative instance of the list to determine the groupings, and the Applicative for lists finds all possible pairings, like this:

```haskell
>>> liftA2 Pair [1, 2] [3, 4]
[Pair 1 3,Pair 1 4,Pair 2 3,Pair 2 4]
```

What happens if we run the same thing with the containers flip-flopped? Then `reflected` will group elements using the `Pair` applicative which matches the first elements of each pair and the second elements of each Pair together.

Note that the reason we can flip-flop them like this is that Lists and Pairs each have both `Applicative` AND `Traversable` instances.

```haskell
>>> [Pair 1 2, Pair 3 4] & reflected >- show
Pair "[1,3]" "[2,4]"
```

This time we can see we've grouped the elements into lists by their positions in the pair!

Let's try one more; we'll use a Map for the outer container.

```haskell
>>> M.fromList [('a', Pair 1 2), ('b', Pair 3 4)] & reflected >- show
Pair "fromList [('a',1),('b',3)]" "fromList [('a',2),('b',4)]"
```

This creates two separate maps, one with the elements from the first portion of the pair, the other with the second portion.

## Back to measurements

So, back to our flower problem, now that we very *roughly* understand how `reflected` groups elements for aggregation we'll see how we can use it to group respective measurements of our flowers.

Here's our simplified representation of our problem:

```haskell
allMeasurements :: [[Float]]
allMeasurements =
      [ [1  , 2  , 3  , 4  ]
      , [10 , 20 , 30 , 40 ]
      , [100, 200, 300, 400]
      ]
```

We want to get the average of the first elements, second elements, and third elements respectively (e.g. average of a **column** rather than the **row**). We can use the outer list as our container, but if we try to use `reflected` as is it'll find ALL POSSIBLE GROUPINGS! 

```haskell
>>> allMeasurements & reflected >- show
["[1,10,100]","[1,10,200]","[1,10,300]","[1,10,400]","[1,20,100]","[1,20,200]"
,"[1,20,300]","[1,20,400]","[1,30,100]","[1,30,200]","[1,30,300]","[1,30,400]"
, ...
]
```

Not what we want! We need to use a different `Applicative` instance! We want the applicative to *zip* each matching element together... sounds like a ZipList to me!

Let's try this instead:

```haskell
zippyMeasurements :: [ZipList Float]
zippyMeasurements =
      [ ZipList [1  , 2  , 3  , 4  ]
      , ZipList [10 , 20 , 30 , 40 ]
      , ZipList [100, 200, 300, 400]
      ]

>>> zippyMeasurements & reflected >- show
ZipList [ "[1,10,100]"
        , "[2,20,200]"
        , "[3,30,300]"
        , "[4,40,400]"
        ]
```

Much better! It's common to want this alternate Applicative behaviour for lists, so I'll define a new kaleidoscope which uses the zippy behaviour for lists:

```haskell
-- Use an iso to wrap/unwrap the list in ZipList before reflecting
zipWise :: Kaleidoscope [a] [b] a b
zipWise = iso ZipList getZipList . reflected
```

Great! Now we can use `zipWise` to do our grouping, and we'll run `mean` as our aggregation rather than `show`'ing

```haskell
>>> let mean xs = fromIntegral (sum xs) / fromIntegral (length xs)
>>> allMeasurements & zipWise >- mean
[37.0, 74.0, 111.0, 148.0]
```

Now we're really close, but we need to tag on another iso to get inside the `Measurements` wrapper our flowers use:

```haskell
aggregate :: Kaleidoscope' Measurements Float
aggregate = iso getMeasurements Measurements . zipWise
```

## Finishing the example

Kaleidoscopes compose nicely with the algebraic lenses we built in the previous post, meaning we can use the `measurements` algebraic lens we built and compose it with our new `aggregate` kaleidoscope! Using all the same `flowers` from the previous post:

```haskell
>>> flowers & measurements . aggregate >- mean
Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

This is doing a LOT for us; it takes a list of flowers, focuses their measurements, averages them zipwise across their independent measurements, then *classifies* the complete set of average measurements as a species using euclidean difference over measurements with the original data set! As it turns out, the 'average' flower is closest to the `Versicolor` species (in my completely made up data set)!

## Other nifty tricks

There's an alternative version of our `Reflector` which depends on `Apply` and `Traversable1` instead of `Applicative` and `Traversable`! It doesn't allow `review`, but opens up `reflected` to work on anything implementing `Apply`; which is particularly handy since that allows us to now `reflect` our way into Maps!

Here's the alternative version of our Reflector class:

```haskell
import Data.Profunctor
import Data.Profunctor.MStrong
import Data.Functor.Apply
import Data.Semigroup.Traversable

class MStrong p => Reflector p where
  reflected :: Apply f => p a b -> p (f a) (f b)

instance Traversable1 f => Reflector (Costar f) where
  reflected (Costar f) = Costar (fmap f . sequence1)
```

This is only a peek at what's possible, but with this version we can use `reflected` to group elements key-wise across all Maps in a non-empty collection.

Let's say we manage several business and have a list of all their profits and expenses. We can group and aggregate over all of their expenses and profits respectively! Again, think of reflected as allowing you to do "column-wise" aggregations, although certain Applicatives provide other intuitions.

Here's the sum of all profits and expenses across all of our businesses

```haskell
>>> let xs = M.fromList [("profits", 1), ("expenses", 2)] 
             :| [ M.fromList [("profits", 10), ("expenses", 20)]
                , M.fromList [("profits", 100), ("expenses", 200)]
                ]
>>> xs & reflected >- sum
fromList [("expenses",222),("profits",111)]

-- Average expenses, average profit
>>> xs & reflected >- mean
fromList [("expenses",74.0),("profits",37.0)]

-- Largest expenses and profit
>>> xs & reflected >- maximum
fromList [("expenses",200),("profits",100)]
```

This is still a new, exciting, and unexplored area of optics; but I suspect that once libraries begin to adopt it we'll have an even more adaptable model for querying and aggregating across many records. Optics are getting close to the expressive power of dedicated query languages like SQL!
