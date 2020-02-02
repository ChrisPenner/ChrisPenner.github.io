---
title: "Kaleidoscopes: Optics for Grouping with Applicatives"
author: "Chris Penner"
date: "Feb 2, 2020"
tags: [haskell]
description: "An introduction to kaleidoscopes"
image: "kaleidoscope.jpg"
---

This is a blog post about optics, if you're at all interested in optics I suggest you go check out my book: [Optics By Example](https://leanpub.com/optics-by-example). Although this post covers a more advanced topic the book covers everything you need to go from a beginner to master in all things optics! Check it out and tell your friends, now onwards to the post you're here for.

In this article we're going to dig into a brand new type of optic, the Kaleidoscope! The theory of which is described in [this abstract](https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf) by Mario Román, Bryce Clarke, Derek Elkins, Jeremy Gibbons, Bartosz Milewski, Fosco Loregian, and Emily Pillmore.

If you haven't read the [previous blog post](https://chrispenner.ca/posts/algebraic) in this series it's not required, but might help to build a stronger understanding.

## What is a Kaleidoscope?

Like most optics, the behaviour of a kaleidoscope is mostly dictated by the type of profunctor you're passing through it; but from a bird's eye view I'd say that kaleidoscopes allow you to perform aggregations, comparisons, and manipulations over different **groupings** of focuses. They can allow you to calculate summaries of each column of a table, or calculate aggregations over each respective key in a list of JSON objects! They perform this grouping using Applicative instances and can end up with interesting behaviour depending on the type of Applicative you're working with.

I have hopes that as kaleidoscopes are added to more optics libraries and the kinks get worked out that we'll end up with an expressive optics language which let us express complex table queries and mutations like SQL queries do!

## A new profunctor class

For this post we'll look at kaleidoscopes from a profunctor optics perspective, meaning we'll need a profunctor class which encompasses the behaviour of the optic.

Since the last article was released I had a chance to chat with Mario, the author of the abstract, on twitter which has been very helpful! He explained that the Kaleidoscope characterization in the abstract:

* Kaleidoscope: `π n∈N (A^n → B) → (S^n → T)`

Is represented by the following Profunctor class:

```haskell
class MStrong p => Reflector p where
  reflected :: Applicative f => p a b -> p (f a) (f b)
```

Reflector has a superclass of the `MStrong` profunctor noted in the corrections at the bottom of the previous post. That every Reflector is MStrong is trivially witnessed by `msecond' = reflected` since `Monoid m => (m, a)` is an Applicative (namely the **Writer** Applicative). 

With this class defined we can say that a Kaleidoscope is any optic which depends on `Reflector`:

```haskell
type Kaleidoscope s t a b = forall p. Reflector p => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a
```

This provides an optic which focuses the contents of any Applicative structure! 

Here's what we get when we write `reflected` with its new signature:

```haskell
reflected :: Applicative f => Kaleidoscope (f a) (f b) a b
```

At first `reflected` seems almost identical to the `traverse'` combinator from the [`Traversing`](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Traversing.html#v:traverse-39-) profunctor class:

```haskell
traverse' :: (Traversing p, Traversable f) => Optic p (f a) (f b) a b
```

But of course `traverse'` works only on Traversables whereas our new optic works only on Applicatives. Although these have significant overlap, the **behaviour** induced by the use of applicative structure is distinct from the behaviour of traversables.

Let's implement our new `Reflector` class for a few common profunctors so we can actually try it out!

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

Unfortunately we can't implement an instance of `Reflector` for `Forget`, so we won't be able to `view` or `fold` over Kaleidoscopes. C'est la vie!

The class and optic are all set up! Let's see what they can do.

## Grouping with kaleidoscopes

In the previous post we managed to fill out most of the examples from the case study using Algebraic Lenses, but we got stuck when it came to **aggregating** over the individual measurements of the flowers in our data-set. We wanted to somehow aggregate over constituent pieces of our data-set all at once, for instance if we had the following measurements as a silly example:

```haskell
Measurements [1  , 2  , 3  , 4  ]
Measurements [10 , 20 , 30 , 40 ]
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

However our Algebraic lenses didn't give us any way to talk about **grouping** or **convolution** of the elements in the container, but kaleidoscopes are going to help us get there!

Let's try just running our optic in a few different ways that type-check and see what happens.

```haskell
-- When updating/setting it simply focuses each 'element' of the applicative.
-- It's indistinguishable from `traversed` in this case
>>> [1, 2, 3] & reflected %~ (*10)
[10,20,30]

-- We can compose it to nest deeper into multiple stacked applicatives of course!
>>> [[1, 2, 3], [4, 5, 6]] & reflected . reflected %~ (*10)
[[10,20,30],[40,50,60]]
>>> [[1, 2, 3], [4, 5, 6]] & reflected . reflected %~ (*10)
[[10,20,30],[40,50,60]]

-- Since `Tagged` is a `Reflector` we can 'review' through 'reflected' 
-- This will embed a value into any Applicative as though we'd used 'pure'
>>> review reflected 1 :: Either () Int
Right 1
-- We can compose with prisms
>>> review (_Just . reflected) 1 :: Maybe [Int]
Just [1]

>>> review (reflected . reflected) 1 :: Maybe [Int]
Just [1]

-- Unfortunately kaleidoscopes don't allow viewing or folding :'(
-- They can still be composed with lenses and traversals, but the resulting optic
-- can only be used as a setter, or as a traversal with Distributive Functors.
>>> [[1, 2, 3], [4, 5, 6]] ^.. reflected . reflected
error:
    • No instance for (Reflector
                         (Data.Profunctor.Types.Forget [Integer]))

>>> [[1, 2, 3], [4, 5, 6]] & traversed . reflected %~ (*10)
[[10,20,30],[40,50,60]]
```

Okay, so that covers `review`, `set` and `over`, what about `>-`? This is where things start to get fun!

Remember from the last post that `>-` has this type:

```haskell
(>-) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
```

So this means it aggregates some **outer** container `f` over the focuses of the provided optic. The hardest part here is keeping track of which container is the collection handled by `>-` and which is the `Applicative` handled by `reflected`. To help keep things separate I'll introduce the simple `Pair` type, defined like so:

```haskell
data Pair a = Pair a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')
```

Now let's run a few simple aggregations to *start* to understand `reflected`.

```haskell
-- Here 'reflected' uses the List's Applicative instance 
-- to group elements from each of the values in the outer Pair.
-- By using `show` as our `(f a -> b)` aggregation we can see 
-- each grouping was passed to our aggregation function.
>>> Pair [1, 2] [3, 4] & reflected >- show
["Pair 1 3","Pair 1 4","Pair 2 3","Pair 2 4"]
```

A few things to notice here. First, the structure of outer collection gets shifted to the inside where it was aggregated, the `Pair` was flattened by our aggregation, leaving only the list's structure. Secondly we can see that we have a value representing each possible pairing of the two lists in our pair. `reflected` used the Applicative instance of the list to determine the groupings, and the Applicative for lists finds all possible pairings. Basically it did this:

```haskell
>>> liftA2 Pair [1, 2] [3, 4]
[Pair 1 3, Pair 1 4, Pair 2 3, Pair 2 4]
```

What happens if we run the same thing with the containers flip-flopped? Then `reflected` will group elements using the `Pair` applicative which matches the elements of each pair zip-wise.

Note that the reason we can flip-flop them like this is that Lists and Pairs each have `Applicative` AND `Traversable` instances. This time we're using the List's Traversable instance and the Pair's Applicative.

```haskell
>>> [Pair 1 2, Pair 3 4] & reflected >- show
Pair "[1,3]" "[2,4]"
```

This time we can see we've grouped the elements into lists by their positions in the pair! Again, the outer structure was flattened by the aggregation after being grouped using the inner applicative instance.

Let's try one more; we'll use a Map for the outer container.

```haskell
>>> M.fromList [('a', Pair 1 2), ('b', Pair 3 4)] & reflected >- show
Pair "fromList [('a',1),('b',3)]" "fromList [('a',2),('b',4)]"
```

This creates two separate maps which have been grouped into Maps using the Applicative instance of the inner Pair! In this case it grouped elements zipwise across maps! Take a moment to see how this behavior is perhaps not very intuitive, but is nonetheless a very useful way of grouping data.

## Back to measurements

So, back to our flower problem from the previous post, now that we very *roughly* understand how `reflected` groups elements for aggregation we'll see how we can use it to group respective measurements of our flowers.

Here's our simplified representation of our problem, we've got a collection of flower measurements. Each element of each list represents a unique measurement, for example maybe the first measurement in each list (the first column) represents leaf length, and the last measurement in each (the last column) represents stem length. Here's our silly simplified data set:

```haskell
allMeasurements :: [[Float]]
allMeasurements =
      [ [1  , 2  , 3  , 4  ]
      , [10 , 20 , 30 , 40 ]
      , [100, 200, 300, 400]
      ]
```

Our task is to get the average of the **each individual measurement**, i.e. the average of each **column** rather than each **row**, averaging different measurement types together doesn't make sense. We've seen how `reflected` can help us group data across an outer container, but if we try to use `reflected` here it will find ALL POSSIBLE GROUPINGS! Let's see how this gong-show unfolds:

```haskell
>>> allMeasurements & reflected >- show
["[1,10,100]","[1,10,200]","[1,10,300]","[1,10,400]","[1,20,100]","[1,20,200]"
,"[1,20,300]","[1,20,400]","[1,30,100]","[1,30,200]","[1,30,300]","[1,30,400]"
, ... ad-nauseum
]
```

Hrmmm, this makes sense when we think about it; we're grouping elements using the list applicative which performs a cartesian product yielding all possible combinations! Instead we really want to group elements by their position within the list, we'll need a different `Applicative` instance!  Since we want the applicative to *zip* each matching element together I suspect a `ZipList` might be just what the doctor ordered!

If you're unfamiliar with `ZipList` it's a newtype around lists provided in `Control.Applicative`. Take a look at how its Applicative instance differs from plain old lists:

```haskell
-- List Applicative is cartesian product; e.g. all combinations.
>>> liftA2 (,) [1, 2, 3] [4, 5, 6]
[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

-- ZipList Applicative does zipwise pairing instead!
>>> liftA2 (,) (ZipList [1, 2, 3]) (ZipList [4, 5, 6])
ZipList [(1,4),(2,5),(3,6)]
```

That looks more like what we want, the results end up grouped according to their position in the list.

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

Much better! It's common to want this alternate Applicative behaviour for lists, so I'll define a new kaleidoscope which uses this zippy behaviour for lists:

```haskell
-- Use an iso to wrap/unwrap the list in ZipList before reflecting
zipWise :: Kaleidoscope [a] [b] a b
zipWise = iso ZipList getZipList . reflected
```

Great! Now we can use `zipWise` to do our grouping, and we'll run `mean` as our aggregation rather than `show`'ing

```haskell
-- Here's an averaging function
mean :: Foldable f => f Float -> Float
mean xs = sum xs / fromIntegral (length xs)

-- `zipWise` will group measurements by type (e.g. get lists representing columns)
-- then we can take the average of each column, which will be collected back into a single row.
>>> allMeasurements & zipWise >- mean
[37.0, 74.0, 111.0, 148.0]
```

Cool! We use any aggregation function in place of `mean`, so we could get the `sum`, `median`, `maximum`, or `minimum` of each column, whatever you like!

## Finishing the example

Let's finish up the example from the previous post; if you haven't read that recently this part might not make much sense.

Our new `zipWise` kaleidoscope is almost what we need, but we'll need another iso to let it accept the `Measurements` wrapper our flowers use:

```haskell
aggregate :: Kaleidoscope' Measurements Float
aggregate = iso getMeasurements Measurements . zipWise
```

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
