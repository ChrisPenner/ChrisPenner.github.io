---
title: "Kaleidoscopes: Applicative convolution using lenses"
author: "Chris Penner"
date: "Dec 19, 2019"
tags: [haskell]
description: "A profunctor implementation of kaleidoscopes"
---

In this article we're going to dig into a brand new type of optic, the Kaleidoscope! The theory of which is described in [this abstract](https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf) by Mario Román, Bryce Clarke, Derek Elkins, Jeremy Gibbons, Bartosz Milewski, Fosco Loregian, and Emily Pillmore.

If you haven't read the previous blog post in this series you should definitely read [that one first](https://chrispenner.ca/posts/algebraic).

## Translating from Math

We'll begin like we did last time, by digging into the Math presented in the abstract, translating what we can into Haskell, then filling in the rest with educated guesswork. I talked about characterizations in the previous post, the mathematical characterization of a kaleidoscope presented in the abstract is as follows:

* Kaleidoscope: `π n∈N (A^n → B) → (S^n → T)`

Which I *believe* we can translate into something like this:

* Kaleidoscope: `(f a -> b) -> (f s -> t)`

If you recently read the previous post you might actually recognize this pattern, it's the exact shape of `Costar f a b -> Costar f s t`, but as we also said previously, Costar is effectively just the free Corepresentable, so we can in fact generalize this further into: `Corepresentable p => p a b -> p s t` or: `Corepresentable p => Optic p s t a b`.

Great! So far it looks like we can just piggy-back on `Corepresentable` and see where that leads us. 

One might wonder, if this is just another expression of Corepresentable profunctors is there anything new here? After asking around a bit and watching [Mario's talk](https://youtu.be/ceCwD7L0t3w?t=1100) I learned that Kaleidoscopes are meant to be optics over Applicative Functors, and that we can express the primary behaviour as the following optic:

```haskell
??? :: (Corepresentable p, Applicative f) => Optic p (f a) (f b) a b
```

This is an optic which can focus the values inside **any Applicative structure**. It's almost identical to the `traverse'` combinator from the [`Traversing`](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Traversing.html#v:traverse-39-) profunctor class:

```haskell
traverse' :: (Traversing p, Traversable f) => Optic p (f a) (f b) a b
```

But of course `traverse'` works only on Traversables whereas our new optic works on only Applicatives. Although these have significant overlap, the **behaviour** induced by applicatives is distinct from the behaviour of traversables.

Let's implement a first draft of this new combinator so we can try it out. We need a name for it first, this combinator works by intertwining an applicative with itself through convolution; and also it's pretty complicated to understand, so I'll just make up an appropriate word and we can get on with the rest of the post:

```haskell
convoluted
    :: (Traversable (Corep p), Applicative f, Corepresentable p)
    => p a b
    -> p (f a) (f b)
convoluted p = cotabulate (fmap (cosieve p) . sequenceA)

type Kaleidoscope s t a b = forall p. (Traversable (Corep p),  Corepresentable p) => p a b -> p s t
type Kaleidoscope' s a = Kaleidoscope s s a a
```

This is the most elegant implementation for this combinator I was able to find, if you can figure out a better one which (preferably) doesn't require `Traversable` on the representation of our Corepresentable then please let me know!

Though it may be a smidge awkward it DOES implement the correct signature and allows us to project an optic through any applicative structure!

## Convoluted flowers

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

However our Algebraic lenses didn't give us any way to talk about **grouping** or **convolution** of the elements in the container. This is where Kaleidoscopes come in! They allow us to specify a way to lift an optic through a method of **combining** elements. We do this generally through the use of an Applicative!

Remember the `Traversable` requirement we had on the **container** type of our Corepresentable profunctor? We can use that to flip-flop our Applicative through that container! Effectively that allows us to group elements in the same way that `sequenceA` would (see the `sequenceA` in our implementation?) but operate on them as though they were grouped within the Corepresentable's container. That's as clear as we can get with just words, let's see how a few different Applicatives react to `sequenceA`!

First let's try using the simple list applicative to see what sort of groups we get if we sequence it. We'll unwrap each set of measurements so we've got a value of type `[[Float]]`

```haskell
allMeasurements :: [[Float]]
allMeasurements =
      [ [1  , 2  , 3  , 4  ]
      , [10 , 20 , 30 , 40 ]
      , [100, 200, 300, 400]
      ]

>>> sequenceA allMeasurements
[[1.0,10.0,100.0],[1.0,10.0,200.0],[1.0,10.0,300.0],[1.0,10.0,400.0],
 [1.0,20.0,100.0],[1.0,20.0,200.0],[1.0,20.0,300.0],[1.0,20.0,400.0],
 [1.0,30.0,100.0],[1.0,30.0,20 0.0],[1.0,30.0,300.0],[1.0,30.0,400.0],
 ...
]
```

You get the idea; it goes through every single possible pairing of each measurement! That's not quite what we want in this case! We want to pair the matching measurements together! Sounds like a ZipList to me:

```haskell
>>> sequenceA (ZipList <$> allMeasurements)
ZipList 
  [ [1.0, 10.0, 100.0]
  , [2.0, 20.0, 200.0]
  , [3.0, 30.0, 300.0]
  , [4.0, 40.0, 400.0]
  ]
```

Ahh, much better! Yes I'm aware I could just `traverse` with ZipList 😉. The ZipList applicative pairs elements together pointwise! So if we lift an aggregation through a convolution using `ZipList` we'll get a collection of our original container type (a list) grouped according to the applicative instance we "convoluted" through.  In this case we definitely want the ZipList behaviour, but you can imagine that the cartesian product behaviour of lists might also be useful for other problems.

Now that we know we can use `convoluted` to group up our measurements properly (if we're careful), we can write a version of this kaleidoscope which works specifically over measurements. The zippy applicative seems like it's going to be pretty handy for this sort of thing, so I'll define that as a separate combinator using an `Iso` between ZipList and list alongside the generic `convoluted` optic:

```haskell
pointWise :: Kaleidoscope [a] [b] a b
pointWise = iso ZipList getZipList . convolving
```

We can combine this with another iso for our Measurements newtype wrapper to get what we need:

```haskell
aggregate :: Kaleidoscope' Measurements Float
aggregate = iso getMeasurements Measurements . pointWise
```

Notice that this is a Kaleidoscope over `Float`s on their own, the original list container isn't part of the kaleidoscope, it's determined when we actually run the kaleidoscope with an action. The Kaleidoscope simply requires that it be Traversable, which luckily includes most container types we'd want to use.

To demonstrate what this might end up looking like, here's one more example of what the behaviour of `convoluted` would be if we use a Map for the outer container:

```haskell
measurementMap :: M.Map String (ZipList Float)
measurementMap = M.fromList 
      [ ("setosa"    , ZipList [1  , 2  , 3  , 4  ])
      , ("versicolor", ZipList [10 , 20 , 30 , 40 ])
      , ("virginica" , ZipList [100, 200, 300, 400])
      ]

>>> sequenceA measurementMap
ZipList [ fromList [("setosa", 1.0), ("versicolor", 10.0), ("virginica", 100.0)]
        , fromList [("setosa", 2.0), ("versicolor", 20.0), ("virginica", 200.0)]
        , fromList [("setosa", 3.0), ("versicolor", 30.0), ("virginica", 300.0)]
        , fromList [("setosa", 4.0), ("versicolor", 40.0), ("virginica", 400.0)]
        ]
```

We can see that it beautifully creates a map for each of the measurements, where each of the measurements is labeled according to the original map!

This is pretty nifty, it allows us to carry information from the outer traversable container (Map) **through** the Applicative (ZipList). This is what the `convoluted` optic does for us.

Enough talk, time to implement the example!

## Finishing the example

The example in the abstract is:

```haskell
iris >- measurements . aggregateWith mean
-- Iris Versicolor (5.8 , 3.0 , 3.7 , 1.1)
```

We could of course implement some sort of `aggregateWith` combinator, but the version of `>-` we defined in the previous post already allows us to pass a custom aggregation in, so personally I think it makes more sense to pass `mean` in that way. It also means that we can continue composing more optics onto the chain before we decide how we want to handle the information (maybe we want to aggregate over multiple different convolutions of dimensions?).

Using the version of `(>-)` I defined in the previous post (which has the arguments flipped around a bit), and the `aggregate` we defined above, we can do this:

```haskell
>>> flowers & measurements . aggregate >- mean
Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

It has averaged each measurement across the data-set and has classified the resulting "average flower" as `Versicolor` just as we expected from the simpler version we built in the previous post.

If you're curious about what's actually being passed into our `mean` function we can add a trace statement to find out. With a bit of rearranging and reformatting it looks roughly like this:

```haskell
>>> flowers
[ Flower Versicolor (Measurements [2.0,3.0,4.0,2.0])
, Flower Setosa     (Measurements [5.0,4.0,3.0,2.5])
]

>>> flowers & measurements . aggregate >- mean . traceShowId
[2.0,5.0]
[3.0,4.0]
[4.0,3.0]
[2.0,2.5]

Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

It's passing the groups point-wise just as we'd hoped!

## Kaleidoscopes as lenses

Unfortunately, like algebraic lenses, 

You might be wondering what

















## First guesses at an implementation

Let's start by taking a look at the examples provided in the abstract. Unfortunately we're only given a few very small snippets without any source code or even type-signatures to help us out, so I'll be mostly guessing my way through this. I'll need to take a few creative liberties to get everything to wire together. 

The abstract uses Kaleidoscopes and Algebraic lenses for classifying flowers into species by analyzing their measurements.  These are the examples in the abstract:

```haskell
>>> (iris !! 1) ^. measurements
(4.9 , 3.0 , 1.4 , 0.2)

>>> iris ?. measurements ( Measurements 4.8 3.1 1.5 0.1)
Iris Setosa (4.8 , 3.1 , 1.5 , 0.1)

>>> iris >- measurements . aggregateWith mean
Iris Versicolor (5.8 , 3.0 , 3.7 , 1.1)
```

We're not provided with the implementation of `?.`, `>-`, `measurements`, OR `aggregateWith`. Nor do we have the data-set that builds up `iris`... We don't have a lot to work with here...

I'm going to make some assumptions and we'll build up a dummy data-set of flowers to experiment with:

```haskell
data Species = Setosa | Versicolor | Virginica
  deriving Show

data Measurements = Measurements {getMeasurements :: [Float]}
  deriving Show

data Flower = Flower { flowerSpecies :: Species
                     , flowerMeasurements :: Measurements}
  deriving Show

flower1 :: Flower
flower1 = Flower Versicolor (Measurements [2, 3, 4, 2])

flower2 :: Flower
flower2 = Flower Setosa (Measurements [5, 4, 3, 2.5])

flowers :: [Flower]
flowers = [flower1, flower2]
```

That gives us something to fool around with at least, even if it's not anything like the one used for the paper.

Now for the fun part, we need to start implementing these weird comparison optics. They describe two optics: `measurements` which "encapsulates some learning algorithm which classifies measurements into a species", and "aggregateWith" which somehow runs the provided aggregation function over all the selected values at that stage.

We'll start with `measurements` and see if we can somehow figure out how to embed a classification algorithm into an optic!

I stared at this for the better part of a few weeks before I figured it out, the tough part is to match up the behaviour we need to a profunctor class that encodes that behaviour with as few constraints as possible. My first attempt looked like this:

```haskell
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep
import Data.Foldable

type Optic p s t a b = p a b -> p s t

listLens :: forall p f s t a b
         . (Corepresentable p, Corep p ~ f, Foldable f)
         => (s -> a)
         -> (([s], b) -> t)
         -> Optic p s t a b
listLens project flatten p = cotabulate run
  where
    run :: f s -> t
    run fs = flatten (toList fs, (cosieve p . fmap project) fs)
```

This is a LOT to take in, let's address it in pieces. Firstly, the `Corepresentable` constraint.

Corepresentable has `Cosieve` as a superclass, and so provides us with the following methods:

```haskell
Cosieve p f       => cosieve    :: p a b -> f a -> b
Corepresentable p => cotabulate :: (Corep p d -> c) -> p d c
```

These two functions together allow us to round-trip our profunctor from `p a b` into some `f a -> b` and then back! In fact, this is the essence of what `Corepresentable` means, we can "represent" the profunctor as a function from a value in some context `f` to the result.

These two functions allow us to reflect the profunctor into something we can **actually run**! We can fmap `project` over the `s`'s, run them through the continuation: `f a -> b`, then flatten the whole thing using the provided function.

Don't worry if this doesn't make a ton of sense on its own, it took me a while to figure out. This allows us to write our `measurements` classifier, but we'll need a few helper functions first.

First we'll write a helper to compute the Euclidean distance between two flowers' measurements:

```haskell
measurementDistance :: Measurements -> Measurements -> Float
measurementDistance (Measurements xs) (Measurements ys) =
    sqrt . sum $ zipWith diff xs ys
  where
    diff a b = (a - b) ** 2
```

This will tell us how similar two measurements are, the lower the distance, the more similar!

Next we'll write a function which, given a reference set of flowers will detect the flower which is most similar to a given set of measurements. It will then build a flower out of the estimated species and return it:

```haskell
classify :: ([Flower], Measurements) -> Flower
classify (flowers, m) =
    let Flower species _ = 
          minimumBy (comparing (measurementDistance m . measurements)) flowers
     in Flower species m
```

This function is partial, as we should really be using a Non-Empty list, but I hope you can still somehow sleep at night 😅

Now we have our pieces, we can build a list lens!

```haskell
measurements :: (Corepresentable p, Corep p ~ f, Foldable f) 
             => Optic' p Flower Measurements
measurements = listLens flowerMeasurements classify
```

Okay! Now we have enough to try some things out! The first example is:

```haskell
>>> (iris !! 1) ^. measurements
```

Which we'll translate into:

```haskell
>>> (flowers !! 1) ^. measurements
```

But unfortunately we get an error:

```haskell
• No instance for (Corepresentable
                      (Data.Profunctor.Types.Forget Measurements))
    arising from a use of ‘measurements’
```

Hrmm, looks like `(^.)` uses `Forget` for its profunctor and it doesn't have a `Corepresentable` instance! We'll come back to that soon, let's see if we can get anything else working first. The next example is:

```haskell
iris ?. measurements (Measurements 4.8 3.1 1.5 0.1)
```

I'll admit I don't understand how this example could possibly work, optics necessarily have the type `p a b -> p s t`, so how are they passing a `Measurements` object directly into the optic? I'd guess that it has some other signature, but we know that's not true from the previous example which uses it directly as a lens. Hrmm, I suspect either this example is short-hand pseudocode and isn't meant to run as-is, or perhaps I'm missing something sneaky (the latter is more likely).

Well, I can't imagine any way this could work, but let's build an alternative version that gets close!

It appears as though `(?.)` is some sort of **action** which runs the optic, which means we need to take an optic, and some arguments, then specialize the profunctor so we can run them! We know we need something that's `Corepresentable`, and the easiest instance for that is definitely `Costar`! Here's what it looks like:

```haskell
newtype Costar f a b = Costar (f a -> b)
```

If we specialize to Costar and swap a few of our arguments around we can end up with:

```haskell
(?.) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
(?.) xs f a = (runCostar $ f (Costar (const a))) xs
```

Which lets us write:

```haskell
>>> (flowers ?. measurements) $ Measurements [5, 4, 3, 1]
Flower Setosa (Measurements [5.0,4.0,3.0,1.0])
```

Which is *really* close to 

```haskell
>>> iris ?. measurements (Measurements 4.8 3.1 1.5 0.1)
```

We see that even though we only enter in some measurements, it successfully finds the flower in the data-set which has the closest measurements and assigns the resulting flower to the correct species `Setosa`!

We can change the measurements to try to get closer to our `Versicolor` flower, which has measurements:

```haskell
flower1 :: Flower
flower1 = Flower Versicolor (Measurements [2, 3, 4, 2])
```

```haskell
>>> (flowers ?. measurements) $ Measurements [2.2, 3.5, 4.1, 1.9]
Flower Versicolor (Measurements [2.2,3.5,4.1,1.9])
```

It seems to work! Great!

The next example moves on to including a new optic, so before we move onto that, let's see if we can fix our current setup so we can use `(^.)` properly.

We know that `Forget` doesn't have a valid instance of `Corepresentable`, but perhaps there's some other *weaker* abstraction we can invent which works for our purposes.

The "essence" of our algebraic lens is that we can construct an optic out of our helper functions, what if we simply embed that functionality into a profunctor class directly?

```haskell
class Profunctor p => Algebraic p where
  algebraic :: (s -> a) -> (([s], b) -> t) -> p a b -> p s t
```

We could probably be more general over the `[]`, but we won't worry about that now. We've defined a typeclass which represents profunctors which can lift our algebraic operation into a profunctor morphism (a.k.a. an optic)!

Now that we have a typeclass we'll implement our Costar instance again:

```haskell
instance (Functor f, Foldable f) => Algebraic (Costar f) where
  algebraic project flatten p = cotabulate run
    where
      run s = flatten (toList s, cosieve (lmap project p) s)
```

Technically this implementation works on any Corepresentable profunctor, but we'll be specific.

Now we need to see if we can implement an instance for `Forget`, if we can manage that, then we can use `view` over our `measurements` optic!

```haskell
instance Algebraic (Forget r) where
  algebraic project _flatten (Forget f) = Forget (f . project)
```

Well that was pretty painless!

Okay, with that, we just need to re-implement our `measurements` optic using `Algebraic`:

```haskell
type ListLens s t a b = forall p. Algebraic p => p a b -> p s t
type ListLens' s a = ListLens s s a a

measurements :: ListLens' Flower Measurements
measurements = algebraic flowerMeasurements classify
```

Then we can view through our `measurements` optic, fulfilling the first example:

```haskell
>>> (flowers !! 1) ^. measurements
Measurements {getMeasurements = [5.0,4.0,3.0,2.5]}
```

As it turns out, `(->)` is also a Corepresentable profunctor, so we can implement `Algebraic` for that too!

```haskell
instance Algebraic (->) where
  algebraic project flatten p = run
    where
      run s = flatten ([s], p . project $ s)
```

And in profunctor-land, `(->)` is used for setters and modifiers, so we can now set/modify measurements through our algebraic lens!

```haskell
>>> flower1 & measurements .~ Measurements [9, 8, 7, 6]
Flower Versicolor Measurements [9.0,8.0,7.0,6.0]
```

Since we can get and set, our algebraic lens is indeed a full-blown lens! This is really interesting actually since we didn't make any use of `Strong` which is how most lenses are implemented.

Here's one last interesting instance:

```haskell
instance Algebraic Tagged where
  algebraic project flatten (Tagged b) = Tagged (flatten ([], b))
```

Tagged is used for `Review`, which means we can try running our algebraic lens as a review:

```haskell
>>> review measurements (Measurements [1, 2, 3, 4])
Flower {flowerSpecies = *** Exception: Prelude.foldl1: empty list
```

Unfortunately this doesn't work because `classify` is partial when the container is empty, we should probably add additional constraints to prevent this from happening in the future, but what it DOES mean is that we can use our algebraic lenses with `review` so long as they don't assume a non-empty container, pretty cool!

## Aggregating with Kaleidoscopes

Now that we've got our classifier lens all figured out, we'd love to be able to use it to run classification over our data-set in interesting ways! The abstract provides an example:

```haskell
>>> iris >- measurements . aggregateWith mean
Iris Versicolor (5.8 , 3.0 , 3.7 , 1.1)
```

This seems to be getting the 'mean' values for each of the four measurements, then classifying that result as a `Versicolor` flower according to the source data.

This suggests a type signature something like:

```haskell
(>-) :: [s] -> Optic p s t a b -> t
```

But we know that in order to **run** an optic like this we'll need to provided it with an initial optic for it to transform. The only way I can think to do that in this case is if `aggregateWith` has a signature something like:

```haskell
aggregateWith :: ([a] -> b) -> Optic p [a] b () ()
```

Or perhaps:

```haskell
aggregateWith :: ([a] -> b) -> Optic p [a] [a] a a
```

Both of these result in some tricky implementations for `(>-)`, and I wasn't able to find one I was happy with. It's possible I'm missing something here.

Instead, I ended up swapping the arguments around a bit and ended up using the continuation as the "aggregation" function, meaning we can write something like this:

```haskell
>>> flowers & measurements . aggregate >- mean
```

Which looks pretty lensy, and does the job! Here's what my version looks like:

```haskell
(>-) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(>-) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs
```

This will run any optic over Costar using an aggregation function over the focus!

But I'm getting ahead of myself, what's this `aggregate` optic I'm using?

The secret-sauce behind `aggregate` is an optic which allows you "lift" your computation **through** an arbitrary `Applicative`! In our case, we want to lift our measurements of type `[Float]` into an operation which runs over the **pointwise** comparison of each measurement, effectively, we want to run a function `[Float] -> Float` where the list of floats contains ALL of the nth measurements across all the flowers.

The abstract gives us a hint in the form of the characterization of a kaleidoscope, which we translated into: `(f a -> b) -> (f s -> t)`, which, if we substitute in `Costar` instead of `(->)`, gives us: `Costar f a b -> Costar f s t`, which is an optic!

```haskell

```

