---
title: "Kaleidoscopes and Algebraic optics"
author: "Chris Penner"
date: "Dec 14, 2019"
tags: [haskell]
description: "A profunctor implementation of algebraic lenses and kaleidoscopes"
---

In this article we're going to dig into some exciting new optics, the theory of which is described in [this abstract](https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf) by Mario Román, Bryce Clarke, Derek Elkins, Jeremy Gibbons, Bartosz Milewski, Fosco Loregian, and Emily Pillmore.

I'm not really a "Mathy" sort of guy, I did very little formal math in university, and while I've become comfortable in some of the absolute basics of Category Theory through my travels in Haskell, I certainly wouldn't consider myself well-versed. I AM however very well versed in the practical uses of optics, and so of course I need to keep myself up to speed on new developments, so when this abstract became available I set to work trying to understand it!

Most of the symbols and Category Theory went straght over my head, but I managed to pick out a few bits and pieces that we'll look at today. I'll be translating what little I understand into a language which I DO understand: Haskell!

If the aabove wasn't enough of a disclaimer I'll repeat: I don't really understand most of the math behind this stuff, so it's very possible I've made a few errors (though I think the result I've come to is interesting on its own, even if not a perfect representation of the abstract). Please correct me if you know better :)

We'll be looking at two new types of optics from the abstract, namely the "Algebraic lens" and the "Kaleidoscope". We'll build a functioning implementation of these optics using Profunctors and rebuild the example presented in the abstract.

Here we go!

## Translating from Math

We'll start by taking a look at the formal definition of the "characterization" of these two optics. Effectively, what sort of thing allows you to unambiguously define the behaviour of each of these optics. The paper presents them as follows: (my apologies for lack of proper LaTeX on my blog 😬)

* Algebraic Lens: `(S → A) × (ψS × B → T)`
* Kaleidoscope: `π n∈N (A^n → B) → (S^n → T)`

That's likely to make even LESS sense than it does when it's rendered properly, so feel free to check it out in the [abstract](http://events.cs.bham.ac.uk/syco/strings3-syco5/slides/roman.pdf) instead.

I'm not hip to all these crazy symbols, but as best as I can tell, we can translate it roughly like this:

* Algebraic Lens: `(s -> a, (f s, b) -> t) -> Optic p s t a b`
* Kaleidoscope: `(f a -> b) -> (f s -> t) -> Optic p s t a b`

Rougly speaking, an Algebraic lens allows us to run some *aggregation* on a focus within *collection* of `s`'s, then use the result of the aggregation to pick a `t` to return. We can compare the `b` with the original collection of `s`'s when building/selecting our `t`.

The Kaleidoscope effectively allows us to project an optic through an `Applicative`, where the applicative's behaviour affects how we "collect" all the a's together.

This is all very fuzzy and vague, so let's start trying to implement this.

## First guesses at an implementation

Let's start by taking a look at the examples provided in the abstract. Unfortnately we're only given a few very small snippets without any source code or even type-signatures to help us out, so I'll be mostly guessing my way through this. I'll need to take a few creative liberties to get everything to wire together. 

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

Here are a few more handy instances:

```haskell
instance Algebraic (->) where
  algebraic project flatten p = run
    where
      run s = flatten ([s], p . project $ s)

instance Algebraic Tagged where
  algebraic project flatten (Tagged b) = Tagged (flatten ([], b))
```

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
