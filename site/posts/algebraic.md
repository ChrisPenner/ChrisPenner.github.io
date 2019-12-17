---
title: "Algebraic lenses"
author: "Chris Penner"
date: "Dec 14, 2019"
tags: [haskell]
description: "A profunctor implementation of algebraic lenses"
---

In this article we're going to dig into an exciting new type of optics, the theory of which is described in [this abstract](https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf) by Mario Román, Bryce Clarke, Derek Elkins, Jeremy Gibbons, Bartosz Milewski, Fosco Loregian, and Emily Pillmore.

I'm not really a "Mathy" sort of guy, I did very little formal math in university, and while I've become comfortable in some of the absolute basics of Category Theory through my travels in Haskell, I certainly wouldn't consider myself well-versed. I AM however very well versed in the practical uses of optics, and so of course I need to keep myself up to speed on new developments, so when this abstract became available I set to work trying to understand it!

Most of the symbols and Category Theory went straght over my head, but I managed to pick out a few bits and pieces that we'll look at today. I'll be translating what little I understand into a language which I DO understand: Haskell!

If the above wasn't enough of a disclaimer I'll repeat: I don't really understand most of the math behind this stuff, so it's very possible I've made a few errors (though I think the result I've come to is interesting on its own, even if not a perfect representation of the abstract). Please correct me if you know better :)

There are several new types of optics presented in the paper, we'll start by looking at one of them in particular, but will set the groundwork for the others which I'll hopefully get to in future posts. Namely we'll be looking at "Algebraic lenses".

Here we go!

## Translating from Math

We'll start by taking a look at the formal definition of the "characterization" of algebraic lenses. By characterization of an optic I mean a set of values which completely describe the behaviour of that optic. For instance a Lens is characterized by a getter and a setter: `(s -> a, s -> b -> t)`, and an Iso is characterized by its `to` and `from` functions: `(s -> a, b -> t)`.

The paper presents the characterization of an algebraic lens like this: (my apologies for lack of proper LaTeX on my blog 😬)

* Algebraic Lens: `(S → A) × (ψS × B → T)`

That's likely to make even LESS sense than it does when it's rendered properly as a pdf, so feel free to check it out in the [abstract](http://events.cs.bham.ac.uk/syco/strings3-syco5/slides/roman.pdf) instead.

I'm not hip to all these crazy symbols, but as best as I can tell, we can translate it roughly like this:

* Algebraic Lens: `(s -> a, f s -> b -> t)`

If you squint a bit, this looks really close to the characterization of a lens, the only difference being that instead of a *single* `s` we have some container `f` filled with them. The type of container we choose further specifies what type of algebraic lens we're dealing with. For instance, the paper calls it a `List Lens` if `f` is chosen to be a list `[]`.

So, what can we actually do with this characterization? Well for starters it implies we can pass it more than one `s` at once, which is already different than a normal lens, but we can also use all of those `s`'s alongside the result of the continuation (i.e. `b`) to choose our return value `t`. If that sounds overly general, that's because it is! Now we're cooking with Math!

To put it into slightly more concrete terms, an Algebraic lens allows us to run some *aggregation* over a collection of substates of our input, then use the result of the aggregation to pick some `t` to return.

The example given in the paper (which we'll implement soon) uses an algebraic lens to do classification of flower measurements into a given species! The "projection" function (e.g. `s -> a`) is a function which selects measurements from a `Flower`, and the "selection" function takes a **list** of Flowers, and a reference set of measurements and classifies those measurements into a species, returning a flower with the selected measurements and species.

We'll learn more about that as we implement it!

## First guesses at an implementation

We're given the prose for what the provided examples are intended to do, unfortunately we're only given a few very small code snippets without any source code or even type-signatures to help us out, so I'll be mostly guessing my way through this. As far as I can tell the paper is more concerned with proving the math first, since an implementation **must** exist if the math works out right? Let's see if we can take on the role of **applied** mathematician and get some code we can actually run 😃. I'll need to take a few creative liberties to get everything wired together. 

Here are the examples given in the abstract:

```haskell
>>> (iris !! 1) ^. measurements
(4.9 , 3.0 , 1.4 , 0.2)

>>> iris ?. measurements ( Measurements 4.8 3.1 1.5 0.1)
Iris Setosa (4.8 , 3.1 , 1.5 , 0.1)

>>> iris >- measurements . aggregateWith mean
Iris Versicolor (5.8 , 3.0 , 3.7 , 1.1)
```

We're not provided with the implementation of `?.`, `>-`, `measurements`, OR `aggregateWith`. Nor do we have the data-set that builds up `iris`... Looks like we've got our work cut out for us here...

I'm going to make some assumptions and to build up a dummy data-set of flowers to experiment with:

```haskell
-- Some flower species
data Species = Setosa | Versicolor | Virginica
  deriving Show

-- Our measurements will just be a list of floats for now
data Measurements = Measurements {getMeasurements :: [Float]}
  deriving Show

-- A flower consists of a species and some measurements
data Flower = Flower { flowerSpecies :: Species
                     , flowerMeasurements :: Measurements}
  deriving Show

versicolor :: Flower
versicolor = Flower Versicolor (Measurements [2, 3, 4, 2])

setosa :: Flower
setosa = Flower Setosa (Measurements [5, 4, 3, 2.5])

flowers :: [Flower]
flowers = [versicolor, setosa]
```

That gives us something to fool around with at least, even if it's not exactly like the data-set used in the paper.

Now for the fun part, we need to figure out how we can bake in a classification algorithm into an optic! They loosely describe `measurements` as a list-lens which "encapsulates some learning algorithm which classifies measurements into a species". 

I'll be implementing these as Profunctor optics, since that's the more "mathy" lens implementation. I stared at this for a good long while before I figured out a way to do it. One of the tough parts is that the optic has to work for a **single** flower (like the `(iris !! 1) ^. measurements` example), but must also somehow be able to run a classifier over a **group** of flowers, like the `iris ?. measurements ( Measurements 4.8 3.1 1.5 0.1)` example. We need to find the minimum number of profunctor constraints which allow us to lift the characterization into an actual runnable optic!

Here was my first crack at it:

```haskell
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Rep
import Data.Foldable

type Optic p s t a b = p a b -> p s t

listLens :: forall p f s t a b
         . (Corepresentable p, Corep p ~ f, Foldable f)
         => (s -> a)
         -> ([s] -> b -> t)
         -> Optic p s t a b
listLens project flatten p = cotabulate run
  where
    run :: f s -> t
    run fs = flatten (toList fs) (cosieve p . fmap project $ fs)
```

This is a LOT to take in, let's address it in pieces. Firstly, the `Corepresentable` constraint.

[**Corepresentable**](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Rep.html#t:Corepresentable) has [`Cosieve`](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Sieve.html#t:Cosieve) as a superclass, and so provides us with both of the following methods:

```haskell
Cosieve p f       => cosieve    :: p a b -> f a -> b
Corepresentable p => cotabulate :: (Corep p d -> c) -> p d c
```

These two functions together allow us to round-trip our profunctor from `p a b` into some `f a -> b` and then back! In fact, this is the essence of what `Corepresentable` *means*, we can "**represent**" the profunctor as a function from a value in some context `f` to the result.

These two functions allow us to reflect an opaque and mysterious generic profunctor into a real function that we can **actually run**! We can fmap `project` over the `s`'s, run them through the provided optical continuation: `f a -> b`, then flatten the whole thing using the provided *classification*-style function.

Don't worry if this doesn't make a ton of sense on its own, it took me a while to figure out. This allows us to write our `measurements` classifier, but we'll need a few helper functions first.

First we'll write a helper to compute the Euclidean distance between two flowers' measurements (e.g. we'll compute the difference between each set of measurements, then sum the difference):

```haskell
measurementDistance :: Measurements -> Measurements -> Float
measurementDistance (Measurements xs) (Measurements ys) =
    sqrt . sum $ zipWith diff xs ys
  where
    diff a b = (a - b) ** 2
```

This will tell us how similar two measurements are, the lower the result, the more similar they are!

Next we'll write a function which when given a reference set of flowers will detect the flower which is most similar to a given set of measurements. It will then build a flower by combining the closest species and the given measurements.

```haskell
classify :: Foldable f => f Flower -> Measurements -> Flower
classify flowers m =
  let Flower species _ = minimumBy 
                          (comparing (measurementDistance m . flowerMeasurements)) 
                          flowers
   in Flower species m
```

This function is partial, we should really be using a Non-Empty list, but I hope you can still somehow sleep at night 😅

Now we have our pieces, we can build the `measurements` list-lens!

```haskell
measurements :: (Corepresentable p, Corep p ~ f, Foldable f) 
             => Optic' p Flower Measurements
measurements = listLens flowerMeasurements classify
```

We specify that the container type used in the `Corepresentable` instance must be foldable so that we can convert it into a list to do our classification.

Okay! Now we have enough to try some things out! The first example given in the abstract is:

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

I'll admit I don't understand how this example could possibly work, optics necessarily have the type `p a b -> p s t`, so how are they passing a `Measurements` object directly into the optic? Perhaps it has some other signature, but we know that's not true from the previous example which uses it directly as a lens! Hrmm, I strongly suspect that this is a typo, mistake, or perhaps this example is short-hand pseudocode and isn't meant to run as-is. Maybe I missed something sneaky going on here? Without the source code for the example we'll never know, but since I can't see how this version could work, let's modify it into something *close* which I **can** figure out.

It appears as though `(?.)` is an **action** which runs the optic, which means we need to take an optic, and some arguments, then specialize the profunctor so we can run them (this is how actions for profunctor optics are defined). We know we need something that's `Corepresentable`, and the easiest instance for that is definitely `Costar`! Here's what it looks like:

```haskell
newtype Costar f a b = Costar (f a -> b)
```

`Costar` is basically the "free" Corepresentable, it's just a new-type wrapper around a function from values in a container to a result.

If we swap the arguments in the example around a bit, we can write an action which runs the optic using Costar like this:

```haskell
(?.) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
(?.) xs f a = (runCostar $ f (Costar (const a))) xs
```

Which lets us write the example like this instead:

```haskell
>>> flowers ?. measurements $ Measurements [5, 4, 3, 1]
Flower Setosa (Measurements [5.0,4.0,3.0,1.0])
```

Which is *really* close to the original, we just added a `$` to make it work.

```haskell
>>> iris ?. measurements (Measurements 4.8 3.1 1.5 0.1)
```

Let's see if this is actually working properly. We're passing a "fixed" measurement in as our aggregation function, meaning we're comparing every flower in our list to these measurements and will find the flower that's "closest" to these specific measurements. Then we return the combination of that flower species with the provided measurements. To test that this is actually working properly, let's try again with measurements that match our `versicolor` flower more closely:

```haskell
>>> setosa
Flower Setosa (Measurements [5.0,4.0,3.0,2.5])
>>> versicolor
Flower Versicolor (Measurements [2.0,3.0,4.0,2.0])

>>> flowers ?. measurements $ Measurements [1.9, 3.2, 3.8, 2]
Flower Versicolor (Measurements [1.9,3.2,3.8,2.0])
```

We can see that indeed it now switches the classification to `Versicolor`! It appears to be working!

Before moving on any further, let's see if we can fix our current setup so we can use `(^.)` properly on a single flower like the first example does. Remember, `(^.)` uses `Forget` as the profunctor instead of `Costar`, so whatever we do, it has to have a valid instance for the `Forget` profunctor which looks like this:

```haskell
newtype Forget r a b = Forget (a -> r)
```

As an exercise for the reader, try to implement `Corepresentable` for `Forget` (or even `Cosieve`) and you'll see it's not possible.

So we know that `Forget` doesn't have a valid instance of `Corepresentable`, but perhaps there's some other *weaker* abstraction we can invent which works for our purposes.

The goal here is to create an optic out of the characterization of an algebraic lens, so what if we just encode that exact idea into a typeclass?

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Profunctor p => Algebraic f p | p -> f where
  algebraic :: (s -> a) -> (f s -> b -> t) -> p a b -> p s t
```

By keeping `f` general we can write list-lenses or any other type of algebraic lens. We've added a functional dependency here to help with type-inference. This class represents **exactly** what we want an algebraic lens to do. It's entirely possible there's a more general profunctor class I'm missing, please let me know if you're aware of one!

Now that we have a typeclass we'll implement our Costar instance so we can still use our `(?.)` action:

```haskell
instance Functor f => Algebraic f (Costar f) where
  algebraic project flatten p = cotabulate run
    where
      run fs = flatten fs (cosieve (lmap project p) fs)
```

Technically this implementation works on any Corepresentable profunctor, so we could re-use this for a few other profunctors too!

Now we need to see if we can implement an instance for `Forget`, if we can manage that, then we can use `view` over our `measurements` optic!

```haskell
instance Algebraic Proxy (Forget r) where
  algebraic project _flatten (Forget f) = Forget (f . project)
```

Well that was pretty painless! This allows us to do what our `Corepresentable` requirement didn't.

With that, we just need to re-implement our `measurements` optic using `Algebraic`:

```haskell
type AlgebraicLens f s t a b = forall p. Algebraic f p => p a b -> p s t
type AlgebraicLens' f s a = AlgebraicLens f s s a a

measurements :: Foldable f => AlgebraicLens' f Flower Measurements
measurements = algebraic flowerMeasurements classify
```

The name `measurements` is a bit of a misnomer, it does classification and selection, which is quite a bit more than just selecting the measurements! Perhaps a better name would be `measurementsClassifier` or something. I'll stick to the name used in the abstract for now.

Then we can view through our `measurements` optic, fulfilling the first example perfectly!

```haskell
>>> (flowers !! 1) ^. measurements
Measurements [5.0,4.0,3.0,2.5]
```

Awesome! All that's left to have a **proper** lens is to be able to **set** as well. In profunctor optics, the set and modify actions simply use the `(->)` profunctor, so we'll need an instance for that. Technically `(->)` is `Corepresentable` so we could use the exact same implementation we used for Costar, but there's a simpler implementation if we specialize. Since a function only takes a single argument we pick `Identity` as the container type.

```haskell
instance Algebraic Identity (->) where
  algebraic project flatten p = run
    where
      run s = flatten (Identity s) (p . project $ s)
```

Now we can modify or set through our algebraic lens too:

```haskell
>>> versicolor & measurements .~ Measurements [9, 8, 7, 6]
Flower Versicolor Measurements [9.0,8.0,7.0,6.0]
```

Since we can get and set, our algebraic lens is indeed a full-blown lens! This is surprisingly interesting interesting since we didn't make any use of `Strong` which is how most lenses are implemented, and in fact `Costar` **isn't** a Strong profunctor!

Here's one last interesting instance just for fun:

```haskell
instance Algebraic Tagged where
  algebraic project flatten (Tagged b) = Tagged (flatten ([], b))
```

`Tagged` is used for the `review` actions, which means we can try running our algebraic lens as a review:

```haskell
>>> review measurements (Measurements [1, 2, 3, 4])
Flower {flowerSpecies = *** Exception: Prelude.foldl1: empty list
```

Unfortunately this doesn't work because `classify` is partial when the container is empty, we should probably add additional constraints to prevent this from happening in the future, but what it DOES mean is that we can use our algebraic lenses with `review` so long as they don't assume a non-empty container, pretty cool! I'm sure we'll find some valid uses for this as more algebraic lenses are discovered.

## Running custom aggregations

We have one more example left to look at:

```haskell
>>> iris >- measurements . aggregateWith mean
Iris Versicolor (5.8 , 3.0 , 3.7 , 1.1)
```

In this example they compute the **mean** of **each** of the respective measurements across their whole data-set, then find the species of flower which best represents the "average flower" of the data-set.

In order to implement this we'd need to implement `aggregateWith`, which is a `Kaleidoscope`. That's a whole other type of optic, so we'll continue this thread in a subsequent post, but we can get **most** of the way there with what we've got already if we write a slightly smarter aggregation function.

To spoil the next post a little, `aggregateWith` allows running aggregations over lists of *associated* measurements, that is, it **groups up** each set of related measurements across all of the flowers, then takes the mean of each **set** of measurements (i.e. the mean all the first measurements, the mean of all the second measurements, etc.). If we don't mind the inconvenience, we can implement this exact same example by baking that logic into an aggregation function.

Right now our `measurements` function **focuses** the `Measurements` of a set of flowers, the only action we have right now ignores the data-set entirely and accepts a specific measurement as input, but we can easily modify it to take a custom aggregation function:

```haskell
infixr 4 >-
(>-) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(>-) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs
```

My version of the combinator re-arranges the arguments a bit to make it read a bit more like `%~` and friends. It takes an algebraic lens on the left and an aggregation function on the right. It'll run the custom aggregation and hand off the result to the algebraic lens.

This lets us write the above example like this:

```haskell
>>> flowers & measurements >- avgMeasurement
````

But we'll need to define the `avgMeasurement` function first. It needs to take a Foldable container filled with measurements, and compute the average value for each of the four measurements. If we're clever about it, transpose will re-group all the measurements exactly how we want!

```haskell
avgMeasurement :: Foldable f => f Measurements -> Measurements
avgMeasurement ms = Measurements (mean <$> groupedMeasurements)
  where
    groupedMeasurements :: [[Float]]
    groupedMeasurements = transpose (getMeasurements <$> toList ms)
    mean :: [Float] -> Float
    mean xs = sum xs / fromIntegral (length xs)
```

We manually pair all the associated elements, then construct a new set of measurements where each value is the average of that measurement across all the inputs.

Now we can finally find out what species the average flower is closest to!

```haskell
>>> flowers & measurements >- avgMeasurement
Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

Looks like it's closest to the Versicolor species!

We can substitute `avgMeasurement` for any sort of aggregation function of type `[Measurements] -> Measurements` and this expression will run it on our data-set and return the species which is closest to those measurements. Pretty cool stuff!

## Summarizing Algebraic Lenses

The example we've looked at here is just one of many possible ways we can use Algebraic lenses! Remember that we can generalize the `f` container into almost anything!  We can use Maps, Lists, we could even use a function as the container! In addition we can use any sort of function in place of the classifier, there's no requirement that it has to return the same type as its input. It could return a summary of all items in the collection, or could select a single item from the collection based on an index returned from the classifier! Algebraic lenses allow us to compose lenses which focus on a specific portion of state, run a comparison there (e.g. get the maximm or minimum element from the collection based on some property), then zoom back out and select the larger element which contains the minimum/maximum substate!

This means we can embed operations like `minimumBy`, `findBy`, `elemIndex` and friends AS COMPOSABLE OPTICS! I'm very excited to see where this ends up going, there are a ton of possibilities which I haven't begun to think about yet.

Not to mention that algebraic lenses tend to compose better with Grate-like optics than traditional `Strong` based lenses, so they'll open up even more possibilities when combining them with other optics types.

Thanks for reading! We'll look at Kaleidoscopes next, so stay tuned!
