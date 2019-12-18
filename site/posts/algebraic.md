---
title: "Algebraic lenses"
author: "Chris Penner"
date: "Dec 18, 2019"
tags: [haskell]
description: "A profunctor implementation of algebraic lenses"
image: math.jpeg
---

This is a blog post about optics, if you're at all interested in optics I suggest you go check out my book: [Optics By Example](https://leanpub.com/optics-by-example). It covers everything you need to go from a beginner to master in all things optics! Check it out and tell your friends, now onwards to the post you're here for.

In this post we're going to dig into an exciting new type of optics, the theory of which is described in [this abstract](https://cs.ttu.ee/events/nwpt2019/abstracts/paper14.pdf) by Mario Román, Bryce Clarke, Derek Elkins, Jeremy Gibbons, Bartosz Milewski, Fosco Loregian, and Emily Pillmore. Thanks go out to these awesome folk for researching optics at a high level! The more that we realize the Category Theory representations of optics the more we can convince ourselves that they're a true and beautiful abstraction rather than just a useful tool we stumbled across.

I'm not really a "Mathy" sort of guy, I did very little formal math in university, and while I've become comfortable in some of the absolute basics of Category Theory through my travels in Haskell, I certainly wouldn't consider myself well-versed. I AM however well versed in the practical uses of optics, and so of course I need to keep myself up to speed on new developments, so when this abstract became available I set to work trying to understand it!

Most of the symbols and Category Theory went straight over my head, but I managed to pick out a few bits and pieces that we'll look at today. I'll be translating what little I understand into a language which I DO understand: Haskell!

If the above wasn't enough of a disclaimer I'll repeat: I don't really understand most of the math behind this stuff, so it's very possible I've made a few (or a lot) of errors though to be honest I think the result I've come to is interesting on its own, even if not a perfect representation of the ideas in the abstract. Please correct me if you know better :)

There are several new types of optics presented in the paper, we'll start by looking at one of them in particular, but will set the groundwork for the others which I'll hopefully get to in future posts. Today we'll be looking at "Algebraic lenses"!

## Translating from Math

We'll start by taking a look at the formal characterization of algebraic lenses presented in the abstract. By the characterization of an optic I mean a set of values which completely describe the behaviour of that optic. For instance a `Lens s t a b` is characterized by a getter and a setter: `(s -> a, s -> b -> t)` and an `Iso s t a b` is characterized by its `to` and `from` functions: `(s -> a, b -> t)`.

The paper presents the characterization of an algebraic lens like this: (my apologies for lack of proper LaTeX on my blog 😬)

* Algebraic Lens: `(S → A) × (ψS × B → T)`

My blog has kind of butchered the formatting, so feel free to check it out in the [abstract](http://events.cs.bham.ac.uk/syco/strings3-syco5/slides/roman.pdf) instead.

I'm not hip to all these crazy symbols, but as best as I can tell, we can translate it roughly like this:

* Algebraic Lens: `(s -> a, f s -> b -> t)`

If you squint a bit, this looks really close to the characterization of a standard lens, the only difference being that instead of a *single* `s` we have some container `f` filled with them. The type of container further specifies what type of algebraic lens we're dealing with. For instance, the paper calls it a `List Lens` if `f` is chosen to be a list `[]`, but we can really define optics for nearly any choice of `f`, though Traversable and Foldable types are a safe bet to start.

So, what can we actually do with this characterization? Well for starters it implies we can pass it more than one `s` at once, which is already different than a normal lens, but we can also use all of those `s`'s alongside the result of the continuation (i.e. `b`) to choose our return value `t`. That probably sounds pretty overly generalized, and that's because it is! We're dealing with a mathematical definition, so it's intentionally as general as possible.

To put it into slightly more concrete terms, an Algebraic lens allows us to run some *aggregation* over a collection of substates of our input, then use the result of the aggregation to pick some result to return.

The example given in the paper (which we'll implement soon) uses an algebraic lens to do classification of flower measurements into particular species. It uses the "projection" function from the characterization (e.g. `s -> a`) to select the measurements from a `Flower`, and the "selection" function (`f s -> b -> t`) to take a **list** of Flowers, and a reference set of measurements, to classify those measurements into a species, returning a flower with the selected measurements and species.

We'll learn more about that as we implement it!

## First guesses at an implementation

In the abstract we're given the *prose* for what the provided examples are intended to do, unfortunately we're only given a few very small code snippets without any source code or even type-signatures to help us out, so I'll mostly be guessing my way through this. As far as I can tell the paper is more concerned with proving the math first, since an implementation **must** exist if the math works out right? Let's see if we can take on the role of **applied** mathematician and get some code we can actually run 😃. I'll need to take a few creative liberties to get everything wired together.

Here are the examples given in the abstract:

```haskell
-- Assume 'iris' is a data-set (e.g. list) of flower objects
>>> (iris !! 1) ^. measurements
(4.9 , 3.0 , 1.4 , 0.2)

>>> iris ?. measurements ( Measurements 4.8 3.1 1.5 0.1)
Iris Setosa (4.8 , 3.1 , 1.5 , 0.1)

>>> iris >- measurements . aggregateWith mean
Iris Versicolor (5.8, 3.0, 3.7, 1.1)
```

We're not provided with the implementation of `?.`, `>-`, `Measurements`, `measurements`, OR `aggregateWith`, nor do we have the data-set that builds up `iris`... Looks like we've got our work cut out for us here 😓

To start I'll make some assumptions to build up a dummy data-set of flowers to experiment with:

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

Now for the fun part, we need to figure out how we can somehow cram a classification algorithm into an optic! They loosely describe `measurements` as a list-lens which "encapsulates some learning algorithm which classifies measurements into a species", but the concrete programmatic definition of that will be up to my best judgement I suppose.

I'll be implementing these as Profunctor optics, they tend to work out a bit **cleaner** than the Van-Laarhoven approach, especially when working with "Grate-Like" optics which is where an algebraic-lens belongs. The sheer amount of guessing and filling in blanks I had to do means I stared at this for a good long while before I figured out a way to make this work. One of the tough parts is that the examples show the optic work for a **single** flower (like the `(iris !! 1) ^. measurements` example), but it somehow also runs a classifier over a **list** of flowers as in the `iris ?. measurements ( Measurements 4.8 3.1 1.5 0.1)` example. We need to find the minimal profunctor constraints which allow us to lift the characterization into an actual runnable optic!

I've been on a bit of a Corepresentable kick lately and it seemed like a good enough place to start. It also has the benefit of being easily translated into Van-Laarhoven optics if needed.

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

This is a LOT to take in, let's address it in pieces. 

First things first, a profunctor optic is simply a morphism over a profunctor, something like: `p a b -> p s t`.

Next, the `Corepresentable` constraint:

[**Corepresentable**](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Rep.html#t:Corepresentable) has [`Cosieve`](https://hackage.haskell.org/package/profunctors-5.5.1/docs/Data-Profunctor-Sieve.html#t:Cosieve) as a superclass, and so provides us with both of the following methods:

```haskell
Cosieve p f       => cosieve    :: p a b -> f a -> b
Corepresentable p => cotabulate :: (Corep p d -> c) -> p d c
```

These two functions together allow us to round-trip our profunctor from `p a b` into some `f a -> b` and then back! In fact, this is the essence of what `Corepresentable` *means*, we can "**represent**" the profunctor as a function from a value in some context `f` to the result.

Profunctors in general **can't simply be applied** like functions can, these two functions allow us to reflect an opaque and mysterious generic profunctor into a real function that we can **actually run**! In our implementation we fmap `project` over the `f s`'s to get `f a`, then run that through the provided continuation: `f a -> b` which we obtain by running `cosieve` on the profunctor argument, then we can flatten the whole thing using the user-provided *classification*-style function.

Don't worry if this doesn't make a ton of sense on its own, it took me a while to figure out. At the end of the day, we have a helper which allows us to write a list-lens which composes with any `Corepresentable` profunctor. This allows us to write our `measurements` classifier, but we'll need a few helper functions first.

First we'll write a helper to compute the Euclidean distance between two flowers' measurements (e.g. we'll compute the difference between each set of measurements, then sum the difference):

```haskell
measurementDistance :: Measurements -> Measurements -> Float
measurementDistance (Measurements xs) (Measurements ys) =
    sqrt . sum $ zipWith diff xs ys
  where
    diff a b = (a - b) ** 2
```

This will tell us how similar two measurements are, the lower the result, the more similar they are.

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

By the way, all the examples in this post are implemented using my **highly experimental** Haskell profunctor optics implementation [**proton**](https://github.com/ChrisPenner/proton). Feel free to play with it, but don't use it in anything important.

Hrmm, looks like `(^.)` uses `Forget` for its profunctor and it doesn't have a `Corepresentable` instance! We'll come back to that soon, let's see if we can get anything else working first. 

The next example is:

```haskell
iris ?. measurements (Measurements 4.8 3.1 1.5 0.1)
```

I'll admit I don't understand how this example could possibly work, optics necessarily have the type `p a b -> p s t`, so how are they passing a `Measurements` object directly into the optic? Perhaps it has some other signature, but we know that's not true from the previous example which uses it directly as a lens! Hrmm, I strongly suspect that this is a typo, mistake, or most likely this example is actually just short-hand pseudocode of what an implementation *might* look like and we're discovering a few rough edges. Perhaps the writers of the paper thought of something sneaky that I missed. Without the source code for the example we'll never know, but since I can't see how this version could work, let's modify it into something *close* which I **can** figure out.

It appears as though `(?.)` is an **action** which runs the optic. Actions in profunctor optics tend to specialize the optic to a specific profunctor, then pass the other arguments through it using that profunctor as a carrier. We know we need a profunctor that's `Corepresentable`, and the simplest instance for that is definitely `Costar`! Here's what it looks like:

```haskell
newtype Costar f a b = Costar (f a -> b)
```

`Costar` is basically the "free" Corepresentable, it's just a new-type wrapper around a function from values in a container to a result. You might also know it by the name `Cokleisli`, they're the same type, but `Costar` is the one we typically use with Profunctors.

If we swap the arguments in the example around a bit, we can write an action which runs the optic using Costar like this:

```haskell
(?.) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
(?.) xs f a = (runCostar $ f (Costar (const a))) xs
```

The example seems to use a static value for the comparison, so I use `const` to embed that value into the `Costar` profunctor, then run that through the provided profunctor morphism (i.e. optic).

This lets us write the example like this instead:

```haskell
>>> flowers ?. measurements $ Measurements [5, 4, 3, 1]
Flower Setosa (Measurements [5.0,4.0,3.0,1.0])
```

Which is *really* close to the original, we just added a `$` to make it work.

```haskell
>>> iris ?. measurements (Measurements 4.8 3.1 1.5 0.1)
```

Let's see if this is actually working properly. We're passing a "fixed" measurement in as our aggregation function, meaning we're comparing every flower in our list to these specific measurements and will find the flower that's "closest". We then build a flower using the species closest to those measurements alongside the provided measurements. To test that this is actually working properly, let's try again with measurements that match our `versicolor` flower more closely:

```haskell
>>> setosa
Flower Setosa (Measurements [5.0,4.0,3.0,2.5])
>>> versicolor
Flower Versicolor (Measurements [2.0,3.0,4.0,2.0])

-- By choosing measurements close to the `versicolor` in our data-set
-- we expect the measurements to be classified as Versicolor
>>> flowers ?. measurements $ Measurements [1.9, 3.2, 3.8, 2]
Flower Versicolor (Measurements [1.9,3.2,3.8,2.0])
```

We can see that indeed it now switches the classification to `Versicolor`! It appears to be working!

Even though this version looks a lot like the example in the abstract, it doesn't quite feel in line with style of existing optics libraries so I'll flip the arguments around a bit further: (I'll rename the combinator to `?-` to avoid confusion with the original)

```haskell
(?-) :: (Foldable f) => f s -> Optic (Costar f) s t a b -> b -> t
(?-) xs f a = (runCostar $ f (Costar (const a))) xs
```

The behaviour is the same, but flipping the arguments allows it to fit the "feel" of other optics combinators better (IMHO), we use it like this:

```haskell
>>> flowers & measurements ?- Measurements [5, 4, 3, 1]
Flower Setosa (Measurements [5.0,4.0,3.0,2.5])
```

We pass in the data-set, and "assign" our comparison value to be the single Measurement we're considering.

## Making `measurements` a proper lens

Before moving on any further, let's see if we can fix up `measurements` so we can use `(^.)` on a single flower like the first example does. Remember, `(^.)` uses `Forget` as the concrete profunctor instead of `Costar`, so whatever we do, it has to have a valid instance for the `Forget` profunctor which looks like this:

```haskell
newtype Forget r a b = Forget (a -> r)
```

As an exercise for the reader, try to implement `Corepresentable` for `Forget` (or even `Cosieve`) and you'll see it's not possible, so we'll need to find a new tactic. Perhaps there's some other *weaker* abstraction we can invent which works for our purposes.

The end-goal here is to create an optic out of the characterization of an algebraic lens, so what if we just encode that exact idea into a typeclass? It's so simple it just might work! Probably should have started here, sticking with the optics metaphor: hindsight is 20/20.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Profunctor p => Algebraic f p | p -> f where
  algebraic :: (s -> a) -> (f s -> b -> t) -> p a b -> p s t

type AlgebraicLens f s t a b = forall p. Algebraic f p => p a b -> p s t
type AlgebraicLens' f s a = AlgebraicLens f s s a a
```

By keeping `f` general we can write list-lenses or any other type of algebraic lens. I added a functional dependency here to help with type-inference. This class represents **exactly** what we want an algebraic lens to do. It's entirely possible there's a more general profunctor class which has equivalent power, if I'm missing one please let me know!

Now that we have a typeclass we'll implement an instance for Costar so we can still use our `(?.)` and `(?-)` actions:

```haskell
instance Functor f => Algebraic f (Costar f) where
  algebraic project flatten p = cotabulate run
    where
      run fs = flatten fs (cosieve (lmap project p) fs)
```

Technically this implementation works on **any** Corepresentable profunctor, not just Costar, so we could re-use this for a few other profunctors too!

Did we make any progress? We need to see if we can implement an instance of `Algebraic` for `Forget`, if we can manage that, then we can use `view` over our `measurements` optic just like the example does.

```haskell
instance Algebraic Proxy (Forget r) where
  algebraic project _flatten (Forget f) = Forget (f . project)
```

Well that was pretty painless! This allows us to do what our `Corepresentable` requirement didn't.

I've arbitrarily chosen `Proxy` as the carrier type because it's empty and doesn't contain any values. The carrier itself isn't every used, but I needed to pick something and this seemed like a good a choice as any. Perhaps a higher-rank void type would be more appropriate, but we'll cross that bridge when we have to.

With that, we just need to re-implement our `measurements` optic using `Algebraic`:

```haskell
measurements :: Foldable f => AlgebraicLens' f Flower Measurements
measurements = algebraic flowerMeasurements classify
```

The name `measurements` is a bit of a misnomer, it does classification and selection, which is quite a bit more than just selecting the measurements! Perhaps a better name would be `measurementsClassifier` or something. I'll stick to the name used in the abstract for now.

Now we can view through our `measurements` optic directly! This fulfills the first example perfectly!

```haskell
>>> (flowers !! 1) ^. measurements
Measurements [5.0,4.0,3.0,2.5]
```

Awesome! All that's left to have a **proper** lens is to be able to **set** as well. In profunctor optics, the set and modify actions simply use the `(->)` profunctor, so we'll need an instance for that. Technically `(->)` is isomorphic to `Costar Identity`, so we could use the exact same implementation we used for our `Costar` instance but there's a simpler implementation if we specialize. It turns out that `Identity` makes a good carrier type since it holds exactly one argument.

```haskell
instance Algebraic Identity (->) where
  algebraic project flatten p = run
    where
      run s = flatten (Identity s) (p . project $ s)
```

Now we can modify or set measurements through our algebraic lens too:

```haskell
>>> versicolor & measurements .~ Measurements [9, 8, 7, 6]
Flower Versicolor Measurements [9.0,8.0,7.0,6.0]
```

Since we can get and set, our algebraic lens is indeed a full-blown lens! This is surprisingly interesting interesting since we didn't make any use of `Strong` which is how most lenses are implemented, and in fact `Costar` **isn't** a Strong profunctor!

You might be curious how this actually works at all, behind the scenes the algebraic lens receives the new measurements as though it were the result of an aggregation, then uses those measurements with the Species of the single input flower (which of course hasn't changed), thus appearing to modify the flower's measurements! It's the "long way round" but it behaves exactly the same as a simpler lens would.

Here's one last interesting instance just for fun:

```haskell
instance Algebraic Proxy Tagged where
  algebraic project flatten (Tagged b) = Tagged (flatten Proxy b)
```

`Tagged` is used for the `review` actions, which means we can try running our algebraic lens as a review:

```haskell
>>> review measurements (Measurements [1, 2,2, 3])
Flower {flowerSpecies = *** Exception: foldl1: Proxy
```

Unfortunately this doesn't work because `classify` is partial when the container is empty (and proxy is empty), we should probably add additional constraints to our `classify` function so it's not partial. A Non-Empty constraint on the container would prevent us from using review when we shouldn't. In general though, this means we can use most algebraic lenses with `review` so long as they're not partial, pretty cool! I'm sure we'll find some valid uses for this as more algebraic lenses are discovered.

## Running custom aggregations

We have one more example left to look at:

```haskell
>>> iris >- measurements . aggregateWith mean
Iris Versicolor (5.8 , 3.0 , 3.7 , 1.1)
```

In this example they compute the **mean** of **each** of the respective measurements across their whole data-set, then find the species of flower which best represents the "average flower" of the data-set.

In order to implement this we'd need to implement `aggregateWith`, which is a `Kaleidoscope`, and that's a whole different type of optic, so we'll continue this thread in a subsequent post but we can get **most** of the way there with what we've got already if we write a slightly smarter aggregation function.

To spoil kaleidoscopes just a little, `aggregateWith` allows running aggregations over lists of *associated* measurements. That is to say that it **groups up** each set of related measurements across all of the flowers, then takes the mean of each **set** of measurements (i.e. the mean all the first measurements, the mean of all the second measurements, etc.). If we don't mind the inconvenience, we can implement this exact same example by baking that logic into an aggregation function and thus avoid the need for a Kaleidoscope until the next blog post 😉

Right now our `measurements` function **focuses** the `Measurements` of a set of flowers, the only action we have right now ignores the data-set entirely and accepts a specific measurement as input, but we can easily modify it to take a custom aggregation function:

```haskell
infixr 4 >-
(>-) :: Optic (Costar f) s t a b -> (f a -> b) -> f s -> t
(>-) opt aggregator xs = (runCostar $ opt (Costar aggregator)) xs
```

My version of the combinator re-arranges the arguments a bit (again) to make it read a bit more like `%~` and friends. It takes an algebraic lens on the left and an aggregation function on the right. It'll run the custom aggregation and hand off the result to the algebraic lens.

This lets us write the above example like this:

```haskell
>>> flowers & measurements >- avgMeasurement
````

But we'll need to define the `avgMeasurement` function first. It needs to take a Foldable container filled with measurements and compute the average value for each of the four measurements. If we're clever about it `transpose` can re-group all the measurements exactly how we want!

```haskell
mean :: Fractional a => [a] -> a
mean [] =  0
mean xs = sum xs / fromIntegral (length xs)

avgMeasurement :: Foldable f => f Measurements -> Measurements
avgMeasurement ms = Measurements (mean <$> groupedMeasurements)
  where
    groupedMeasurements :: [[Float]]
    groupedMeasurements = transpose (getMeasurements <$> toList ms)
```

We manually pair all the associated elements, then construct a new set of measurements where each value is the average of that measurement across all the inputs.

Now we can finally find out what species the *average flower* is closest to!


```haskell
>>> flowers & measurements >- avgMeasurement
Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

Looks like it's closest to the Versicolor species!

We can substitute `avgMeasurement` for any sort of aggregation function of type `[Measurements] -> Measurements` and this expression will run it on our data-set and return the species which is closest to those measurements. Pretty cool stuff!

## Custom container types

We've stuck with a list so far since it's easy to think about, but algebraic lenses work over any container type so long as you can implement the aggregation functions you want on them. In this case we only require Foldable for our classifier, so we can hot-swap our list for a Map without any changes!

```haskell
>>> M.fromList [(1.2, setosa), (0.6, versicolor)] 
      & measurements >- avgMeasurement
Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

This gives us the same answer of course since the foldable instance simply ignores the keys, but the container type is carried through any composition of algebraic lenses! That means our aggregation function now has type: `Map Float Measurements -> Measurements`, see how it still projects from `Flower` into `Measurements` even inside the map? Let's say we want to run a scaling factor over each of our measurements as part of aggregating them, we can bake it into the aggregation like this:

```haskell
scaleBy :: Float -> Measurements -> Measurements
scaleBy w (Measurements m) = Measurements (fmap (*w) m)

>>> M.fromList [(1.2, setosa), (0.6, versicolor)] 
      & measurements >- avgMeasurement . fmap (uncurry scaleBy) . M.toList
Flower Versicolor (Measurements [3.5,3.5,3.5,2.25])
```

Running the aggregation with these scaling factors changed our result and shows us what the average flower would be if we scaled each flower by the amount provided in the input map.

This isn't a perfect example of what other containers could be used for, but I'm sure folks will be dreaming up clever ideas in no time!

## Other aggregation types

Just as we can customize the container type and the aggregation function we pass in we can also build algebraic lenses from any manor of custom "classification" we want to perform. Let's write a new list-lens which partitions the input values based on the result of the aggregation. In essence classifying each point in our data-set as above or below the result of the aggregation.

```haskell
partitioned :: forall f a. (Ord a, Foldable f) => AlgebraicLens f a ([a], [a]) a a
partitioned = algebraic id splitter
  where
    splitter :: f a -> a -> ([a], [a])
    splitter xs ref
      = (filter (< ref) (toList xs), filter (>= ref) (toList xs))
```

It's completely fine for our `s` and `t` to be completely disparate types like this.

This allows us to split a container of values into those which are less than the aggregation, or greater/equal to it. We can use it with a static value like this:

```haskell
>>> [1..10] & partitioned ?- 5
([1,2,3,4],[5,6,7,8,9,10])
```

Or we can provide our own aggregation function; let's say we want to split it into values which are less than or greater than the **mean** of the data-set. We'll use our modified version of `>-` for this:

```haskell
>>> mean [3, -2, 4, 1, 1.3]
1.46

>>> [3, -2, 4, 1, 1.3] & partitioned >- mean
([-2.0,1.0,1.3], [3.0,4.0])
```

Here's a list-lens which generalizes the idea behind `minimumBy`, `maximumBy`, etc. into an optic. We allow the user to provide a selection function for indicating the element they want, then the optic itself will pluck the appropriate element out of the collection.

```haskell
-- Run an aggregation on the first elements of the tuples
-- Select the second tuple element which is paired with the value
-- equal to the aggregation result.
onFirst :: (Foldable f, Eq a) => AlgebraicLens f (a, b) (Maybe b) a a
onFirst = algebraic fst picker
  where
    picker xs a = lookup a $ toList xs

-- Get the character paired with the smallest number
>>> [(3, 'a'), (10, 'b'), (2, 'c')] & onFirst >- minimum
Just 'c'

-- Get the character paired with the largest number
>>> [(3, 'a'), (10, 'b'), (2, 'c')] & onFirst >- maximum
Just 'b'

-- Get the character paired with the first even number
>>> [(3, 'a'), (10, 'b'), (2, 'c')] & onFirst >- head . filter even
Just 'b'
```

If our structure is indexable we can do this much more generally and build a library of composable optics which dig deeply into structures and perform selection aggregations over anything we want. It may take a little work to figure out the cleanest set of combinators, but here's a simplified example of just how easy it is to start messing around with:

```haskell
-- Pick some substate or projection from each value,
-- The aggregation selects the index of one of these projections and returns it
-- Return the 'original state' that lives at the chosen index
selectingOn :: (s -> a) -> AlgebraicLens [] s (Maybe s) a (Maybe Int)
selectingOn project = algebraic project picker
  where
    picker xs i = (xs !!) <$> i

-- Use the `Eq` class and return the index of the aggregation result in the original list
indexOf :: Eq s => AlgebraicLens [] s (Maybe Int) s s
indexOf = algebraic id (flip elemIndex)

-- Project each string into its length, 
-- then select the index of the string with length 11,
-- Then find and return the element at that index
>>> ["banana", "pomegranate", "watermelon"] 
      & selectingOn length . indexOf ?- 11
Just "pomegranate"

-- We can can still use a custom aggregation function,
-- This gets the string of the shortest length. 
-- Note we didn't need to change our chain of optics at all!
>>> ["banana", "pomegranate", "watermelon"] 
      & selectingOn length . indexOf >- minimum
Just "banana"
```

I'm sure you can already imagine all sorts of different applications for this sort of thing. It may seem more awkward than the straight-forward Haskell way of doing these things, but it's a brand new idea, it'll take time for the ecosystem to grow around it and for us to figure out the "best way".

## Summarizing Algebraic Lenses

The examples we've looked at here are just a few of many possible ways we can use Algebraic lenses! Remember that we can generalize the `f` container into almost anything!  We can use Maps, Lists, we could even use a function as the container! In addition we can use any sort of function in place of the classifier, there's no requirement that it has to return the same type as its input. Algebraic lenses allow us to compose lenses which focus on a specific portion of state, run a comparison or aggregation there (e.g. get the maximum or minimum element from the collection based on some property), then zoom back out and select the larger element which contains the minimum/maximum substate!

This means we can embed operations like `minimumBy`, `findBy`, `elemIndex` and friends as composable optics! There are many other interesting aggregations to be found in statistics, linear algebra, and normal day-to-day tasks. I'm very excited to see where this ends up going, there are a ton of possibilities which I haven't begun to think about yet.

Algebraic lenses also tend to compose better with Grate-like optics than traditional `Strong Profunctor` based lenses, they work well with getters and folds, and can be used with setters or traversals for setting or traversing (but not aggregating). They play a role in the ecosystem and are just one puzzle piece in the world of optics we're still discovering.

Thanks for reading! We'll dig into Kaleidoscopes soon, so stay tuned!
