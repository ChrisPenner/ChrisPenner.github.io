---
title: "Monoidal Sorting"
author: Chris Penner
date: Jul 22, 2018
tags: [programming, haskell]
description: We explore a merge-sort monoid
image: monoid-sort/sorting.jpg
---

As I dive deeper into functional programming I'm beginning to think that
monoids can solve most problems. In general monoids work well in combination
with other structures and algorithms, for instance [Finger
Trees](/posts/intro-to-finger-trees), and folds!

This post is just yet-another-demonstration of how adaptable monoids
can really be by solving a problem most people wouldn't immediately associate
with monoids.

## Sorting

We're going to write a wrapper around lists which rather than the traditional monoid for lists (concat) instead 
sorts the two into each other; not much to talk about really, lets see the code!


```haskell
{-# LANGUAGE DeriveFunctor #-}
newtype Sort a =
    Sort {
        getSorted :: [a]
    } deriving (Functor, Show, Eq)


-- So long as the elements can be ordered we can combine two sorted lists using mergeSort
instance (Ord a) => Monoid (Sort a) where
  mempty = Sort []
  mappend (Sort a) (Sort b) = Sort $ mergeSort a b

-- simple merge sort implementation
mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort [] xs = xs
mergeSort xs [] = xs
mergeSort (x:xs) (y:ys)
  | y < x = y : mergeSort (x : xs) ys
mergeSort (x:xs) ys = x : mergeSort xs ys

-- We'll keep the 'Sort' constructor private and expose this smart constructor instead so we can
-- guarantee that every list inside a `Sort` is guaranteed to be sorted.
-- We could use a simple sort function for this, but might as well use mergeSort since 
-- we already wrote it.
toSort :: [a] -> Sort a
toSort = foldMap (Sort . pure)
```

Let's try it out:

```haskell
> toSort [1, 5, 2, 3] `mappend` toSort [10, 7, 8, 4]
Sort {getSorted = [1,2,3,4,5,7,8,10]}

> foldMap (toSort . pure) [5, 2, 3, 1, 0, 8]
Sort {getSorted = [0,1,2,3,5,8]}
```

Nothing too complicated really! `mappend` over sorted lists just sorts the combined list; we have the benefit in this
case of knowing that any list within a `Sort` is guaranteed to be sorted already so we can an efficient merge sort
algorithm. This doesn't make much difference when we're appending small sets of values together, but in particular
cases like FingerTrees where intermediate monoidal sums are cached it can really speed things up!

Just like that we've got a cool monoid which sorts lists for us, and by combining it with `foldMap` we can easily sort
the values from any foldable structure. One other benefit here is that since we know monoids are associative, if we
have a huge list of elements we need sorted we can actually split up the list, sort chunks in parallel and combine them
all and we have a guarantee that it'll all work out.

Anyways, that's about it for this one, nothing to write home about, but I think it's fun to discover new monoids!
