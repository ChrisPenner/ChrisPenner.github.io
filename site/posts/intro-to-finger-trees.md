---
title: "A Practical Introduction to Finger Trees"
author: Chris Penner
date: Jul 21, 2018
tags: [programming, haskell]
description: How to use finger trees to solve problems by choosing the appropriate monoid measure.
image: intro-to-finger-trees/finger-tree.jpg
---

Finger Trees are definitely the coolest data structure I was never taught in
school. The gist of Finger Trees is that they represent sequences of elements
where the elements also have a measurable 'descriptor' of some kind. If that
sounds vague it's because it is! The generality here is what allows Finger
Trees to solve so many different types of problems, but it does require a few
examples and explanations to understand. In this post we'll talk about how the
trees work at a high level, then we'll use them to build a random-access
array-like structure with reasonable performance characteristics.

This data structure stands on the shoulders of giants, it uses a structure called a `Monoid`
at its core.

## Monoids

If you're entirely unfamiliar with the concept of monoids, or just need a
refresher, it would be a good idea to get a solid grounding there first;
[here's a good place to
start](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour).

Monoids are incredibly useful; and the more I learn about Category Theory the
more applications I find for monoidal structures. Once you start to think in
monoids you start to realize how many things you once thought were unique and
interesting problems are actually just a monoid and a fold away from some other
well-solved problem. We're going to start off by introducing a new tool
(i.e. data structure) which employs monoids to do amazing things! Enter [Finger
Trees](https://en.wikipedia.org/wiki/Finger_tree)! Finger Trees are an
adaptable purely functional data structure; they're actually an extremely general structure which makes it a bit
difficult to explain without a concrete use case. This is because they utilize a Monoid in the foundation of the data
structure, and the Monoid you choose can drastically affect how the structure behaves. Here's a glance at the sort of
things you could do by choosing different Monoids:

- Random access/sequence slicing using [`Sum`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html#t:Sum): see [Data.Sequence](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html); we'll explore this one just below!
-  Heap using [`Max/Min`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Semigroup.html#t:Max): see [Data.PriorityQueue.FingerTree](https://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-PriorityQueue-FingerTree.html)
- Ordered Sequence slicing using [Last](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html#t:Last): see the section on [Ordered Sequences](http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf)
- Interval Searching using a custom interval expansion Monoid: see [Data.IntervalMap.FingerTree](https://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-IntervalMap-FingerTree.html)
- Text slicing and dicing using a product of `Sum`s: see [Yi.Rope](https://hackage.haskell.org/package/yi-rope)
- Performant merge sort using a custom merge monoid: blog post coming eventually!
- Many more! Just use your imagination!

How does it all work? Let's learn how to build a simple random-access
\<air-quotes\> Array \</air-quotes\> using a Finger Tree so we can get a sense of things

## Random Access Array using a Finger Trees

Let's implement a simple random access list using a Finger Tree! After a quick
glance through the [Data.FingerTree
Docs](https://hackage.haskell.org/package/fingertree-0.1.4.1/docs/Data-FingerTree.html)
it's a bit tough to tell where we might start! The workhorse of the Finger Tree library is
the `split` function:

```haskell
split :: Measured v a => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
```

Yikes, let's break this down:

-   `Measured v a`: Measured is a simple typeclass which given an `a` can
    convert it into some monoid `v`
-   `(v -> Bool)`: This is our search predicate, `split` will use it to split a
    sequence into two smaller subsequences: The longest
    subsequence such that running the predicate on the measure of this
    subsequence `False`, and the everything that's left-over.
- `FingerTree v a`: This is the tree we want to split, with a monoidal measure `v` and elements of type `a`.
- `(FingerTree v a, FingerTree v a)`: The two (possibly empty) subsequences, the first is before the split point the
    second contains the inflection point of our predicate and everything past it.

That's all great, but how can we actually use it to solve our problem? What
does splitting up a sequence actually have to do with indexing into a list?
Finger Trees get their performance characterics by searching through subtrees
using a strategy very similar to a binary search, they run the predicate on
cached "measures" of subtrees recursively honing in on the **inflection point**
where the predicate flips from `False` to `True`. So what we need to do is find
some pairing of a monoid and a predicate on that monoid which finds the place
in the sequence we're looking for. Getting the first or last element of a
Finger Tree is a simple `O(1)` operation, so if we can split the list either
directly *before* or directly *after* the index we're looking for, then we're
pretty much done! 

Building a predicate for this is pretty simple, we just need to be able to
determine whether the index we're looking for is within some prefix of our
total sequence, which phrased simply is just: `length sequence > index`; we can
use this predicate to recursively hone in on the point where adding a single
element alters the predicate's result from false to true, and we've found our
index! The predicate runs on the measure of the values, which must be a monoid;
so we need to represent the length of our sequence as some monoid, the
combination of the monoidal measure of two sequences must also match the
measure of the combination of the sequences themselves! Luckily for us the
length of the combination of two lists is just the sum of the lengths! This
gives us the hint that we can use the [`Sum`
Monoid](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html#t:Sum)
as our measure!

We're so close now, let's write some code to make it happen.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.FingerTree
import Data.Monoid

-- We need to wrap our primitive value type in a newtype;
-- This allows us to store ANY value in the sequence and helps us avoid
-- some trouble with functional dependencies and orphan instances.
newtype Size a = Size
  { getSize :: a
  } deriving (Show, Eq)

-- Measured is the typeclass we implement to tell the FingerTree how to measure
-- our values into a monoid. In our case every individual element is simply of length '1'
instance Measured (Sum Int) (Size a) where
  measure _ = Sum 1

-- We wrap our values in the 'Size'i wrapper and build a Finger Tree
alphabet :: FingerTree (Sum Int) (Size Char)
alphabet = fromList (fmap Size "abcdefghijklmnopqrstuvwxyz")

-- Get a given index from the tree if it exists
atIndex :: Int -> FingerTree (Sum Int) (Size a) -> Maybe a
atIndex n t =
  case viewl . snd $ split (> Sum n) t of
    Size c :< _ -> Just c
    _ -> Nothing
```

Hopefully the first bits are pretty self explanatory, we set up our datatypes
so the tree knows how to measure our elements, and it already knows how to
combine measures via Sum's Monoid instance. Lastly in `atIndex` we tell the
tree to split open at the point where the length of the measured subsequence
would surpass the index we've provided. Then we simply check if there's an
element to the right of that split. This operation doesn't quite get us the `O(1)`
time complexity we know and love from traditional arrays, but for an immutable, general
data structure which we could build ourselves without ANY special compiler support, getting
logarithmic performance isn't too bad. In fact the actual performance is `O(log(min(i,n-i)))` where
`i` is the index we wish to access and `n` is the length of the sequence. If we're often accessing the first or
last elements then we're down to pretty much constant time!

There we go! We've used 'Sum' as a measure within a finger tree to get
efficient indexing into a sequence! We can also notice that the length of the whole sequence
is computed in `O(1)` if we use `length = measure`; and that we can concat two sequences relatively efficiently
using `(><)`; listed in `Data.FingerTree` as time complexity `O(log(min(n1, n2)))` where n1 and n2 are the length
of each sequence  respectively.

`Sum` is probably the simplest monoid we can use; take a minute to think about how other monoids you know of might
behave; the majority of monoids will create SOME sort of useful structure when used with a Finger Tree!
