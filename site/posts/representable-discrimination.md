---
title: "Radix sorting with Representable Functors"
author: Chris Penner
date: Jul 23, 2017
tags: [haskell, programming]
description: Elements can be sorted into representable functors by embedding them in a monoid
image: containers.jpg
---

Looking at my recent posts it's clear I've been on a bit of a Representable kick lately; 
turns out there's a lot of cool things you can do with it! We'll be adding 'sorting' to that list
of things today. Representable Functors bring with them an intrinsic notion of sorting; not in the
traditional 'ordered' sense, but rather a sense of 'structural' sorting. Since every 'slot' in a
Representable Functor `r` can be uniquely identified by some `Rep r` we can talk about sorting
items into some named slot in `r`. If we like we can also define `Ord (Rep r)` to get a total ordering
over the slots, but it's not required.

I'll preface this post by saying I'm more interested in exploring the structural 'form' of representable
sorting than the performance of the functions we'll define, in fact the performance of some of them as they're
written here is going to be quite poor as I'm sacrificing speed to gain simplicity for the sake of pedagogy.
The intent is to observe the system from a high-level to see some interesting new patterns and shapes we gain
from using Representable to do our sorting.

I'll step through my thought process on this one:

1.  We've got a Representable Functor `r`
2.  If we have a `Rep r` for some `a` we know where to put it into an `r a`
3.  We can get a `Rep r` for every `a` by using a function `a -> Rep r`
4.  Now we want to embed the `a` into an `r a` using the `Rep r`, the tool we
    have for this is `tabulate`
5.  We know one element mapped to one slot, but we need something to put into
    all the other slots.
6.  If `a` were a Monoid we could use `mempty` for the other slots
7.  Now we have the pieces to build something like
    `(Representable r, Monoid a) => (a -> Rep r) -> [a] -> [r a]`
8.  We want a single `r a` as a result, so we need to collapse `[r a]`. We could
    use a Monoid if `r a` is a Monoid! We can actually write a monoid instance for
    any representable if the inner values are also monoids, so we can define a custom
    newtype wrapper with that instance!
    This gives `(Representable r, Monoid a) => (a -> Rep r) -> [a] -> r a`
9.  We can generalize the list to any foldable and get
    `(Representable r, Monoid a, Foldable f) => (a -> Rep r) -> f a -> r a`
10. Nifty! But this requires that every type we want is also a Monoid, we can
    increase the utility by specifying a way to build a Monoid from an `a`:
    `(Representable r, Monoid m, Foldable f) => (a -> m) -> (a -> Rep r) -> f a -> r m`


We're going to need a bunch of imports for this, prepare yourself:


```haskell
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module RepSort where

import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), Co(..), distributeRep)
import Data.Monoid (Sum(..))
import qualified Data.Stream.Infinite as S (Stream, iterate)
import Control.Comonad.Cofree (Cofree)
import qualified Data.Sequence as Seq (Seq, fromList)
```

So here's my implementation for `repSort`:

```haskell
repSort :: (Monoid m, Representable r,  Foldable f, Eq (Rep r)) => (a -> Rep r) -> (a -> m) -> f a -> r m
-- gives us a Monoid for our Representable iff the contained values are Monoids
repSort indOf toM = unMRep . foldMap (MRep . tabulate . desc)
  where
    -- desc takes an 'a' from the foldable and returns a descriptor function which can be passed to 'tabulate',
    -- The descriptor just returns mempty unless we're on the slot where the 'a's result is supposed to end up.
    desc a i
      | i == indOf a = toM a
      | otherwise = mempty


-- Here's our newtype with a Monoid over Representables
newtype MRep r a = MRep {unMRep ::r a}
  deriving (Show, Eq, Functor)

instance (Monoid a, Representable r) => Monoid (MRep r a) where
  mempty = MRep $ tabulate (const mempty)
  -- We can just tabulate a new representable where the value is the `mappend` of
  -- the other two representables. BTW `index a `mapend` index b` depends on
  -- the monoid instance for functions, so go check that out if you haven't seen it!
  (MRep a) `mappend` (MRep b) = MRep . tabulate $ index a `mappend` index b
```

Great! Let's see some examples so we can get a handle on what this does! First I'll
set up a super simple but useful Representable for Pair:

```haskell
data Pair a = Pair a a
  deriving (Show, Eq, Functor)

-- This instance is required, but we can just lean on our Representable instance
instance Distributive Pair where
  distribute = distributeRep

instance Representable Pair where
  -- Bool is a great index for this!
  type Rep Pair = Bool
  index (Pair a _) True = a
  index (Pair _ b) False = b

  tabulate desc = Pair (desc True) (desc False)
```

So since Pair is index by a Bool, the `a -> Rep Pair` is actually just a
predicate `a -> Bool`! Let's try sorting out some odd and even integers!

Remember that repSort needs a function from `a -> Rep r`, in this case Rep r ~ Bool,
so we can use `odd` to plit the odd and even integers up! Next it needs a function
which transforms an `a` into a monoid! The simplest one of these is `(:[])` which
just puts the element into a list! Let's see what we get!


```haskell
sortedInts :: Pair [Int]
sortedInts = repSort odd (:[]) [1..10]

λ> sortedInts
Pair [1,3,5,7,9] [2,4,6,8,10]
```

We can choose any monoid we like, it doesn't have to be a list! Let's say
we wanted the sums of all odd and even ints respectively between 1 and 10:

```haskell
oddEvenSums :: Pair (Sum Int)
oddEvenSums = repSort odd Sum [1..10]

λ> oddEvenSums
Pair (Sum {getSum = 25}) (Sum {getSum = 30})
```

Choosing our own monoid and index function gives us a lot of flexibility and power!

This pattern generalizes to any Representable you can think of, and most
Representables which have an interesting `Rep r` will have some cool features!

Let's try another Functor and see what happens, here we'll go with an infinite
`Stream` from `Data.Stream.Infinite`, whose representation is `Int`.

With a representation type of `Int` we could do all sorts of things! Note here
how the Functor (`Stream`) is actually infinite (although the representable is
technically bounded by the size of Int), but that's fine so long as we don't
try to get every value out of it, Haskell's inherent laziness helps us out and
we won't actually calculate the values stored in any slots where we don't look,
and since the whole thing is a data structure any computations that do occur
are automatically memoized! This also means that you don't pay the cost for
value transformation or monoidal append unless you actually look in the bucket;
only the initial `a -> Rep r` must be computed for each element.

Let's sort some stuff! See if you can figure this one out:


```haskell
byLength :: S.Stream [String]
byLength = repSort length (:[]) ["javascript", "purescript", "haskell", "python"]

λ> index byLength 10
["javascript","purescript"]
λ> index byLength 7
["haskell"]
λ> index byLength 3
[]
```

The fact that `Int` is the `Rep` for Stream provides us with a few quick wins,
any Enumerable type can be injected into Int via fromEnum:
`fromEnum: (Enum e) => e -> Int`. So we can turn any Enumerable type into an index
into Stream and we gain a whole new set of possibilities:

```haskell
byFirstChar :: S.Stream [String]
byFirstChar = repSort (fromEnum . head) (:[]) ["cats", "antelope", "crabs", "aardvarks"]

λ> index byFirstChar . fromEnum $ 'c'
["cats","crabs"]
λ> index byFirstChar . fromEnum $ 'a'
["antelope","aardvarks"]
λ> index byFirstChar . fromEnum $ 'z'
[]
```

This sort of thing is really cool and carries a lot of possibilities, but it
can get a bit unwieldy when we start dealing with indexes in the thousands.
`Stream` is effectively a linked list, so we need to traverse the whole thing
every time! We've begun to stray a bit from sorting into the idea of data
storage; let's say we wanted to store values in a structure where they're keyed
by a `String`. The first step would be to find a Representable where the Index
could be a String. Hrmmm, our `Stream` representation can index by Char, which
is close; what if we nested further representables and had the index be a
'path' to the value? Something like `Stream (Stream (Stream ...))`, We
essentially have an infinite tree of trees at this point, but we'll never reach
the end to get a value! Whenever I think of tagging tree like structure with
values I go straight to Cofree!

One way you can think of Cofree is as a Tree where every branch/node has a
value `a`, but the branching structure is determined by some Functor! For
example, a simple Rose tree is isomorphic to `Cofree [] a`, A binary tree is
isomorphic to `Cofree Pair a`, etc.

We want a tree where the branching structure is indexed by a String, so let's
give `Cofree Stream a` a try! Effectively this creates an infinite number of
branches at every level of the tree, but in practice we can only actually index
paths which are represented by some string, the rest will just be filled with
boring old `mempty`s.

As it turns out, we made a good choice! `Cofree r a` is Representable whenever `r`
is Representable, and `Rep (Cofree r a)` is `Seq r`! Under the hood the Rep for our
structure is `Seq Int`, but we can easily write `mkInd :: String -> Seq Int`!

```haskell
mkInd :: String -> Seq.Seq Int
mkInd = Seq.fromList . fmap fromEnum
```

Great! Now we can 'sort' values by strings and index into a tree structure
performantly! If this whole sort of structure is looking a bit familiar you've
probably seen it by the name of a "Trie Tree", a datastructure often used in
Radix sorts and search problems. Its nice feature is that it gives `O(m)`
lookups where `m` is the length of the key string, in our case it will be
slightly longer since we have to scan each child tree to the appropriate place
first, but you could fix that pretty easily by using a proper `Vector` as the
underlying representation rather than `Stream`. I'll leave that as an exercise
for the reader :P

That's a whole lot of explaining without any practical examples, so I bet you're
itching to try out our new Trie-based Sorter/Map! With a few helpers we can build 
something quickly!

```haskell
-- I'm going to specialize the signature to `Cofree Stream` to make it a bit more
-- readable, but you could re-generalize this idea if you wanted to.
trieSort :: (Monoid m, Foldable f) => (a -> String) -> (a -> m) -> f a -> Cofree S.Stream m
trieSort getInd = repSort (mkInd . getInd)

-- Build a map out of some key and a Monoidal value!
trieMap :: Monoid m => [(String, m)] -> Cofree S.Stream m
trieMap = trieSort fst snd

get :: Cofree S.Stream a -> String -> a
get r ind = index r (mkInd ind)

λ> get bankAccounts "Bob"
Sum {getSum = 37}
λ> get bankAccounts "Sally"
Sum {getSum = 5}
-- Empty keys are mempty
λ> get bankAccounts "Edward"
Sum {getSum = 0}
```

There are TONS of other possibilities here, we can swap out the underlying
Representable to get new behaviour and performance, we can use `Data.Functor.Compose`
multiple `Representable`s to build new and interesting structures! 
We can sort, store, lookup and search using repSort!

If you have new and interesting ideas definitely let me know! Either leave a comment
here or find me on Twitter [\@chrislpenner](https://twitter.com/chrislpenner).
