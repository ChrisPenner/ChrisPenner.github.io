---
title: 'Recursive Zippers'
author: "Chris Penner"
date: "Nov 20, 2021"
tags: [haskell]
description: "Building a zipper over a recursive structure"
image: gadts.jpg
---

I recently built the [Jet](https://github.com/ChrisPenner/jet#readme) structural JSON editor.
I built a [text editor](https://github.com/ChrisPenner/rasa#readme) in the past, so I knew which
performance issues were likely to come up. In both of these editors we need to be able to edit our underlying data
efficiently, but also need to be able to _render_ portions of our structure to screen efficiently.

To better understand the complications that a structural editor introduces, let's first see how these tasks are typically handled in a standard non-structural text editor.

## Standard Text Editors

Text is pretty easy to split up and view in chunks, and usually text editors only need to display about fifty lines at a time. 
If only lines 72-138 are visible on screen, we just need to locate that range, then simply dump that text out on screen printing it one character at a time. 
Admittedly there are some complications when we start to consider wrapping lines which are too long for the screen, but the core concept is relatively simple.

Editing text is a little trickier. Text is often stored as a large array of bytes,
you might think we can just edit those bytes in place, but that idea breaks down
as soon as you add or delete characters! Let's not forget that in the modern era we definitely can't assume all text is
limited to ASCII. A single "character" encoded in UTF-8 may be anywhere between 1 and 4 bytes.
So how can we edit or delete characters in a file?
Copying the whole array of bytes every time we type a character is far too expensive, so we'll need to find a compromise which addresses our needs.
Luckily all of these problems have already been solved in the world of text editors.

Introducing the clever [Rope data structure](https://en.wikipedia.org/wiki/Rope_(data_structure). 
This structure is carefully designed to allow efficient random-access edits by grouping text into small chunks which are linked together to form a larger structure. It's much more reasonable to copy and update these smaller chunks.
Its design permits efficient renders by including an index of our document by its newlines.
This means we can quickly grab and render only lines 72-138 if needed, or we can easily find and delete a single character on line 47 without needing to search or copy our entire document.

I won't dig too much deeper on Ropes here, it should suffice to say that they're worth checking out.

## Complicating things

In contrast with standard text editors, most structural editors operate over some form 
of _self recursive_ data structure. Think of things like HTML, JSON, or the AST 
of your favourite programming language. Each of these structures is comprised of 
_nodes_, and each node contains some data and maybe some child nodes of the same type.

These self-recursive structures unfortunately are not as easily sliced and diced. 
These structures don't have a (nearly) 1-to-1 direct translation to their on-screen representation in the same
way that text does. These documents are nested arbitrarily, a given node 
might take 2 lines to render, or maybe it'll take 500 lines once all is said and done! 
We often don't know how much screen-space a node might take until we've rendered it. 
This makes it very difficult to know which nodes in the document correspond to which _lines_ in the text editor.
If I scan to line 300 in my editor, which node in our JSON object do we need to render?
In general, we just can't be sure without rendering the entire document up until that point.

So, rendering is one tough issue to solve, our other problem is editing.
In languages with mutable data structures you'd likely grab a pointer to a node
and perform any updates in-place. You still need to find ways to efficiently re-render the structure,
and you'll need to find efficient ways to _navigate_ the nodes of your structure, but
editing would be relatively cheap.

However, In a functional language with immutable data structures we can't simply update the structure in place!
If I want to edit the value stored at `users[0].info.address.street_address.postal_code` in my Haskell representation of a JSON document,
I'll need to first crawl my way down from the root of the document
by following the path to that key, then I perform my update, and finally need to rebuild the entire structure around
the changed value. Not only does this take a lot of steps, it also means allocating new nodes in memory for **every** node which is a parent of my change.
This is the same reason that using `++` to append to the _end_ of a linked list in Haskell is quite expensive,
doing so requires traversing the entirety of the list, and allocating an entirely new spine.

If we have to re-allocate the entire multi-megabyte JSON document every time the user types a key, 
the editor will be completely non-responsive. It's a no-go.

What we'd love to do is restructure our document tree such that the node we're editing is at the very root.
Considering our linked-list analogy; it's very quick and easy to peel off the head of our list, make some changes, and 
attach the changed result to the tail of the list. 
In this example the entire tail of the list is unaltered and doesn't need to be re-allocated. The element we want
to change is readily available. 
This works when we access the head of the list, but what if we want to make edits to elements in the _middle_ of a list?
Is there a way we can reap these benefits for any location in a structure?

## Zippers

There's a well known structure called a **list zipper** which encodes this idea specifically for linked lists.

It typically looks something like this:

```haskell
data ListZipper a = ListZipper 
  { parents :: [a]
  , focus :: a 
  , tail :: [a]
  }
```


This structure allows us to traverse a list step by step and collects the parents of our current position as we go along.
On one side we have a list of our parents, on the other a list of all of our children, and in the middle we've got easy access to the node we're focusing on.

Unfortunately, most recursive documents are symmetric in the same way that a list is,
what does it mean to turn a JSON document inside out or upside down?
Can we make a zipper over a recursive document? Even better, can we do this generically for all recursive documents?

A lot of very smart people have spent a long time thinking about this, and have devised some very clever solutions. 
Unfortunately, these approaches are generally pretty complex and the libraries they yield, although very general, are also
extremely difficult to use. See the [zippers](https://hackage.haskell.org/package/zippers) library for an example of how complex the type-signatures can become.

My goal is more specific, I want a structure which provides an efficient way
to edit a self-recursive structure, and also helps me efficiently render the 
structure as I edit it!

## A Zipper for JSON

Let's start by looking at what a zipper designed specifically for JSON looks like,
then we can generalize it so it works for other domains.

```haskell
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data JSONZipper = JSONZipper
  { parents :: [Either (Text, Object) (Int, Array)],
    focused :: Value
  }
```

Here's my basic data structure with all the imports we'll need as we go along.

In this case we keep track of a single **focused** node of the JSON structure,
and keep all parents in a list. Each item in the `parents` list also stores
the _index_ within that parent which lead to the currently focused node.

In JSON we know that every parent **must** be either an _array_ or an _object_. There
are other JSON types like numbers and strings of course, but these don't contain
any child nodes, and thus couldn't ever be parents of a focused node.

Now that we've got a basic structure to work with we can start to add some helpful combinators:

```haskell
newZipper :: Value -> JSONZipper
newZipper value = JSONZipper [] value

down :: Either Text Int -> JSONZipper -> Maybe JSONZipper
down key (JSONZipper parents current) =
  case (key, current) of
    (Left txt, Object hm) -> JSONZipper (Left (txt, hm) : parents) <$> HM.lookup txt hm
    (Right i, Array arr) -> JSONZipper (Right (i, arr) : parents) <$> arr Vector.!? i
    _ -> Nothing
```

Here's our first helper, it allows us to descend into the structure by one step.
The `down` combinator can fail for a number of reasons. We return `Nothing` if the index to descend into
doesn't align with the kind of JSON value which is at the currently focused node, or if there's no value 
at that index to select.

Going `down` into the structure stores the previous focus as a parent.
We can also re-zip our structure by going back upwards.

```haskell
up :: JSONZipper -> Maybe JSONZipper
up (JSONZipper parents current) =
  case parents of
    [] -> Nothing
    (Left (key, hm) : ancestors) -> Just $ JSONZipper ancestors (Object $ HM.insert key current hm)
    (Right (i, arr) : ancestors) -> Just $ JSONZipper ancestors (Array $ arr Vector.// [(i, current)])
```

This combinator fails if we don't have any parents to re-zip.
If we do have a parent, we've already stored which position to insert our current node back into so it's a simple
matter of updating the parent node's child at the index where the focus was located.

The astute among you will notice that when we're focusing on a child node, the _old_ child node is still
located inside the parent node. Conceptually this is a little messy, but in practice it doesn't cause any
issues since that node will always be overwritten when we rezip our structure. At worst we're keeping
a bit of extra memory around, but due to the way Haskell shares duplicate structures even this is usually not
a major issue. There are many ways to resolve this if you care about the additional memory so I'll leave that as an exercise
since not all of them generalize well to arbitrary recursive structures.

That's the crux of it! See if you can implement a `sibling` combinator, or any other
helpers you might want.

## Generalizing our Zipper

Great, so we've got a working zipper for JSON, but we've still got to generalize
it to work with any recursive type and we need to solve our rendering performance problems.

First let's talk quickly about how we can _generalize recursion_.
Maybe you've heard of recursion schemes?
If you're not familiar with the concept don't worry, we'll make our way there gradually.
The first step towards using recursion schemes is to factor the recursion out of our type by defining a new type called a **base functor**.
The idea is that if we want to talk about recursion generically we need to be able to interact with 
the recursive bits of our structure in a well-defined way, and in Haskell we love our Functors and Traversables!

Let's see what this process looks like for a well-known type.
Here's a representation of a simple recursive type, the list:

```haskell
data List a =
    Cons a (List a)
  | Nil
```

Notice how we have a `List a` inside the list definition, we use the type in its own definition, that's recursion.
Here's what we get if we replace every recursive slot with the new type parameter `r`, thus
forming our base-functor for the list type:

```haskell
data ListF a r =
    ConsF a r
  | NilF
  deriving (Functor, Foldable, Traversable)
```

Notice how the `ListF` type is no longer self-recursive, the recursion has been factored out
and replaced with the `r` parameter (for recursive).

I've derived Functor, Foldable, and Traversable instances as well, but note that unlike
the instances defined on a regular list, these instances pertain to the `r` parameter instead of the elements of the list `a`.

By factoring out the `r` parameter we're able to choose any representation we like
for the **spine** of our recursive structure.

As a first step we can reclaim the original type by choosing our spine to be an infinite 
stack of the base functor nesting within itself.
We need a wrapper type in order to represent this in Haskell's type system,
here's the `Fix` type which performs the infinite nesting trick:

```haskell
newtype Fix f = Fix (f (Fix f))

myList :: Fix ListF
myList = Fix (ConsF 1 (Fix (ConsF 2 (Fix NilF))))
```

This adds a ton of noise, so we rarely use these structures in application code, 
but they're really helpful for generalizing over recursive structures.

So what does this mean for our JSONZipper? Let's see if we can rewrite the whole 
while being generic over our base-functor.

```haskell
data RecursiveZipper i f = RecursiveZipper
  { parents' :: [(i, Fix f)],
    focused' :: Fix f
  }
```

So far so good! We just parameterize the base functor and the index, otherwise
our structure is basically the same as the JSON one here.

```haskell
class Indexable f i | f -> i where
  getIndex :: i -> f r -> Maybe r
  setIndex :: i -> r -> f r -> f r
```

In addition to generically talking about recursion, we'll need a generic way
to talk about different _locations_ within a recursive structure.
The `Indexable` typeclass I've defined here is just one way to do this.

Let's re-implement some of our combinators.

```haskell
newZipper' :: Fix f -> RecursiveZipper i f
newZipper' value = RecursiveZipper [] value

down' ::
  Indexable f i =>
  i ->
  RecursiveZipper i f ->
  Maybe (RecursiveZipper i f)
down' key (RecursiveZipper parents f@(Fix focused)) =
  case getIndex key focused of
    Nothing -> Nothing
    Just child -> Just $ RecursiveZipper ((key, f) : parents) child
```

This is effectively the same implementation we used for JSON, but now we talk about
our indexing and nesting generically using our Indexable typeclass.
We'll do the same for our `up` combinator.

```haskell
up' ::
  Indexable f i =>
  RecursiveZipper i f ->
  Maybe (RecursiveZipper i f)
up' (RecursiveZipper parents focused) =
  case parents of
    [] -> Nothing
    ((key, Fix v) : ancestors) -> Just $ RecursiveZipper ancestors (Fix (setIndex key focused v))
```

Great! The `Fix` makes things a bit more confusing to work with, but users of the library won't need to worry about that
because it's hidden away in the internals of the `RecursiveZipper` type most of the time.

Next we'll need to find a way to fix my rendering problems, for that we'll need a new type of recursive spine.

## Render caching with Cofree

This section requires a bit of knowledge about the `recursion-schemes` library.

As I mentioned earlier on, rendering the contents of a structural editor to screen
is a bit trickier than a text editor, since we don't necessarily know which nodes
relate to which on-screen positions. We can alleviate this problem by doing an initial
render of the whole structure and caching the text representation of each node and its children
along with its size. If we do this correctly we can keep track of the size of each node, and only need
to update the renders of nodes we've changed and their ancestors.

In order to track renders alongside nodes we'll need to be able to tag our recursive
structure with additional data, the type for this is called `Cofree` and it's defined like this:

```haskell
data Cofree f a = a :< f (Cofree f a)
```

This type is the same as `Fix`, but has an additional `a` threaded through.
We can use this `a` type to keep track of whatever information we want on our base functor `f`.

If we update the `RecursiveZipper` to use this instead, we get:

```haskell
data Zipper i f a = Zipper
  { parents :: [(i, Cofree f a)],
    focus :: Cofree f a
  }
  deriving (Functor)
```

And if we pull in `recursion-schemes` we can easily build combinators for tagging
tree structures as needed.


```haskell
import qualified Data.Functor.Foldable as FF

tagged :: FF.Recursive t => (t -> a) -> t -> Zipper i (FF.Base t) a
tagged f t = Zipper $ Cofree.unfold (\x -> (f x, FF.project x)) t
```

This `tagged` helper allows us to construct a zipper where each node is tagged with its render.
Then we just need to make sure we update the render of the focused node every time we edit that node. If we move 'up' a layer we can re-render that node using the cached renders of its children.
