---
title: 'Recursive Zippers'
author: "Chris Penner"
date: "Nov 20, 2021"
tags: [haskell]
description: "Building a zipper over a recursive structure"
image: gadts.jpg
---

I recently built the [Jet](https://github.com/ChrisPenner/jet#readme) structural JSON editor.
I've built a [text editor](https://github.com/ChrisPenner/rasa#readme) in the past, so I was familiar with some of the
performance issues which were likely to come up. In both of these editors we have to be able to our underlying data
efficiently, but also need to be able to _render_ a portion of our structure efficiently.

Let's start by talking about rendering text. Text is pretty easy to split up and view in chunks, and usually text editors only need to display about fifty lines at a time. 
If only lines 72-138 are visible on screen, we just need to locate that range, then simply dump that text out on screen printing it one character at a time. 
Admittedly there are some complications when we start to consider wrapping lines which are too long for the screen, but the core concept is relatively simple.

Editing is a little trickier. Text is often stored as a large array of bytes,
you might think we can just edit those bytes in place, but that idea breaks down
as soon as you add or delete characters! Let's not forget that text is no longer
limited to ASCII, and a single "character" encoded in UTF-8 may be anywhere between 1 and 4 bytes.
Copying the whole array of bytes every time we type a character is far too expensive, so what's a compromise which addresses our needs?
Luckily all of these problems are largely solved in the world of text editors.

Enter: the clever [Rope data structure](https://en.wikipedia.org/wiki/Rope_(data_structure). 
This structure is carefully designed to allow efficient random-access edits by grouping text into small chunks which are linked together to form a larger structure. 
Its design also permitting efficient renders by effectively indexing our document by its newlines.
This means we can quickly grab and render only lines 72-138, or easily delete a single character on line 47 without needing to copy our entire document.

I won't dig too much deeper on Ropes here, suffice to say they're worth checking out.

In contrast with text editors, most structural editors operate over some form 
of _self recursive_ data structure. Think of things like HTML, JSON, or the AST 
of your favourite programming language, each of these structures is comprised of 
_nodes_, each of which contains some data and/or some child nodes of the same type.

These self-recursive structures aren't as easily sliced and diced. 
They don't have a direct translation to their on-screen representation in the same
way that text does. The nesting that these documents have means that a single node
could take 2 lines to render, or maybe it takes 500 lines! This also makes it very difficult
to know which nodes in the document correspond to which "lines" in the text editor.
If I scan to line 300 in my editor, which JSON path does that translate into?
In general, you can't be sure without rendering the entire document up until that point!

So, rendering is one tough issue to solve, but what about editing?
In a language with mutable data structures you would likely focus a given node
and perform any updates in place. You still need to find ways to efficiently re-render,
and you'll need to find efficient ways to _navigate_ the nodes of your structure, but
editing should be relatively cheap.

In a functional language with immutable data structures we can't simply update the structure in place!
Indeed, if I want to edit the value stored at `users[0].info.address.street_address.postal_code` in my JSON document,
I've likely loaded that JSON document into an `aeson` Value, so I need to first crawl my way down from the root of the document
following the path to that key, then perform my update, and finally I need to rebuild the entire structure around
the changed value. Not only does this take a lot of steps, it also means allocating new nodes in memory for every node which is a parent of my change!
This is the same reason that using `++` to append to the _end_ of a linked list in Haskell is quite expensive,
to do so requires traversing the entirety of the list, and allocating an entirely new spine.

If we have to do this every time the user types a key when we're editing a JSON document that's
several megabytes in size, the editor will be completely non-responsive. It's a no-go.

What we'd love to do is restructure our document tree such that the node we're editing is at the very root, 
and the rest of the document is attached from there. In our linked-list analogy it's very quick and easy to peel off the head of our list, make some changes, and 
attach the changed result to the tail of the list. In this example the entire tail of the list is unaltered and doesn't need to be re-allocated, and we have the element we want
to change immediately within our grasp. This works for accessing the head of a list, but what if we want to make edits to elements in the _middle_ of a list?
Is there a way we can reap these benefits for any location in our structure? This is what want for our recursive document too!

There's a well known structure called a **list zipper** which encodes this idea specifically for linked lists.
It's quite clever in its simplicity really, it allows us to traverse a list and just turns the "parents" of the list inside out as we go along, 
on one side we have a linked list of parents, on the other a linked list of all of our children.

Unfortunately, most recursive documents are symmetric in the same way that a list is,
what does it mean to turn a JSON document inside out or upside down?
Can we make a zipper over a recursive document? Even better, can we do this generically for all recursive documents?

A lot of very smart people have spent a long time thinking about this, and have 
devised some extremely clever solutions! Unfortunately, many of these approaches
are quite complex and as a result the libraries they yield, although very general, are also
extremely difficult to use.

My goal was very specific, I wanted a structure which provided an efficient way
to edit a self-recursive structure, and also helped me efficiently render the 
structure as I edited it!

## A Zipper for JSON

Let's start by looking at what a zipper designed specifically for JSON looks like,
then we can try to generalize it so it works for other domains.

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

In JSON we know that every parent MUST be either an array or an object. There
are other JSON types like numbers and strings of course, but these can't contain
any child nodes.

Now that we've got a basic structure we can start to add some helpful combinators:

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
doesn't correctly match the type of the currently focused node, or if there's no value at that index in the structure to
select.

We can also re-zip our structure by going upwards to a parent.

```haskell
up :: JSONZipper -> Maybe JSONZipper
up (JSONZipper parents current) =
  case parents of
    [] -> Nothing
    (Left (key, hm) : ancestors) -> Just $ JSONZipper ancestors (Object $ HM.insert key current hm)
    (Right (i, arr) : ancestors) -> Just $ JSONZipper ancestors (Array $ arr Vector.// [(i, current)])
```

This one fails if we don't have any parents to re-zip.
If we do have a parent, we'll also know which position to insert our current node back into, so it's a simple
matter of updating the parent structure to overwrite the location which we know the focused child came from.


The astute among you will notice that when we're focusing on a child node, the _old_ child node is still
located inside the parent node. While conceptually this is a little messy, in practice it doesn't cause any
issues since that node will always be overwritten when we rezip our structure. At worst we're keeping
a bit of extra memory around, but due to the way Haskell shares duplicate structures even this is usually not
a major issue. There are many ways to resolve this if you care about the additional memory, but I'll leave that as an exercise
since not all of them generalize well to arbitrary recursive structures.

That's the crux of it! See if you can implement a `sibling` combinator, or any other
helpers you might want.

## Generalizing our Zipper

Great, so we've got a working zipper for JSON, but we've still got to generalize
it to work with any recursive type and we need to solve our rendering performance problems.

First let's talk quickly about how we can _generalize recursion_.
There's a pattern we can use for this which involves factoring the recursion out of our type by defining a new type called a **base functor**.
The idea is that if we want to talk about recursion generically we need to be able to interact with 
the recursive bits of our structure in a well-defined way, and in Haskell we love our Functors and Traversables!

Here's a representation of a simple recursive type, the list:

```haskell
data List a =
    Cons a (List a)
  | Nil
```

Here's what we get if we replace every recursive slot with a new type parameter, thus
forming our base-functor for the list type:

```haskell
data ListF a r =
    ConsF a r
  | NilF
  deriving (Functor, Foldable, Traversable)
```

Notice how the `ListF` type is no longer self-recursive, the recursion has been factored out
and replaced with the `r` parameter (for recursive).

We can reclaim the behaviour of the original type by infinitely nesting our base functor within itself.
Here's a `Fix` type which performs the infinite nesting trick:

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
because it's hidden away in the internals of the RecursiveZipper type most of the time.

Next we'll need to find a way to my rendering problems: Enter Cofree.
