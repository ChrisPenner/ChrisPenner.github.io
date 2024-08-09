---
title: "Building Type Search for Unison"
author: Chris Penner
date: July 30, 2024
tags: [programming, haskell, unison]
description: Building an efficient and scalable type search for Unison
image: flux-monoid/flux.jpg
---

# Motivating type-directed search

If you haven't used a type-directed code search like [Hoogle](https://hoogle.haskell.org/) before
it can be tough to fully understand how useful it can be. Before I started on my journey to learn Haskell
I had never even thought to ask for a tool like it. Now I reach for it daily.

Many languages offer some form of code search, or a package search at least, which allows you to find code which is relevant for 
the task you're trying to accomplish, but typically one searches these using human language which causes a few issues. For one, 
these searches require the code or package being documented well enough to contain the phrases you're searching for. 
For two, searching at the package level is often far too rough-grained for the problem at hand. If I just want to very quickly remember the name of the function 
which lifts a `Char` into a `Text`, I already know it's in the Text package, but can save a bit of time by asking for it precisely.
Most importantly though, natural languages are quite imprecise (incidentally a reason programming languages continue to exist).

If I google "javascript function to group elements of a list using a predicate" I find many different functions which do _some_ form of grouping, 
but none of them quite match the shape I had in mind, and I need to read through blogs, stack-overflow answers, and package documentation to determine
whether the provided functions actually do what I'd like them to.

In Haskell, I would instead express that question using a type! 
If I enter the type `[a] -> (a -> Bool) -> ([a], [a])` into Hoogle I get a list of
functions which match that type exactly, there are few other operations with a matching signature, 
but I can quickly open those definitions on Package and determine that `partition` is what I was looking for in this case.

Hopefully that helps to convince you of the utility of a type-directed search, 
though it does raise a question; if type-directed search is so useful, why isn't it more ubiquitous?

I haven't interviewed or surveyed anyone to know for sure, but there are few rather significant obstacles
implementors face.

* A lot of languages don't have sufficiently advanced type-systems to express useful queries
* Not all languages have a centralized package repository
* Indexing every function ever written in your language can be computationally expensive
* It's not immediately obvious how to implement such a system

I hope to help with at least the final point in this post.

# Establishing our goals

Before attempting to build a system which solves our search problem we should be precise about what our search problem actually is.

Here are a few goals we had for the Unison type-directed search:

* I should be able to find functions based on a partial type-signature
* I shouldn't need to know the argument order of a given function in order to find it
* I shouldn't need to know the names the implementor used for their type variables
* The search should be fast
* The search should scale
* The search should be *good*...

The last criteria is subjective of course, but you know it when you see it.

# The method

Let's talk about how we'll accomplish these goals. I'll skim past how we implemented search-by-name since
that's a pretty straight-forward plain-text search, and will instead focus on the type-search aspect.

It's easy to imagine search methods which match _some_ of these characteristics, 
e.g. we could imagine running the typechecker to unify the user-provided type with 
the type of every definition, but this would be far too slow, wouldn't allow partial matches,
and it definitely wouldn't scale. Or we could do a plain-text search over rendered type signatures, 
but this would be very imprecise.

Surprisingly, Hoogle uses a linear scan over a set of pre-built function-fingerprints
for whittling down potential matches. In our case,
Unison Share (our code-hosting platform and package manager for Unison) is backed
by a Postgres database which is where the code we'll be indexing is stored, so 
I investigated a few different Postgres index variants and landed on a GIN (Generalized inverted index).

I won't go too in-depth here on how GIN indexes work, as you can consult the excellent [Postgres documentation](https://www.postgresql.org/docs/current/gin.html)
if you'd like a deeper dive into that area.
For a quick overview, the gist of it is that a GIN index allows us to quickly find rows which are associated to a given combination of search tokens.
They're typically useful when implementing full-text searches, for instance we may choose to index a text document like the following:

```

postgres=# select to_tsvector('And what is the use of a book without pictures or conversations?');
                      to_tsvector
-------------------------------------------------------
 'book':8 'convers':12 'pictur':10 'use':5 'without':9
(1 row)
```

The generated lexemes represent a fingerprint of the text file which can be used to 
quickly and efficiently determine a subset of stored documents which can then be filtered
using other more precise methods.

We can do something similar to search for a set of properties of a type signature.

The properties we want to search for can be distilled from our requirements; we need to know
which types are mentioned in the signature, and we need some way to _normalize_ type variables and argument position.

From this I devised how we'd build a set of lexemes (a.k.a. search tokens) from a given type signature to facilitate this.

## Mentions of concrete types

Assuming we can parse a user-provided signature, let's start with normalizing mentions of concrete types.

Consider the following signature: `Text.take : Nat -> Text -> Text`

We can boil down the info here into the following data:

* A type called `Nat` is mentioned _once_, and it does _NOT_ appear in the return type of the function.
* A type called `Text` is mentioned _twice_, and it _does_ appear in the return type of the function.

There really aren't any rules on how to represent lexemes in a GIN index; earlier we saw an English language tokenizer
doing its thing, but we can just as easily construct our own literal lexemes manually from this information.

Here's the format I went with for type mentions

Here's a token: `mn,1,Nat.`. I start by namespacing the token by its type, in this case `mn` for Mention by Name. 
Next I include the number of times it has been mentioned, followed by it's fully qualified name with the path _reversed_.

In this case `Nat` is a single segment, but if the type were named `data.JSON.Array` it would be encoded as `Array.JSON.data.`,
Why? Well Postgres allows us to do wild-card matches over tokens in GIN indexes, so later on this will allow us to search for matches 
for any valid suffix of the path, e.g. `mn,1,Array.*`, `mn,1,Array.JSON.*` or `mn,1,Array.JSON.data.*` would all match a single mention of this type.

Users don't always know ALL of the arguments of a function they're looking for, so we'd love for partial type matches to still return results. 
For instance `Nat -> Text` should still find `Text.take`, so to facilitate that,
when we have more than a single mention we make a separate token for each of the `1..n` mentions, e.g. in `Text.take : Nat -> Text -> Text` we'd have both `mn,1,Text` AND `mn,2,Text`.
This is that this is primarily a text index, so we can't easily search for tokens where the number of mentions is greater than or equal to some number, but if we just include all the tokens for each 
lower number of mentions the partial type search will work just fine.

This is Unison after all, so if there's a SPECIFIC type you care about you can search for it exactly by hash, e.g. `#abcdef -> #ghijk` will boil down to the tokens:
`mh,1,#abcdef` and `mh,1,#ghijk`. Similar to name mentions this allows us to search using only a prefix of the actual hash.

Lastly I want to track when a given mention appears in the return type of a definition, which I do by simply including an extra token with `r` in the 'mentions' place, e.g. `mn,r,Text`

We can use this later to filter, or score results, or even perform more advances searches like "Show me all functions which produce a value of this type", a.k.a. functions which return that type but don't accept it as an argument.

A note on higher-kinded types and abilities like `Map Text Nat` and `a -> {Exception} b`, I simply treat each of these as its own concrete type mention. The system could be expanded to include more token types for each of these, but one has to be wary of an explosion in the number of generated tokens and in initial testing the search seems to work quite well despite no special treatment.

## Mentions of type variables

That handles concrete types, but what about type variables? Consider the type signature: `const: b -> a -> b`.

This type contains `a` and `b` which are type _variables_. The names of type variables are not important on their own,
you can rename any type variable to anything you like as long as you consider its scope and rename all the mentions of the same variable within its scope.

To normalize the names of type variables I simply use numbers instead; so in this example we may choose to assign `b` the number `1` and `a` the number `2`.
However, we have to be careful because we _also_ want to be agnostic to argument order, such that a search for `a -> b -> b` would still find `const`, if we assigned 
`a` to `1` and `b` to `2` according to the order of their appearance we wouldn't have a match.

To fix this issue we can simply sort the type variables according to their number of _occurrences_, so in this example `a` has fewer occurrences than `b`, so it gets the lower variable ID.

`a` generates the token: `v,1,1`, and `b` generates a token for each occurrence, plus its occurrence in the in the return value of the function:  `v,1,2` `v,2,2` `v,r,2`


## Performing the search

Now that we're done figuring out our token scheme, we simply need to build the appropriate GIN index out of the tokens derived from each type signature.
Now we can process a user search into the same tokens and use them to filter down results!

For Unison's search I've opted to require that each occurrence in the query MUST be present in each match, however for better partial type-signature support I 
don't require that the return values match and instead use the existence of a matching return value to simply boost a results precedence in the results list.

Other match sorting criteria include:
* Types with an arity closer to the user's query are ranked higher
* How complex the type signature is, types with more tokens are ranked lower.
* We give a slight boost to some core projects, e.g. Unison's standard library `base` will show up higher in search results if they match.
* You can include a text search along with your type search to further filter results, e.g. `map (a -> b) -> [a] -> [b]` will prefer finding definitions with `map` somewhere in the name.
* Queries can include a specific user or project to search within to further filter results, e.g. `@unison/cloud Remote`


# Summary

I hope that helps shed some light on how it all works, and perhaps will help others in implementing their own type-directed-search down the road.

Now all that's left is to go [try out a search or two](https://share.unison-lang.org/)!

If you're interested in digging deeper, Unison Share, and by-proxy the entire type-directed search implementation, is all Open-Source, so go check it out!
It's changing and improving all the time, but [this module](https://github.com/unisoncomputing/share-api/blob/6ee875db4ac35156733a0f2c9349bc528736243f/src/Share/Postgres/Search/DefinitionSearch/Queries.hs) would be a good place to start digging.

Let us know in the [Unison Discord](https://unison-lang.org/discord) if you've got any suggested improvements or run into any bugs.
Cheers!
