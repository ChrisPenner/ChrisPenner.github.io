---
title: "Transform and query nested data with Optics and State"
author: "Chris Penner"
date: "May 24, 2020"
tags: [haskell, optics]
description: "Using the Monads and optics to quickly and easily transform and query data."
---

Recognize patterns

jq
Meander
XPath

DSLs, good, but not re-usable or usable with other tools


Components of these systems, and their functional equivalents.

## The characteristics of a traversal system

1. Implicit scope
2. Multiplicity
3. Mutation
4. Cross-Reference


Recursive - ability to adjust selection
Multiplicity
Mutate deeply nested state OR Run a sort of query
Combine data from multiple places in a structure


## Implicit Scope

### What is implicit scope

The first trait that these systems have in common is that their combinators all make use of an implicit local scope, at all times there's a selected node or context. 

For example, each *jq* expression references `.` which references the currently focused value.

In the following expression, the first half selects the `foo` property of the focused object and uses that as the focus for the second half of the expression, which focuses the first element of its array, and will use that as the focus for the next expression, and so on.

```jq
.foo | .[0]
```

An XPath behaves similarly; it is composed of multiple nested selector expressions.

For example:

```xquery
//[@id="references"]/a[@href="google.ca"]
```

The `//div` **focuses** all `div`s in the document, the `/a[@href="google.ca"]` is run once within the context of of each focused div, selecting any `a` tag which is a direct child and links to `google.ca`.

The benefit of implicit scope within these systems is primarily brevity and concision. It allows us to dive deeply into a structure and reference data which is "local" to the current position without drowning in syntax. We will also see that local scopes make it much easier to think about concepts like multiplicity, which we'll address in the next section.

### Implicit scope in functional programming

Functional programming libraries provide a near-perfect tool for addressing well typed implicit scoping. The *Reader Monad*!

The Reader Monad's primary purpose is to provide access to an implicit context, and even provides the `local` combinator for adjusting that context as we dive deeper.

The core operations provided by `Reader` are `ask` and `local`.

```
ask :: Reader r r
local :: (r -> r) -> Reader r r
```

Let's see what it looks like to use implicit local scope in Haskell by employing the Reader Monad.

First I'll set up some example data with a bit of nesting:

```haskell
data Address =
    Address { street :: String
            }
    deriving Show

data Book =
    Book { title  :: String
         , author :: String
         }
    deriving Show

data BookStore =
    BookStore { address :: Address
              , books   :: [Book]
              }
    deriving Show

bobsBooks :: BookStore
bobsBooks =
    BookStore (Address "213 Drury Lane")
              [ Book "The Great Gatsby" "F. Scott Fitzgerald"
              , Book "Moby Dick" "Herman Melville"
              ]
```

In its simplest form, we can simply use function composition!

```haskell
import Control.Category ((>>>))

gatsbyAuthor :: BookStore -> String
gatsbyAuthor =
    books >>> head >>> author

>>> gatsbyAuthor bobsBooks
"F. Scott Fitzgerald"
```

Pointfree expressions accept their arguments implicitly, so composing functions in pointfree form is a quick and easy approximation of implicit scope.

We can do one better though, what if we want to perform several operations within a given scope? We can use the Monad instance of functions alongside do-notation to use the implicit argument in multiple expressions.

```haskell
gatsbySummary :: BookStore -> String
gatsbySummary = books >>> head >>> do
    theTitle <- title
    theAuthor <- author
    return $ theTitle <> " by " <> theAuthor

>>> gatsbySummary bobsBooks
"The Great Gatsby by F. Scott Fitzgerald"
```

This may seem a bit strange if you haven't used functions-as-monads before, but it's all perfectly sound. Each expression on the right-hand of a `<-` accepts the current *context* as an argument, then assigns the result to the binding!

This is perhaps not a terribly _familiar_ way to write Haskell, but nonetheless it is effective, concise, and typesafe.

## Multiplicity

### What is multiplicity

An aspect which makes traversal systems so nice to use is that you can very easily perform the same operation in many different contexts at the same time. That is, they allow you to abstract over the *multiplicity* of an operation. This is very similar to how the `map` function allows you to apply a function for a single element over a container with zero or more values, it leads to simpler code and more code re-use!

In an *XPath*, it looks something like this:

```xquery
//div//a/text()
```

This expression selects all `div`s in a document, then, for *each* of those divs, it selects every `a` tag inside, then, for *each* of those tags it selects the text! Notice how the expression doesn't need to explicitly handle the notion that zero or more nodes may be selected by each expression, it's just baked into the system.

Let's look at *jq* next, it's very similar.

```jq
.foo | .[] | .bar
```

This expression selects the `foo` property of an object, then the `.[]` focuses each member of an array or object one at a time. *jq* will then run the `.bar` expression over *each* member of the array or object one at a time. This allows us to very concisely drill down into structured data and collect pieces of information spanning the structure.

One other important aspect is the ability to handle *failure* in an expression.

In an XPath, an expression which fails to _match_ will simply prune that "branch" of computation and it will be ignored by all following expressions. Just as the `//` operator can *add* many branches to the tree, failing expressions can *remove* branches!

*jq* is nearly identical, however it makes the filtering more explicit by providing a separate `?` operator.

```jq
.foo | .[] | .bar?.baz?
```

This expression is very similar to the previous one, however if either `bar` or `baz` are mission from the focused value, the branch will simply be pruned without any errors.

### Multiplicity in functional programming

How do we accomplish this magic in Haskell? If you guessed "a monad" you're right!

Did you know that lists implement the Monad typeclass? They sure do! Each time you bind a list in a monadic context it's as though the remainder of the expression will be evaluated using *each* value of that list!

```haskell
listMonad :: [String]
listMonad = do
    name <- ["Bob", "Steven"]
    return $ "Hello " <> name
```

(A note to the pros out there, yes I'm aware that this doesn't actually count as using the monad yet)

If we bind multiple values we'll see every possible *combination* of values; here's a quick example to help build a small amount of intuition. I'd also strongly recommend playing around with it on your own.

```haskell
listMonad :: [(Char, Int)]
listMonad = do
    char <- ['a', 'b']
    num <- [1, 2, 3]
    return (char, num)

>>> listMonad
[('a',1),('a',2),('a',3),('b',1),('b',2),('b',3)]
```

So we can see that the list monad actually encompasses a lot of the behaviour we're looking for!

Let's expand on our bookstore example, but this time we'll build a summary for *every* book in the store.

Since we'll want *both* implicit scope _and_ *multiplicity*, we'll want to use both `Reader` and `[]`! Luckily, monad transformers are a thing!

`ListT` can get a bit messy, so we'll make sure to put Reader on the outside.

