---
title: "Transform and query nested data with Optics and State"
author: "Chris Penner"
date: "May 24, 2020"
tags: [haskell, optics]
description: "Using the Monads and optics to quickly and easily transform and query data."
---

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



## What's a Traversal System?

First off, this is a name I just came up with, you probably won't find anything if you search for it (unless this post really catches on 😉).

A traversal system encompasses the idea of **traversing** your way through a piece of (typically recursive) data, allowing you to fetch, query, and edit the structure as you go while maintaining references to other pieces of the structure to influence your actions.

Some good examples of **traversal systems** include the [jq](https://stedolan.github.io/jq/) utility for manipulating and querying JSON, and the [meander](https://github.com/noprompt/meander) data manipulation system in Clojure.

Although each of these systems may appear drastically different at a glance, they both *accomplish many of the same goals* of manipulating and querying data in a concise way.

Honourable mentions for traversal systems go to XPath/XQuery which fit the description, but in their essential forms they lack the ability to **update** data as they traverse.

Each of the examples provided are designed for a specific _type_ of data; i.e. **jq** is for **JSON**, **XPath/XQuery** are for **XML**, and **meander** works on Clojure maps and lists.

As someone fascinated with category theory and abstractions in general my first instinct was to try and find out what these systems have in common and how they might be unified! That's when I dug deeper into my study of Haskell and discovered optics! Optics beautifully generalize over data traversals in a very usage agnostic manner. Optics themselves are an _abstraction_, and so _concrete optics_ can be written for each individual data-type or behaviour you wish to apply them to.

That's a lot of talk with not much code, so let's see how optics compare to something like **jq** so we can build the comparison.

For a fair comparison we'll initialize the same set of data in JSON for `jq` and as Haskell records for use with optics!

Here's the JSON

```json
{
    "staff":
      [
        { "id": "1"
        , "name": "bob"
        , "pets": [
              { "name": "Rocky"
              , "type": "cat"
              },
              { "name": "Bullwinkle"
              , "type": "dog"
              }
            ]
        },
        { "id": "2"
        , "name": "sally"
        , "pets": [
              { "name": "Inigo"
              , "type": "cat"
              }
            ]
        }
      ],
    "salaries": {
        "1": 12,
        "2": 15
    }
}
```

And here's the Haskell representation:

```haskell
data Company = Company { _staff :: [Employee]
                       , _salaries :: M.Map Int Int
                       } deriving Show
data Pet = Pet { _petName :: String
               , _petType :: String
               } deriving Show
data Employee = Employee { _employeeId :: Int
                         , _employeeName :: String
                         , _pets :: [Pet]
                         } deriving Show

makeLenses ''Company
makeLenses ''Pet
makeLenses ''Employee

company :: Company
company = Company [ Employee 1 "bob" [Pet "Rocky" "cat", Pet "Bullwinkle" "cat"] 
                  , Employee 2 "sally" [Pet "Inigo" "dog"]
                  ] (M.fromList [ (1, 12)
                                , (2, 15)
                                ])
```

Let's dive into a few example queries to test the waters! First an easy one, let's write a query to find all the cats that are owned by any employee.

Here's how it looks in `jq`

```jq
$ cat company.json | jq '.staff[].pets[] | select(.type == "cat")'
{
  "name": "Rocky",
  "type": "cat"
}
{
  "name": "Inigo",
  "type": "cat"
}
```

And in Haskell using optics:

```haskell
>>> company ^.. staff . folded . pets . folded . filteredBy (petType . only "cat")
[ Pet {_petName = "Rocky", _petType = "cat"}
, Pet {_petName = "Inigo", _petType = "cat"}
]
```

At a glance the two are extremely similar!

They each allow the *enumeration* of multiple values, in **jq** using `[]` and in optics using `folded`.
When a collection is enumerated, the remainder of the expression operates over **each** value in the enumeration one at a time.

Both of them also implement a form of **filtering**; in **jq** using `select` and in optics with `filteredBy/filtered`.

Great! We're already starting to see a lot of similarities!

Let's move on to a more complex example, let's say that for an arbitrary reason we need to fetch the pets of every employee who makes more than $15/hr.

```sh
 $ cat company.json | jq '.staff[] | "\(.pets[].name) belongs to \(.name)"'
"Rocky belongs to bob"
"Bullwinkle belongs to bob"
"Inigo belongs to sally"
```

This example showcases a little bit of "magic" that **jq** does for you, which depending on your experience may be less **magical** and more **confusing**. Since the final expression contains an **enumeration** (i.e. `\(.pets[].name)`) **jq** will expand the final term once for each value in the enumeration. This is really cool, but unfortunately a bit "less principled" in my opinion.

As for the optics approach, unfortunately this is where it starts to break down, look at this mess:

```haskell
owners :: [String]
owners = 
  company ^.. 
    (staff . folded . reindexed _employeeName selfIndex <. pets . folded . petName) 
    . withIndex 
    . to (\(eName, pName) -> pName <> " belongs to " <> eName)

>>> owners
[ "Rocky belongs to bob"
, "Bullwinkle belongs to bob"
, "Inigo belongs to sally"
]
```

You can bet that nobody is calling that "easy to read". Heck, I wrote a book on optics and it still took me a few tries to figure out where the brackets needed to go!

Optics are great for handling a *single* stream of values, but they're much worse at more complex expressions, especially those which require a reference to values that occur _earlier_ in the chain. Let's see how we can address those shortcomings.

## Adding a Monad

In Haskell, when looking to provide a powerful and expressive language for accomplishing a task, we tend to create an **embedded DSL**. What this means is that we write a set of **combinators** which behave well together and are meant to be used in **do-notation**.

To proceed we'll need to pick out an appropriate **Monad** for the task at hand. Since all we're doing at the moment is **querying** data, we can make use of the **Reader Monad** to provide a context for our query.

Here's what it looks like when we use the `Reader` monad with the `magnify` combinator:

```haskell
owners' :: Reader Company [String]
owners' = do
    magnify (staff . folded) $ do
        eName <- view employeeName
        magnify (pets . folded) $ do
            pName <- view petName
            return [pName <> " belongs to " <> eName]

>>> runReader owners' company
[ "Rocky belongs to bob"
, "Bullwinkle belongs to bob"
, "Inigo belongs to sally"
]
```

I won't explain how the `Reader` monad itself works here, so if you're a bit shaky on that you'll probably want to familiarize yourself with that first.

As for `magnify`, it's a combinator from the `lens` library which takes an **optic** and an **action** as arguments. It uses the optic to focus a *subset* of the Reader's environment, then runs the action within a Reader with that environment subset. It's just that easy!

One more thing! `magnify` can accept a `Fold` which focuses **multiple** elements, in this case it will run the action once for **each** focus, then combine all the results together using a semigroup. In this case, we return our result wrapped in a **list**, so all the results are concatenated together using their semigroup action.

We can see that rewriting the problem in this style has made it much easier to read. It allows us to descend some depth into our structure, pause there, assign some data to bindings, then continue descending with each of those bindings in scope! We also have a clear indication of the scope of all our bindings by looking at the indentation of each block.

Depending on your personal style, you could write this expression using the `(->)` monad directly, or even omit the indentation entirely; though I don't recommend that. Here's a different way to write the same query:

```haskell
owners'' :: Company -> [String]
owners'' = do
  magnify (staff . folded) $ do
  eName <- view employeeName
  magnify (pets . folded) $ do
  pName <- view petName
  return [pName <> " belongs to " <> eName]
```


Okay! On to the next step! Let's say that according to our company policy, we want to give a $5 raise to anyone who owns a dog! Hey, I don't make the rules here; notice that this time, we're running an **update** not just a **query**!

Now we get to see where **jq** starts to break down in readability a little bit.

```jq
cat company.json | jq '
[.staff[] | select(.pets[].type == "dog") | .id] as $d
| .salaries[$d[]] += 5
'

{
  "staff": [
    {
      "id": "1",
      "name": "bob",
      "pets": [
        {
          "name": "Rocky",
          "type": "cat"
        },
        {
          "name": "Bullwinkle",
          "type": "dog"
        }
      ]
    },
    {
      "id": "2",
      "name": "sally",
      "pets": [
        {
          "name": "Inigo",
          "type": "cat"
        }
      ]
    }
  ],
  "salaries": {
    "1": 17,
    "2": 15
  }
}
```

We first scan the staff to see who's worthy of a promotion, then we iterate over each of their ids and bump up their salary, and sure enough it works!

This sort of problem is tricky because it involves enumeration over one area, storing those results, then enumerating AND updating in another!

Now for the haskell version:

```haskell
salaryBump :: State Company ()
salaryBump = do
    ids <- gets $ toListOf (staff . traversed . filteredBy (pets . traversed . petType . only "dog") . employeeId)
    for_ ids $ \id' ->
        salaries . ix id' += 5

>>> execState salaryBump company
Company { _staff = [ Employee { _employeeId = 1
                              , _employeeName = "bob"
                              , _pets = [ Pet { _petName = "Rocky"
                                              , _petType = "cat"
                                              }
                                        , Pet { _petName = "Bullwinkle"
                                              , _petType = "dog"
                                              }
                                        ]
                              }
                   , Employee { _employeeId = 2
                              , _employeeName = "sally"
                              , _pets = [Pet { _petName = "Inigo"
                                             , _petType = "cat"
                                             }
                                        ]
                              }
                   ]
        , _salaries = fromList [ (1, 17)
                               , (2, 20)
                               ]
      }
```

In this case we're able to lean entirely on optics to collect a list of the ids we want, we can then simply iterate through all the matching ids and run a new state action over each of them using `for_` from `Data.Foldable`. In the action we select and bump the appropriate salary.

## Next Steps

At this point we can start to think about additional behaviours we may want to add to our monad; it'd be nice to be able to prune a branch of computation from within based on the data in scope at the time.

Let's write a small wrapper around zoom to get the behaviour we want.

```haskell
infixr 0 %>
(%>) :: Traversal' s e -> MaybeT (State e) a -> MaybeT (State s) [a]
l %> m = do
    zoom l $ do
        e <- get
        a <- lift $ runMaybeT m
        return (maybe [] (:[]) a)
```

This defines a handy new combinator for our traversal DSL which allows us to zoom just like we did before, but the addition of `MaybeT` allows us to easily use `guard` to prune branches!

We make sure to run and re-lift the results of our action rather than embedding them directly otherwise a single failed "guard" would fail the **entire** remaining computation, which we certainly don't want! Since each individual branch may fail, and since we've usually been collecting our results as lists anyways, I just went ahead and embedded our results in a list as part of the combinator, it should make everything a bit easier to use!

Let's try it out! I'll rewrite the previous example, but we'll use `guard` instead of `filteredBy` this time.

```haskell
salaryBump'' :: MaybeT (State Company) ()
salaryBump'' = do
    ids <- staff . traversed %> do
            isDog <- pets . traversed %> do
                       uses petType (== "dog")
            guard (or isDog)
            use employeeId
    for_ ids $ \id' ->
        salaries . ix id' += 5

>>> flip execState company . runMaybeT $ salaryBump''
Company
{ _staff    =
      [ Employee { _employeeId   = 1
                 , _employeeName = "bob"
                 , _pets         =
                       [ Pet { _petName = "Rocky"
                             , _petType = "cat"
                             }
                       , Pet { _petName = "Bullwinkle"
                             , _petType = "dog"
                             }
                       ]
                 }
      , Employee { _employeeId   = 2
                 , _employeeName = "sally"
                 , _pets         = [ Pet { _petName = "Inigo"
                                         , _petType = "cat"
                                         }
                                   ]
                 }
      ]
, _salaries = fromList [(1, 17), (2, 15)]
}
```

I wrote it out in "long form"; the expressiveness of our system means there are a few different ways to write the same thing; which probably isn't a good thing, but you can find the way that you like to work and standardize on that!

It turns out that if you want even **more** power you can replace `MaybeT` with a "List transformer done right" like [`LogicT`](https://hackage.haskell.org/package/logict) or [`list-t`](https://hackage.haskell.org/package/list-t). This will allow you to actually **expand** the number of branches within a zoom, not just filter them! It leads to a lot of power! I'll leave it as an exercise for the reader to experiment with, see if you can rewrite `%>` to use one of these list transformers instead!


















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

Functional programming libraries provide a near-perfect tool for addressing well typed implicit scoping, normally we'd use the *Reader Monad*!

The Reader Monad's primary purpose is to provide access to an implicit context to any actions we perform within that context.

However, since later on in this post we'll be needing the State Monad, to avoid rewriting things later on we'll just start using State from the start. The State Monad provides all the same capabilities as the reader monad, but with the added ability to modify the environment.

Let's see what it looks like to use implicit local scope in Haskell by employing the State Monad, everything in this first section can also be accomplished by using the equivalent methods from the Reader Monad.

First I'll set up some example data with a bit of nesting. We'll define lenses for each field since optics will play a large role in keeping things expressive and terse.

```haskell
data Address =
    Address { _street :: String
            }
    deriving Show

data Book =
    Book { _title  :: String
         , _author :: String
         }
    deriving Show

data BookStore =
    BookStore { _address :: Address
              , _books   :: [Book]
              }
    deriving Show

makeLenses ''BookStore
makeLenses ''Address
makeLenses ''Book

bobsBooks :: BookStore
bobsBooks =
    BookStore (Address "213 Drury Lane")
              [ Book "The Great Gatsby" "F. Scott Fitzgerald"
              , Book "Moby Dick" "Herman Melville"
              ]
```

For very simple queries we actually don't need to take advantage of implicit scoping at all, we can just access things directly using `view` with a lensy path to the data we want.

```haskell
gatsbyAuthor :: BookStore -> String
gatsbyAuthor =
    view (books . ix 0 . author)

>>> gatsbyAuthor bobsBooks
"F. Scott Fitzgerald"
```

What if we want to perform several operations within a given scope?

We can use the handy `magnify` combinator provided by `lens` to select a part of our structure, then provide an action to perform there as a monadic do-notation block.

Note that all "do" blocks here are actually using the function monad, which has the same behaviour as the Reader monad, but it means we can "call" the action directly on some data to get our results!

```haskell
gatsbySummary :: BookStore -> String
gatsbySummary = do
    magnify (books . ix 0) $ do
        theTitle <- view title
        theAuthor <- view author
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

There are two easy ways to handle this sort of multiplicity in Haskell, the "simplest" is to simply pass lists of items around explicitly.

Here's how we'd alter our previous book summary code to compute summaries for each book in turn, it relies on the fact that `magnify` knows how to combine results when the result is a Monoid, we can simply embed the result in a list and `magnify` will collect them for us properly.

```haskell
allSummaries :: BookStore -> [String]
allSummaries = do
    magnify (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        return [theTitle <> " by " <> theAuthor]

>>> allSummaries bobsBooks
["The Great Gatsby by F. Scott Fitzgerald","Moby Dick by Herman Melville"]
```

If we had already abstracted out our "summary" action we could simply `fmap (:[])` onto it to get the same result.

Remember that filtering out results is also part of multiplicity. Here's how we could filter out books who have authors which long names:

```haskell
allSummariesNoLongAuthors :: BookStore -> [String]
allSummariesNoLongAuthors = do
    magnify (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        if length theAuthor > 16 
           then return []
           else return [theTitle <> " by " <> theAuthor]

>>> allSummariesNoLongAuthors bobsBooks
["Moby Dick by Herman Melville"]
```

This is nice, but it means that the multiplicity is explicit instead of implicit! What if we wanted a more implicit style instead?

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

`ListT` can get a bit messy and often doesn't work as expected, so we'll make sure to put Reader on the outside so we can just use `[]` on the inside.

We'll use the same technique we used with our explicit list-passing style, but then we'll use the functionality of our `[]` monad to "lift" away the multiplicity so that it's implicit! We can embed this behaviour into a new `magnify` combinator so we don't even need to worry about it:


```haskell
magnifyEach :: Fold s a -> ReaderT a [] r -> ReaderT s [] r
magnifyEach fld m = do
    xs <- magnify fld $ do
        a <- ask
        return $ runReaderT m a
    lift xs

allSummaries' :: ReaderT BookStore [] String
allSummaries' = do
    magnifyEach (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        return $ theTitle <> " by " <> theAuthor

>>> runReaderT allSummaries' bobsBooks
["The Great Gatsby by F. Scott Fitzgerald","Moby Dick by Herman Melville"]
```

Note that the block which generates the summary itself doesn't need to change at all! This demonstrates the beautiful abstraction and re-usability of traversal systems.

Since the `[]` implements the `Alternative` typeclass, we can filter within this monad by using `guard`!

```haskell
allSummariesNoLongAuthors' :: ReaderT BookStore [] String
allSummariesNoLongAuthors' = do
    magnifyEach (books . folded) $ do
        theTitle <- view title
        theAuthor <- view author
        guard (length theAuthor <= 16)
        return $ theTitle <> " by " <> theAuthor

>>> runReaderT allSummariesNoLongAuthors' bobsBooks
["Moby Dick by Herman Melville"]
```


## Mutation

### What is mutation?

This should be a relatively easy one to understand; most traversal systems support either *querying* a structure _or_ *mutating* it! It's often useful to drill deep down into an object and mutate many pieces of it at once, where each adjustment is specific to its context.

XPath doesn't have first class support for editing things, so we'll look at an example in `jq`.

Here's our bookstore described as JSON:

```json
{ "address": "213 Drury Lane"
, "books": [
        { "title": "The Great Gatsby"
        , "author": "F. Scott Fitzgerald"
        },
        { "title": "Moby Dick"
        , "author": "Herman Melville"
        }
    ]
}
```

Here's how we could truncate our book titles with a mutation using *jq*:

```jq
.books[].title |= .[0:5] + "..."
```

Resulting in:

```json
{
  "address": "213 Drury Lane",
  "books": [
    {
      "title": "The G...",
      "author": "F. Scott Fitzgerald"
    },
    {
      "title": "Moby ...",
      "author": "Herman Melville"
    }
  ]
}
```


The `|=` operator runs the selected value through the filter on its right-hand-side and assigns the result as the new value. Let's see how we can do something similar!
