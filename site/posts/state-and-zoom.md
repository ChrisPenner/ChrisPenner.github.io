---
title: "Generalizing 'jq' and Traversal Systems using optics and standard monads"
author: "Chris Penner"
date: "September 27, 2020"
tags: [haskell, optics]
description: "We rebuild the core behaviour of 'jq' using standard Haskell combinators and optics"
---

Hi folks! Today I'll be chatting about **Traversal Systems** like **jq** and **XPath**; we're going to discover which properties make them useful, then see how we can replicate their most useful behaviours in Haskell using (almost entirely) pre-ols!existing standard Haskell tools! Let's go!

## What's a Traversal System?

First off I'll admit that "Traversal System" is a name I just came up with, you probably won't find anything if you search for it (unless this post really catches on 😉).

A **Traversal System** allows you dive deeply into a piece of data and may allow you to fetch, query, and edit the structure as you go while maintaining references to other pieces of the structure to influence your work. The goal of most Traversal Systems is to make this as painless and concise as possible. It turns out that this sort of thing is **incredibly useful** for manipulating JSON, querying HTML and CSS, working with CSVs, or even just handling standard Haskell Records and data-types.

Some good examples of existing **Traversal Systems** which you may have heard of include the brilliant [jq](https://stedolan.github.io/jq/) utility for manipulating and querying JSON, the **XPath** language for querying XML, and the [meander](https://github.com/noprompt/meander) data manipulation system in Clojure.
Although each of these systems may appear drastically different at a glance, they both *accomplish many of the same goals* of manipulating and querying data in a concise way.

The similarities between these systems intrigued me! They seem so similar, but yet still seem to share very little in the way of structure, syntax, and prior art. They re-invent the wheel for each new data type! Ideally we could recognize the useful behaviours in each system and build a generalized system which works for any data type.

This post is an attempt to do exactly that; we'll take a look at a few things that these systems do well, then we'll re-build them in Haskell using standard tooling, all the while abstracting over the type of data!

## Optics as a basis for a traversal system

For any of those who know me it should be no surprise that my first thought was to look at optics (i.e. Lenses and Traversals). In general I find that optics solve a lot of my problems, but in this case they are particularly appropriate! Optics inherently deal with the idea of diving deep into data and querying or updating data in a structured and compositional fashion. 

In addition, optics also allow abstracting over the data type they work on. There are pre-existing libraries of optics for working with JSON via [`lens-aeson`](https://hackage.haskell.org/package/lens-aeson) and for html via [`taggy-lens`](https://hackage.haskell.org/package/taggy-lens). I've written optics libraries for working with [CSVs](https://hackage.haskell.org/package/lens-csv) and even [Regular Expressions](https://hackage.haskell.org/package/lens-regex-pcre), so I can say confidently that they're a brilliantly adaptable tool for data manipulation.

It also happens that optics are well-principled and mathematically sound, so they're a good tool for studying the properties that a system like this may have.

However, optics themselves don't provide everything we need! Optics are rather obtuse, in fact I wrote [a whole book](https://leanpub.com/optics-by-example) to help teach them, and they lack clarity and easy of use when it comes to building larger expressions. It's also pretty tough to work on one part of a data structure while referencing data in another part of the same structure. My hope is to address some of these short comings in this post.

In this particular post I'm mostly interested in explaining a framework for traversal systems in Haskell, we'll be using many standard [**mtl**](https://hackage.haskell.org/package/mtl) Monad Transformers alongside a lot of combinators from the [**lens**](https://hackage.haskell.org/package/lens) library. You won't need to understand any of these intimately to get the _gist_ of what's going on, but I won't be explaining them in depth here, so you may need to look elsewhere if you're lacking a bit of context.

## Establishing the Problem

I'll be demoing a few examples as we go along so let's set up some data. I'll be working in both **jq** and **Haskell** to make comparisons between them, so we'll set up the same data in both **JSON** and Haskell.

Here's a funny lil' company as a JSON object:

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

And here's the same data in its Haskell representation, complete with generated optics for each record field.

```haskell
data Company = Company { _staff :: [Employee]
                       , _salaries :: M.Map Int Int
                       } deriving Show
data Pet = Pet { _petName :: String
               , _petType :: String
               } deriving Show
data Employee = Employee { _employeeId :: Int
                         , _employeeName :: String
                         , _employeePets :: [Pet]
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

## Querying

Let's dive into a few example queries to test the waters! First an easy one, let's write a query to find all the cats owned by any of our employees.

Here's how it looks in **jq**:

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

We look in the `staff` key, then *enumerate* that list, then for each staff member we enumerate their cats! Lastly we filter out anything that's not a cat.

We can recognize a few hallmarks of a **Traversal System** here. **jq** allows us to "dive" down deeper into our structure by providing a path to where we want to be. It also allows us to **enumerate** many possibilities using the `[]` operator, which will forward **each** value to the rest of the pipeline one after the other. Lastly it allows us to **filter** our results using `select`.

And in Haskell using optics it looks like this:

```haskell
>>> toListOf (staff . folded . employeePets . folded . filteredBy (petType . only "cat")) company
[ Pet {_petName = "Rocky", _petType = "cat"}
, Pet {_petName = "Inigo", _petType = "cat"}
]
```

Here we use "toListOf" along with an optic which "folds" over each staff member, then folds over each of their pets, again filtering for "only" cats.

At a glance the two are extremely similar!

They each allow the *enumeration* of multiple values, in **jq** using `[]` and in optics using `folded`.

Both implement some form of **filtering**, **jq** using `select` and our optics with `filteredBy`.

Great! So far we've had no trouble keeping up! We're already starting to see a lot of similarities between the two, and our solutions using optics are easily generalizable to any data type.

Let's move on to a more complex example.

## Keeping references

Let's say that as part of a tax audit we need to fetch the pets of every employee who makes more than $15/hr.

First, here's the **jq**:

```sh
$ cat join.json | jq '
    .staff[] 
  | .name as $personName 
  | .pets[] 
  | "\(.name) belongs to \($personName)"
'
"Rocky belongs to bob"
"Bullwinkle belongs to bob"
"Inigo belongs to sally"
```

Here we see a new feature in **jq** which is the ability to maintain **references** to a part of the structure for later while we continue to dig deeper into the structure. We're grabbing the name of each employee as we enumerate them and saving it into `$personName` so we can refer to this later on. Then we enumerate each of the pets and use string interpolation to describe who owns each pet.

If we try to stick with optics on their own, well, it's possible, but unfortunately this is where it all starts to break down, look at this absolute mess:

```haskell
owners :: [String]
owners = 
  company ^.. 
    (staff . folded . reindexed _employeeName selfIndex <. employeePets . folded . petName) 
    . withIndex 
    . to (\(eName, pName) -> pName <> " belongs to " <> eName)

>>> owners
[ "Rocky belongs to bob"
, "Bullwinkle belongs to bob"
, "Inigo belongs to sally"
]
```

You can bet that nobody is calling that "easy to read". Heck, I wrote a book on optics and it still took me a few tries to figure out where the brackets needed to go!

Optics are great for handling a *single* stream of values, but they're much worse at more complex expressions, especially those which require a reference to values that occur _earlier_ in the chain. Let's see how we can address those shortcomings as we build our **Traversal System** in Haskell.

Just for the **jq** aficionados in the audience I'll show off this alternate version which uses a little bit of _magic_ that **jq** does for you.

```sh
 $ cat company.json | jq '.staff[] | "\(.pets[].name) belongs to \(.name)"'
"Rocky belongs to bob"
"Bullwinkle belongs to bob"
"Inigo belongs to sally"
```

Depending on your experience may be less **magical** and more **confusing** 😬. Since the final expression contains an **enumeration** (i.e. `\(.pets[].name)`) **jq** will expand the final term once for each value in the enumeration. This is really cool, but unfortunately a bit "less principled" and tough to understand in my opinion.

Regardless, the behaviour is the same, and we haven't replicated it in Haskell satisfactorily yet, let's see what we can do about that!

## Monads to the rescue (again...)

In Haskell we love our **embedded DSLs**; if you give a Haskeller a problem to solve, you can bet that 9 times out of 10 they'll solve it with a custom monad and an DSL 😂. Well, I'm sorry to tell you that I'm no different!

We'll be using a monad to address the readability problem of the last optics solution, but the question is... _which_ monad?

Since all we're doing at the moment is **querying** data, we can make use of the esteemed **Reader Monad** to provide a context for our query.

Here's what that last query looks like when we use the [`Reader`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html) monad with the relatively lesser known [`magnify`](https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Combinators.html#v:magnify) combinator:

```haskell
owners' :: Reader Company [String]
owners' = do
    magnify (staff . folded) $ do
        personName <- view employeeName
        magnify (employeePets . folded) $ do
            animalName <- view petName
            return [animalName <> " belongs to " <> personName]

>>> runReader owners' company
[ "Rocky belongs to bob"
, "Bullwinkle belongs to bob"
, "Inigo belongs to sally"
]
```

I won't explain how the `Reader` monad itself works here, so if you're a bit shaky on that you'll probably want to familiarize yourself with that first.

As for `magnify`, it's a combinator from the `lens` library which takes an **optic** and an **action** as arguments. It uses the optic to focus a **subset** of the `Reader`'s environment, then runs the action within a Reader with that data subset as its focus. It's just that easy!

One more thing! `magnify` can accept a `Fold` which focuses **multiple** elements, in this case it will run the action once for **each** focus, then combine all the results together using a **semigroup**. In this case, we wrapped our result in a **list** before returning it, so magnify will go ahead and automatically **concatenate** all the results together for us. Pretty nifty that we can get so much functionality out of `magnify` without writing any code ourselves!

We can see that rewriting the problem in this style has made it considerably easier to read. It allows us to "pause" as we use optics to descend and poke around a bit at any given spot. Since it's a monad and we're using do-notation, we can easily bind any intermediate results into names to be referenced later on; the names will correctly reference the value from the current iteration! It's also nice that we have a clear indication of the scope of all our bindings by looking at the indentation of each nested do-notation block.

Depending on your personal style, you could write this expression using the `(->)` monad directly, or even omit the indentation entirely; though I don't personally recommend that. In case you're curious, here's the way that I DON'T RECOMMEND writing this:

```haskell
owners'' :: Company -> [String]
owners'' = do
  magnify (staff . folded) $ do
  eName <- view employeeName
  magnify (employeePets . folded) $ do
  pName <- view petName
  return [pName <> " belongs to " <> eName]
```

## Updating deeply nested values

Okay! On to the next step! Let's say that according to our company policy we want to give a $5 raise to anyone who owns a dog! Hey, I don't make the rules here 🤷‍♂️. Notice that this time we're running an **update** not just a **query**!

Here's one of a few different ways we could express this in **jq**

```jq
cat company.json | jq '
[.staff[] | select(.pets[].type == "dog") | .id] as $peopleWithDogs
| .salaries[$peopleWithDogs[]] += 5
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

I'll admit that it took me a few tries to get this right in **jq**; if you're not careful you'll **enumerate** in a way that means `jq` can't keep track of your references and you'll be unable to edit the correct piece of the original object. For example, here's my first attempt to do this sort of thing:

```sh
$ cat company.json | jq '
. as $company 
| .staff[] 
| select(.pets[].type == "dog").id 
| $company.salaries[.] += 5
'

jq: error (at <stdin>:28): Invalid path expression near attempt to access element "salaries" of {"staff":[{"id":"1","name"...
```

In this case it looks like **jq** can't edit something we've stored as a variable; a bit surprising, but fair enough I suppose.

This sort of task is tricky because it involves enumeration over one area, storing those results, then enumerating AND updating in another! It's definitely possible in `jq`, but some of the magic that **jq** performs makes it a bit tough to know what will work and what won't at a glance.

Now for the Haskell version:

```haskell
salaryBump :: State Company ()
salaryBump = do
    ids <- gets $ toListOf 
            ( staff 
            . traversed 
            . filteredBy (employeePets . traversed . petType . only "dog") 
            . employeeId
            )
    for_ ids $ \id' ->
        salaries . ix id' += 5

>>> execState salaryBump company
Company { _staff = [ Employee { _employeeId = 1
                              , _employeeName = "bob"
                              , _employeePets = [ Pet { _petName = "Rocky"
                                              , _petType = "cat"
                                              }
                                        , Pet { _petName = "Bullwinkle"
                                              , _petType = "dog"
                                              }
                                        ]
                              }
                   , Employee { _employeeId = 2
                              , _employeeName = "sally"
                              , _employeePets = [Pet { _petName = "Inigo"
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

You'll notice that now that we need to **update** a value rather than just **query** I've switched from the `Reader` monad to the `State` monad, which allows us to keep track of our Company in a way that imitates mutable state.

First we lean on optics to collect all the ids of people who have dogs. Then, once we've got those ids we can iterate over our ids and perform an update action using each of them. The `lens` library includes a lot of nifty combinators for working with optics inside the `State` monad; here we're using `+=` to "statefully" update the salary at a given id. `for_` from `Data.Foldable` correctly sequences each of our operations and applies the updates one after the other.

When we're working inside `State` instead of `Reader` we need to use `zoom` instead of `magnify`; here's a rewrite of the last example which uses `zoom` in a trivial way; but `zoom` allows us to also edit values after we've zoomed in!

```haskell
salaryBump :: State Company ()
salaryBump = do
    ids <- zoom ( staff 
                . traversed 
                . filteredBy (employeePets . traversed . petType . only "dog")
                ) $ do
              uses employeeId (:[])
    for_ ids $ \id' ->
        salaries . ix id' += 5
```

## Next Steps

So hopefully by now I've convinced you that we can faithfully re-create the core behaviours of a language like `jq` in Haskell in a data-agnostic way! By swapping out your optics you can use this same technique on JSON, CSVs, HTML, or anything you can dream up. It leverages standard Haskell tools, so it composes well with Haskell libraries, and you maintain the full power of the Haskell language so you can easily write your own combinators to expand your vocabulary.

The question that remains is, where can we go from here? The answer, of course, is that we can add more monads!

Although we have `filtered` and `filteredBy` from `lens` to do filtering of our enumerations and traversals using optics; it would be nice to have the same power when we're inside a do-notation block! Haskell already has a stock-standard combinator for this called [`guard`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#v:guard). It will "fail" in whichever monad you're working with. To work it depends on your type having an instance of the `Alternative` type; which unfortunately for us `State` does NOT have; so we'll need to look for an _alternative_ way to get an Alternative instance 😂

The `MaybeT` monad transformer exists specifically to add failure to other monad types, so let's integrate that! The tricky bit here is that we want to fail only a **single** branch of our computation, not the whole thing! So we'll need to "catch" any failed branches before they merge back into the main computation.

Let's write a small wrapper around zoom to get the behaviour we want.

```haskell
infixr 0 %>
(%>) :: Traversal' s e -> MaybeT (State e) a -> MaybeT (State s) [a]
l %> m = do
    zoom l $ do
        -- Catch and embed the current branch so we don't fail the whole program
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
            isDog <- employeePets . traversed %> do
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
                 , _employeePets =
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
                 , _employeePets = [ Pet { _petName = "Inigo"
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

Hopefully that helps to show how a few optics along with a few monads can allow you to replicate the power of something like `jq` and even add more capabilities, all by leveraging composable tools that already exist and while maintaining the full power of Haskell!

There are truly endless types of additional combinators you could add to make your code look how you want, but I'll leave that up to you. You can even use `ReaderT` or `StateT` as a base monad to make the whole stack into a transformer so you can add any other Monadic behaviour you want to your DSL (e.g. `IO`).

## Is it really data agnostic?

Just to show that everything we've built so far works on any data type you like (so long as you can write optics for it); we'll rewrite our Haskell code to accept a JSON `Aeson.Value` object instead!

You'll find it's a bit longer than the `jq` version, but keep in mind that it's fully typesafe!

```haskell
salaryBumpJSON :: MaybeT (State Value) ()
salaryBumpJSON = do
    ids <- key "staff" . values %> do
        isDog <- key "pets" . values %> do
                        pType <- use (key "type" . _String)
                        return $ pType == "dog"
        guard (or isDog)
        use (key "id" . _String)
    for_ ids $ \id' ->
        key "salaries" . key id' . _Integer += 5
```

As you can see it's pretty much the same! We just have to specify the type of JSON we expect to find in each location (e.g. `_String`, `_Integer`), but otherwise it's very similar!

For the record I'm not suggesting that you go and replace all of your CLI usages of `jq` with Haskell, but I hope that this exploration can help future programmers avoid "re-inventing" the wheel and give them a more mathematically structured approach when building their traversal systems; or maybe they'll just build those systems in Haskell instead 😉

I'm excited to see what sort of cool tricks, combinators, and interactions with other monads you all find!


## Bonus

Just to show that you can do "real" work with this abstraction here are a few more examples using this technique with different data types. These examples will still be a bit tongue in cheek, but hopefully show that you really can accomplish actual tasks with this abstraction across a wide range of data types.

---

First up; here's a transformation over a kubernetes manifest describing the pods available in a given namespace. You can see an example roughly what the data looks like [here](https://gist.github.com/ChrisPenner/53e4b505ff0673b39c60e6c926b2715d).

This transformation takes a map of docker image names to port numbers and goes through the manifest and sets each container to use the correct ports. It also tags each pod with all of the images from its containers, and finally returns a map of container names to docker image types! It's pretty cool how this abstraction lets us mutate data while also returning information.


```haskell
{-# LANGUAGE OverloadedStrings #-}
module K8s where

import Data.Aeson hiding ((.=))
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Foldable

-- Load in your k8s JSON here however you like
k8sJSON :: Value
k8sJSON = undefined

transformation :: M.Map T.Text Int -> State Value (M.Map T.Text T.Text)
transformation ports = do
    zoom (key "items" . values) $ do
        containerImages <- zoom (key "spec" . key "containers" . values) $ do
            containerName <- use (key "name" . _String)
            imageName <- use (key "image" . _String . to (T.takeWhile (/= ':')))
            zoom (key "ports" . values) $ do
                let hostPort = M.findWithDefault 8080 imageName ports
                key "hostPort" . _Integral .= hostPort
                key "containerPort" . _Integral .= hostPort + 1000
            return $ M.singleton containerName imageName
        zoom (key "metadata" . key "labels") $ do
          for_ containerImages $ \imageName ->
              _Object . at imageName ?= "true"
        return containerImages

imagePorts :: M.Map T.Text Int
imagePorts = M.fromList [ ("redis", 6379)
                        , ("my-app", 80)
                        , ("postgres", 5432)
                        ]

result :: (M.Map T.Text T.Text, Value)
result = runState (transformation imagePorts) k8sJSON
```

---

Next up; let's work with some HTML! The following transformation uses `taggy-lens` to interact with HTML (or any XML you happen to have lying around.)

This transformation will find all direct parents of `<img>` tags and will set the `alt` tags on those images to be all the text inside the parent node.

After that, it will find all `<a>` tags and wrap them in a `<strong>` tag while also returning a list of all `href` attributes so we can see all the links we have in the document!

```haskell
{-# LANGUAGE OverloadedStrings #-}
module HTML where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Control.Monad.State
import Text.Taggy.Lens
import Control.Lens hiding (elements)

transformation :: State TL.Text [T.Text]
transformation = do
    -- Select all tags which have an "img" as a direct child
    zoom (html . elements . deep (filteredBy (elements . named (only "img")))) $ do
        -- Get the current node's text contents
        altText <- use contents
        -- Set the text contents as the "alt" tag for all img children
        elements . named (only "img") . attr "alt" ?= altText

    -- Transform all "a" tags recursively
    (html . elements . transformM . named (only "a")) 
      -- Wrap them in a <strong> tag while also returning their href value
      %%= \tag -> (tag ^.. attr "href" . _Just, Element "strong" mempty [NodeElement tag])
```

---

Lastly let's see a CSV example! I'll be using my [`lens-csv`](https://hackage.haskell.org/package/lens-csv-0.1.1.0/docs/Data-Csv-Lens.html) library for the optics.

This simple example iterates through all the rows in a csv and uses an overly simplistic formula to recompute their ages based on their birth year.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module CSV where

import Control.Lens
import Data.Csv.Lens
import qualified Data.ByteString.Lazy as BL
import Control.Monad.State

recomputeAges :: State BL.ByteString ()
recomputeAges = do
    zoom (namedCsv . rows) $ do
        preuse (column @Int "birthYear") >>= \case
            Nothing -> return ()
            Just birthYear -> do
                column @Int "age" .= 2020 - birthYear
```

Hopefully these last few examples help convince you that this really is an adaptable solution even though they're still a bit silly.
