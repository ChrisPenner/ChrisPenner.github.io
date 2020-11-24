---
title: ''
author: "Chris Penner"
date: "Oct 31, 2020"
tags: [haskell]
description: ""
image: withered.jpg
---

Hey folks! Today we'll be talking about GADTs, that is, "Generalized Abstract Data Types".
It sounds pretty intense, and they bring a lot of power to the table, but luckily
aren't actually too tough to use once you understand a few principles.

There's a lot of "high level" research and academia about GADTs and what they
bring to the proverbial table, but I've often been asked what they're good for
in terms of practical, everyday programming. In this post we'll take a look at a very real example
where we can leveraged GADTs in a real-world Haskell library to build a simple
and expressive end-user interface.

The problem we'll be working with is CSV manipulation, we're going to build a handy
library for parsing a CSV and poking around at different rows and columns.

Here's a teensy CSV that we'll work with throughout the rest of the post:

```csv
Name,Age,Home
Luke,19,Tatooine
Leia,19,Alderaan
Han,32,Corellia
```

In its essence, a CSV is really just a **list of rows** and each row is just a list
of **columns**. Additional structure can be added after the fact by parsing columns
into different types; but since none of this information is tracked in the format
of the CSV itself, every cell in a CSV starts out as just a `String`.

All this means that we can model the data in a CSV using a simple type like `[[String]]`,
so far, so simple! There's a bit of a catch here though:
although it's clear to us humans that `Name,Age,Home` is a header row for this
CSV, there's no distinction in the CSV itself between the header row and the
rest of the CSV. According to the specification, you can specify the existence
of a header in the MIME Type for the CSV, but of course that won't be available
to us when we're reading a CSV within the library.
That means we'll have to rely on the user of the library to specify whether or not
to treat the first row of the document as a set of column headers.

This is where things get interesting; Depending on whether the CSV has a header
row or not the consumer of the CSV will want to reference columns by a
**column name** or **column number** respectively.
In a dynamic language this is pretty easy to handle, we could provide separate methods
for indexing columns by header name or column number and it would be the
programmer's job to keep track of when to use which. However, in a strongly-typed language
like Haskell we much prefer to prevent such mistakes at compile time;
meaning we'd like to limit the programmer to performing sensible actions
using the type system. This is the challenge which this article sets out to solve.

Our well-typed CSV library will need to perform the following functions:

* Decode a CSV string into a structured type
* Extract a list of the values of a given column for all rows
* Allow re-ordering or deleting columns
* Encode the structured type back into a string

## Initial Approach

First things first we'll need a `decode` function to parse the CSV into a
more structured type. In a production environment you'd likely use more
performant types like `ByteString` and `Vector`, but for our toy parser we'll
stick to the Prelude.

This is a post about GADTs, not CSVs, so we won't worry about escaping or quoting here.
We'll do the naive thing and split our rows into cells on every comma.
The Prelude, unfortunately, provides `lines` and `words`, but doesn't provide
a more generic splitting function, so I'll whip one up to suit our needs.

```haskell
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn splitter = foldr go [[]]
  where
    go char xs
      -- If the current character is our "split" character create a new partition
      | splitter == char = []:xs
      -- Otherwise we can add the next char to the current cell
      | otherwise = case xs of
          (cell:rest) -> (char:cell):rest
          [] -> [[char]]

```

We can try it out to ensure it works as expected:

```haskell
>>> splitOn ',' "a,b,c"
["a","b","c"]

-- Remember that CSV cells might be empty and we need it to handle that:
>>> splitOn ',' ",,"
["","",""]
```

Now we'll write a type to represent our CSV structure, it will have a constructor for a CSV with headers, one for a CSV without headers:

```haskell
data CSV =
      NamedCsv [String] [[String]]
    | NumberedCsv [[String]]
    deriving (Show, Eq)
```

Great, now we can write our first attempt of a decoding function.

```haskell
decode :: Bool -- Whether to parse a header row or not
       -> String -- The csv file
       -> Maybe CSV
-- Parse a header row
decode True input =
    case splitOn ',' <$> lines input of
        (headers:rows) -> Just (NamedCsv headers rows)
        [] -> Nothing
-- No header row
decode False input =
  let rows = splitOn ',' <$> lines input
   in Just (NumberedCsv rows)
```

Simple enough; we create a CSV with the correct constructor based on whether
we expect headers or not.

Now let's write a function to get a whole column of the CSV.
Here's where things get a bit more interesting:

```haskell
getColumnByNumber :: CSV -> Int -> Maybe [String]
getColumnByName :: CSV -> String -> Maybe [String]
```

Since each type of CSV takes a different index type we need two different functions in order to do effectively the same thing; let's see the implementations:

```haskell
-- Safe indexing function
safeIndex :: Int -> [a] -> Maybe a
safeIndex i = lookup i . zip [0..]

getColumnByNumber :: Int -> CSV -> Maybe [String]
getColumnByNumber columnIndex (NumberedCsv rows) =
    traverse (safeIndex columnIndex) rows
getColumnByNumber columnIndex (NamedCsv _ rows) =
    traverse (safeIndex columnIndex) rows

getColumnByName :: String -> CSV -> Maybe [String]
getColumnByName  _ (NumberedCsv _) = Nothing
getColumnByName columnName (NamedCsv headers rows) = do
    -- Get the column index from the headers
    columnIndex <- elemIndex columnName headers
    -- Lookup the column from each row
    traverse (safeIndex columnIndex) rows
```

This works of course, but it feels very dynamic! If you try to get a column by
name from a numbered CSV you'll ALWAYS fail, the problem becomes more pronounced
when we write a function like `getHeaders`. Which type signature should it have?

This:
```haskell
getHeaders :: CSV -> [String]
```

Or this?
```haskell
getHeaders :: CSV -> Maybe [String]
```

We could pick the first signature and always return `[]` for a numbered CSV, but
that seems a bit disingenuous, especially since it's common to check the number
of columns in a CSV by counting the headers. If we go with the latter signature
it properly handles the failure case, but we know that getting the headers from a `NamedCSV` should **never** fail, so it adds a bit of unnecessary overhead to
handle the `Maybe` in that case.

To fix this problem we need to keep track of whether the CSV has headers in its type.

## Splitting out different CSV types

I promise we'll get to using GADTs soon, but let's look at the approach that
I suspect most folks would try to see how they end up.

The simplest method is to have two separate `decode` methods which return different types:


```haskell
decodeWithoutHeaders :: String -> Maybe [[String]]
decodeWithHeaders :: String -> Maybe [Map String String]
```

Then you would implement:

```haskell
getColumnByNumber :: Int -> [[String]] -> Maybe [String]
getColumnByName :: Int -> [Map String String] -> Maybe [String]
```

However there are a few minor annoyances with this approach.
Notice how we can no longer use `getColumnByNumber` on a CSV with headers?
If we used a different representation like `([String], [[String]])` then we could
`snd` it into `[[String]]`, but converting between types everywhere is annoying
and also makes it considerably more difficult to write code which is polymorphic
over the variety of CSV we're working with. Ideally we would have a single set of functions which was smart about which type of CSV we had and would ensure type-safety as well!

Some readers are likely thinking "Hrmm, a group of functions polymorphic over a type? Sounds like a typeclass!" and you'd be right! As it turns out, this is roughly the approach that the popular [`cassava`](https://hackage.haskell.org/package/cassava) library takes.

`cassava` has separate typeclasses for named and unnamed record types (`(To|From)NamedRecord`, `(To|From)Record`), but we'll try defining a typeclass for the CSV type itself.

Here's the rough idea there:

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

class IsCSV c where
  type Index c :: Type
  decode :: String -> Maybe c

  getColumnByIndex :: Index c -> c -> Maybe [String]
  getColumnByNumber :: Int -> c -> Maybe [String]
  getRow :: Int -> Row c
```

Since the `Index` type changes depending on the CSV type we use an _associated type family_ to allow the instance to choose its own index type.

The headerless CSV is pretty easy to implement:

```haskell
instance IsCSV [[String]] where
  type Index [[String]] = Int
  -- You can re-purpose the earlier decoder here.
  decode = undefined

  getColumnByIndex :: Int -> [[String]] -> Maybe [String]
  getColumnByIndex n rows = traverse (safeIndex n) rows

  -- Since the index is exactly Int we can re-use the implementation
  getColumnByNumber :: Int -> [[String]] -> Maybe [String]
  getColumnByNumber = getColumnByIndex
```

When we implement the Header version we hit another snag; we have to pick a representation for it, the two "obvious" options are:

* `[Map String String]`

Unfortunately this representation doesn't allow `getColumnByNumber`, nor can we re-encode our CSV without messing up our column ordering since the original header ordering is lost!

This version works a bit better:

* `([String], [[String]])`

```haskell
instance IsCSV ([String], [[String]]) where
  -- We can index a column by the header name
  type Index ([String], [[String]]) = String
  decode = undefined

  getColumnByIndex :: String -> ([String], [[String]]) -> Maybe [String]
  getColumnByIndex columnName (headers, rows) = do
    columnIndex <- elemIndex columnName headers
    traverse (safeIndex columnIndex) rows
  getColumnByNumber :: Int -> ([String], [[String]]) -> Maybe [String]
  getColumnByNumber n = getColumnByNumber n . snd
```

This works pretty well for the time being; we'll revisit it later, but it's fine
time we saw the GADT approach don't you think?

## Using a GADT

Before we use them for CSVs let's get a quick primer on GADTs, if you're well-acquainted already
feel free to skip to the next section.

GADTs, a.k.a. Generalized Abstract Data Types, bring a few upgrades over regular
Haskell `data` types. Just in case you haven't seen one before, let's compare
the regular `Maybe` definition to its GADT version.

Here's how `Maybe` is written using standard `data` syntax:

```haskell
data Maybe a =
    Nothing
  | Just a
```

When we turn on `GADTs` we can write it like this instead:

```haskell
data Maybe a where
  Nothing :: Maybe a
  Just :: a -> Maybe a
```

So we can see that the syntax, which looks a bit foreign at first, is really just spelling out the type of constructors as though they were functions!
Each argument to the function represents a "slot" in the constructor.

So why use GADTs? They bring a few upgrades over regular `data` definitions.
GADTs are most often used for their ability to include constraints
over polymorphic types in their constructor definitions. This means you can
write a type like this:

```haskell
{-# LANGUAGE GADTs #-}

data HasEq a where
  HasEq :: Eq a => a -> HasEq a
```

Where the `Eq a` constraint gets "baked in" to the constructor such that we can then write a function like this:

```haskell
checkEq :: HasEq a -> HasEq a -> Bool
checkEq (HasEq one) (HasEq two) = one == two
```

We don't need to include an `Eq a` constraint in the type because GHC knows that
it's impossible to construct `HasEq` without one!

In this post we'll be using a technique which follows (perhaps unintuitively) from this; take a look at this type:

```haskell
data IntOrString a where
  AnInt :: Int -> IntOrString Int
  AString :: String -> IntOrString String
```

Notice how each constructor fills in a value for the polymorphic `a` type, e.g. `IntOrString Int` where `a` is now `Int`? GHC can use this information when it's
matching constructors to types. It lets us write a silly function like this:

```haskell
toInt :: IntOrString Int -> Int
toInt (AnInt n) = n
```

Again, this doesn't seem too interesting, but there's something unique here.
It **looks** like I've got an incomplete implementation for `toInt`; it lacks
a case for the `AString` constructor! In reality, GHC is smart enough to realize
that any values produced using the `AString` constructor MUST have the type `IntOrString String`, and so it knows that I don't need to handle that pattern here,
in fact if I **do** provide a pattern match on it, GHC will display an "inaccessible code" warning!

The really nifty thing is that we can choose whether to be polymorphic over the
argument or not, and GHC will know which patterns can appear in each case. This means we can just as easily write this function:

```haskell
toString :: IntOrString a -> String
toString (AnInt n) = show n
toString (AString s) = s
```

Since `a` might be `Int` OR `String` we need to provide an implementation for **both** constructors here.

If you're still a bit confused, or generally unconvinced, try writing `IntOrString`, `toInt` and `toString` in a type-safe manner using a regular `data` constructor, it's a good exercise.

## GADTs and CSVs

Now that we're a bit more familiar with GADTs, let's try writing a new CSV type!

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

data CSV index where
  NamedCsv :: [String] -> [[String]] -> CSV String
  NumberedCsv :: [[String]] -> CSV Int

deriving instance Show (CSV i)
```

This type has two constructors, one for a CSV with headers and one without.
We're specifying a polymorphic `index` type variable and saying that CSVs with
headers are specifically indexed by `String` and CSVs without headers are indexed
by `Int`. Notice that it's okay for us to specify a specific type for the `index` parameter even though it's a phantom-type (i.e. we don't actually store the `index` type inside our structure anywhere).

Let's implement our CSV functions again and see how they look.

We still need the end-user to specify whether to parse headers or not, but we can
use another GADT to help with that!

```haskell
data CSVType i where
  Named :: CSVType String
  Numbered :: CSVType Int

deriving instance Show (CSVType i)
```

Now we can write `decode`:

```haskell
decode :: CSVType i -> String -> Maybe (CSV i)
decode Named s = case splitOn ',' <$> lines s of
    (h:xs) -> Just $ NamedCsv h xs
    _ -> Nothing
decode Numbered s = Just . NumberedCsv . fmap (splitOn ',') . lines $ s
```

By accepting `CSVType` as an argument we can provide separate implementations
for each csv-type easily and since the index type provided on the `CSVType` option
is propagated to the result, the type of the out-going CSV is guaranteed to 
match, and is propagated in the type!

Now for `getColumnByIndex` and `getColumnByNumber`; in the typeclass version
we needed to provide an implementation for each class instance, using GADTs we
can collapse everything down to a single implementation for each.

Here's `getColumnByIndex`:

```haskell
getColumnByIndex :: i -> CSV i -> Maybe [String]
getColumnByIndex  columnName (NamedCsv headers rows) = do
    columnIndex <- elemIndex columnName headers
    traverse (safeIndex columnIndex) rows
getColumnByIndex  n (NumberedCsv rows) = traverse (safeIndex n) rows
```

The type signature says, if you give me the index type which matches the index
to the CSV you provide, I can get you that column if it exists. It's smarter than it looks!
In the original "simple" CSV implementation you could try indexing into a numbered csv with a header and it would return a `Nothing`, now it's actually a type error!

```haskell
>>> let Just result = decode Numbered input
NumberedCsv [
  ["Name","Age","Home"],
  ["Luke","19","Tatooine"],
  ["Leia","19","Alderaan"],
  ["Han","32","Corellia"]]
>>> getColumnByIndex "Name" result
    • Couldn't match type ‘Int’ with ‘String’
      Expected type: CSV String
        Actual type: CSV Int
```

It works fine if provide an index which matches the way we decoded:

```haskell
>>> decode Numbered input >>= getColumnByIndex 0
Just ["Name","Luke","Leia","Han"]
>>> decode Named input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]
```

In an earlier attempt we ran into problems writing `getHeaders`, since we knew
that it should always be safe to return the headers from a "Named" csv, but we
needed to introduce a `Maybe` since we couldn't track the CSV's type!

When indexing by number we can ignore the index type of the CSV entirely:

```haskell
getColumnByNumber :: Int -> CSV i -> Maybe [String]
getColumnByNumber n (NamedCsv _ rows) = traverse (safeIndex n) rows
getColumnByNumber n (NumberedCsv rows) = traverse (safeIndex n) rows
```

Now that the CSV has the index as part of the type we can solve that handily:

```haskell
getHeaders :: CSV String -> [String]
getHeaders (NamedCsv headers _) = headers
```

We don't need to match on `NumberedCsv`, since it has type `CSV Int`.

This is the brilliant part of this approach, we can be general when we want to
be general, or specific when we want to be specific. The ability to provide the
expected type using `CSVType` also prevents the instance ambiguity that can creep
in when using the typeclass approach.



The interfaces provided by each approach look relatively similar at the end of the day. The typeclass signatures have a fully polymorphic variable with a type constraint AND a type family, whereas the GADT signatures are simpler, including only a polymorphic index type, and the consumers of the library won't need to know anything about GADTs in order to use it.

```haskell
decode :: IsCSV c => String -> Maybe c
getColumnByIndex :: IsCSV c => Index c -> c -> Maybe [String]
getColumnByNumber :: IsCSV c => Int -> c -> Maybe [String]
```

```haskell
decode :: CSVType i -> String -> Maybe (CSV i)
getColumnByIndex :: i -> CSV i -> Maybe [String]
getColumnByNumber :: Int -> CSV i -> Maybe [String]
```

The GADT types also result in much simpler type errors.


```haskell
>>> decode input >>= getColumnByIndex "Name"
    • Couldn't match type ‘Index c0’ with ‘String’
      Expected type: Index c0
        Actual type: String
      The type variable ‘c0’ is ambiguous
    • In the first argument of ‘getColumnByIndex’, namely
        ‘("Hi" :: String)’
      In the second argument of ‘(>>=)’, namely
        ‘getColumnByIndex ("Hi" :: String)’
      In the expression:
        decode input >>= getColumnByIndex ("Hi" :: String)
```

We can fix this with an explicit type application, but it's a bit clunky:

```haskell
>>> type Named = ([String], [[String]])
>>> decode @Named input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]
```

```haskell
>>> decode @Named input >>= getColumnByIndex (1 :: Int)
    • Couldn't match type ‘Int’ with ‘String’
      Expected type: Index Named
        Actual type: Int
    • In the first argument of ‘getColumnByIndex’, namely ‘(1 :: Int)’
      In the second argument of ‘(>>=)’, namely
        ‘getColumnByIndex (1 :: Int)’
      In the expression:
        decode @Named input >>= getColumnByIndex (1 :: Int)
```


## Conclusion

I'll note that the topics presented here are in no way a criticism of cassava's
chosen implementation, in fact cassava is much more concerned with parsing
and manipulating Records from CSVs and not as concerned with manipulating the CSV
itself. However my `lens-csv` library had different goals and uses the GADTs approach to (I believe) great effect. Check it out if you're interested in learning more.



















This results in `i` being ambiguous!
```haskell
class CSV c i where
  getRows :: CSVRow row i => c -> [row]
  getRow :: CSVRow row i => c -> Maybe row
  decode :: String -> Maybe c
```

Can fix that with either a type family:


Or functional dependencies:

```haskell
class CSV csv row | csv -> row where
  getRows :: csv -> [row]
  decode :: String -> Maybe csv

class CSVRow row i where
  getColumns :: row -> M.Map i String
```

But then we still get ambiguous types that we need to solve with TypeApplications:

```haskell
namedExample :: Maybe String
namedExample = do
    csv <- decode input
    row <- getRows @NamedCsv csv !!? 0
    M.lookup "one" row
```
