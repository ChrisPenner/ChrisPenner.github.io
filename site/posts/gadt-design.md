---
title: 'Simpler and safer API design using GADTs'
author: "Chris Penner"
date: "Dec 10, 2020"
tags: [haskell]
description: "Herein we look at how, for certain situations, GADTs can sometimes provide a cleaner approach than typeclasses."
image: gadts.jpg
---

Hey folks! Today we'll be talking about **GADTs**, that is, "Generalized Abstract Data Types".  As the name implies, they're just like Haskell's normal data types, but the _generalized_ bit adds a few new features!  They aren't actually too tough to use once you understand a few principles.

A lot of the writing out there regarding GADTs is pretty high-level research and academia, in contrast, today I'm going to show off a relatively practical and simple use-case. In this post we'll take a look at a very real example where we can leveraged GADTs in a real-world Haskell library to build a simple and expressive end-user interface.

We'll be designing a library for CSV manipulation. I used all of the following techniques to design the interface for my [lens-csv](https://hackage.haskell.org/package/lens-csv) library. Let's get started!

---

Here's a teensy CSV that we'll work with throughout the rest of the post. Any time you see `input` used in examples, assume it's this CSV.

```csv
Name,Age,Home
Luke,19,Tatooine
Leia,19,Alderaan
Han,32,Corellia
```

In its essence, a CSV is really just a list of **rows** and each **row** is just a list of **columns**. That's pretty much it! Any other meaning, even something as benign as "this column contains numbers" isn't tracked in the CSV itself.

This means we can model the data in a CSV using a simple type like `[[String]]`, so far, so simple! There's a bit of a catch here though. Although it's clear to us humans that `Name,Age,Home` is the **header row** for this CSV, there's no marker in the CSV itself to indicate that! It's up to the user of the library to specify whether to treat the first row of a CSV as a header or not, and herein lies our challenge!

Depending on whether the CSV has a header row or not, the user of our library will want to reference the CSV columns by a either a **column name** or **column number** respectively.  In a **dynamic language** (like Python) this is easily handled, we would provide separate methods for indexing columns by either header name or column number, and it would be the programmer's job to keep track of when to use which. In a strongly-typed language like Haskell however, we prefer to prevent such mistakes at compile time. Effectively, we want to give the programmer jigsaw pieces that only fit together in a way that works!

For the sake of pedagogy our miniature CSV library will perform the following tasks:

* Decode a CSV string into a structured type
* Get all the values in a given row

## An Initial Approach

First things first we'll need a `decode` function to parse the CSV into a more structured type. In a production environment you'd likely use performant types like `ByteString` and `Vector`, but for our toy parser we'll stick to the types provided by the Prelude.

Since this is a post about GADTs and not CSVs encodings, we won't worry about escaping or quoting here.  We can just do the naive thing and split our rows into cells on every comma. The Prelude, unfortunately, provides `lines` and `words`, but doesn't provide a more generic splitting function, so I'll whip one up to suit our needs.

Here's a function which splits a string on commas in such a way that each "cell" is separated in the resulting list.

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
      -- A CSV with headers has named columns
      NamedCsv [String] [[String]]
      -- A CSV without headers has numbered columns
    | NumberedCsv [[String]]
    deriving (Show, Eq)
```

Great, now we can write our first attempt of a decoding function. The implementation isn't really important here, so just focus on the type!

```haskell
decode :: Bool -- Whether to parse a header row or not
       -> String -- The csv file
       -> Maybe CSV -- We'll return "Nothing" if anything is wrong
```

And here's the implementation just in case you're following along at home:

```haskell
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

Simple enough; we create a CSV with the correct constructor based on whether we expect headers or not.

Now let's write a function to get a whole column of the CSV. Here's where things get a bit more interesting:

```haskell
getColumnByNumber :: CSV -> Int    -> Maybe [String]
getColumnByName   :: CSV -> String -> Maybe [String]
```

Since each type of CSV takes a different index type we need two different functions in order to do effectively the same thing; let's see the implementations:

```haskell
-- A safe indexing function to get elements by index.
-- This is strangely missing from the Prelude... 🤔
safeIndex :: Int -> [a] -> Maybe a
safeIndex i = lookup i . zip [0..]

-- Get all values of a column by the column index
getColumnByNumber :: Int -> CSV -> Maybe [String]
getColumnByNumber columnIndex (NumberedCsv rows) =
    -- Fail if a column is missing from any row
    traverse (safeIndex columnIndex) rows
getColumnByNumber columnIndex (NamedCsv _ rows) =
    traverse (safeIndex columnIndex) rows

-- Get all values of a column by the column name
getColumnByName :: String -> CSV -> Maybe [String]
getColumnByName  _ (NumberedCsv _) = Nothing
getColumnByName columnName (NamedCsv headers rows) = do
    -- Get the column index from the headers
    columnIndex <- elemIndex columnName headers
    -- Lookup the column from each row, failing if the column is missing from any row
    traverse (safeIndex columnIndex) rows
```

This works of course, but it _feels_ like we're programming in a dynamic language! If you try to get a column **by name** from a **numbered CSV** we know it will ALWAYS fail, so why do we even allow the programmer to express that? Certainly it should fail to typecheck instead! 

```haskell
>>> decode True input >>= getColumnByName "Name"
Just ["Luke","Leia","Han"]

-- We'll get 'Nothing' no matter what if we index a numbered csv by name!
>>> decode False input >>= getColumnByName "Name"
Nothing
```

The problem here becomes even more pronounced when we write a function like `getHeaders`. Which type signature should it have?

This one:
```haskell
getHeaders :: CSV -> [String]
```

Or this one?
```haskell
getHeaders :: CSV -> Maybe [String]
```

We could pick the first signature and always return the empty list `[]` if someone mistakenly tries to get the headers of a numbered CSV, but that seems a bit disingenuous; It's common to check the number of columns in a CSV by counting the headers, and it's not that every numbered CSV has zero columns! If we go with the latter signature it properly handles the failure case of calling `getHeaders` on a numbered CSV, but we know that getting the headers from a `NamedCSV` should **never** fail, so in that case we're adding a bit of unnecessary overhead, all callers will have to unwrap `Maybe` in that case no matter what 😬.

In order to fix this issue we'll need to go back to the drawing board and see if we can keep track of whether our CSV has headers inside its **type**.

## Differentiating CSVs using types

I promise we'll get to using GADTs soon, but let's look at the "simple" approach that I suspect most folks would try next and see where it ends up so we can motivate the need for **GADTs**.

The goal is to prevent the user from calling "header" specific methods on a CSV that doesn't have headers. The simplest thing to do is provide two separate `decode` methods which return completely different concrete result types:


```haskell
decodeWithoutHeaders :: String -> Maybe [[String]]
decodeWithHeaders    :: String -> Maybe ([String], [[String]])
```

Next we could would implement:

```haskell
getColumnByNumber :: Int -> [[String]]             -> Maybe [String]
getColumnByName   :: Int -> ([String], [[String]]) -> Maybe [String]
```

This solves the problem at hand, if we decode a CSV without headers we'll have a `[[String]]` value, and can't pass that into `getColumnByName`.  However, there are a few minor annoyances with this approach.  Notice how we can no longer use `getColumnByNumber` to get a column by number on a CSV which has headers? Of course we could could `snd` it into `[[String]]` first, but converting between types everywhere is annoying and also means we **can't write code which is polymorphic over both kinds of CSV**. Ideally we would have a single set of functions which was _smart_ about which type of CSV so it could do **the right thing** while also ensuring type-safety.

Some readers are likely thinking "Hrmmm, a **group of functions polymorphic over a type**? Sounds like a **typeclass**!" and you'd be right! As it turns out, this is roughly the approach that the popular [`cassava`](https://hackage.haskell.org/package/cassava) library takes to its library design.

`cassava` is more **record-centric** than the library we're designing, so it provides separate typeclasses for named and unnamed record types; `ToNamedRecord`, `FromNamedRecord`, and their numbered variants `ToRecord` and `FromRecord`. In our case we'll be defining different typeclass instances for the CSV itself.

Here's the rough idea:

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

class IsCSV c where
  -- A type family to specify the "indexing" type of the CSV
  type Index c :: Type
  -- Try parsing a CSV of the appropriate type
  decode :: String -> Maybe c

  getColumnByIndex  :: Index c -> c -> Maybe [String]
  getColumnByNumber :: Int     -> c -> Maybe [String]
  getRow :: Int -> Row c
```

Let's talk about the `Index` type family. Numbered CSVs are indexed by an `Int`, while Named CSVs are indexed by a String. We can use the `Index` _associated type family_ to specify a different type of Index for each typeclass instance.

The headerless CSV is pretty easy to implement:

```haskell
instance IsCSV [[String]] where
  type Index [[String]] = Int
  -- You can re-purpose the earlier decoder here.
  decode = ...

  -- The Index type is Int, so we index by Int here:
  getColumnByIndex :: Int -> [[String]] -> Maybe [String]
  getColumnByIndex n rows = traverse (safeIndex n) rows

  -- Since the index is an Int we can re-use the other implementation
  getColumnByNumber :: Int -> [[String]] -> Maybe [String]
  getColumnByNumber = getColumnByIndex
```

Now an instance for a CSV with headers:

```haskell
instance IsCSV ([String], [[String]]) where
  -- We can index a column by the header name
  type Index ([String], [[String]]) = String
  decode = ...

  -- The 'index' for this type of CSV is a String
  getColumnByIndex :: String -> ([String], [[String]]) -> Maybe [String]
  getColumnByIndex columnName (headers, rows) = do
    columnIndex <- elemIndex columnName headers
    traverse (safeIndex columnIndex) rows
  -- We can still index a Headered CSV by column number
  getColumnByNumber :: Int -> ([String], [[String]]) -> Maybe [String]
  getColumnByNumber n = getColumnByNumber n . snd
```

This works out pretty well, here's how it looks to use it:

```haskell
>>> decode input >>= getColumnByIndex ("Name" :: String)
<interactive>:99:36: error:
    • Couldn't match type ‘Index c0’ with ‘String’
      Expected type: Index c0
        Actual type: String
      The type variable ‘c0’ is ambiguous
    • In the first argument of ‘getColumnByIndex’, namely
        ‘"Name"’
      In the second argument of ‘(>>=)’, namely
        ‘getColumnByIndex "Name"’
      In the expression:
        decode input >>= getColumnByIndex "Name"
```

Uh oh... one issue with type classes is that GHC might not know which which instance to use in certain situations!

We can help out GHC with a type hint, but it's a bit annoying and the error message isn't always so clear!

```haskell
>>> decode @([String], [[String]]) input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]

-- Or we can define a type alias to clean it up a smidge
>>> type Named = ([String], [[String]])
>>> decode @Named input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]
```

This works out okay, it's by no means "unusable", but let's take a look at how GADTs can allow us to encourage better error messages while also making it easier to read, and reduce the required boilerplate all at once!

## The GADT approach

Before we use them for CSVs let's get a quick primer on GADTs, if you're well-acquainted already
feel free to skip to the next section.

GADTs, a.k.a. **Generalized Abstract Data Types**, bring a few upgrades over regular
Haskell `data` types. Just in case you haven't seen one before, let's compare
the regular `Maybe` definition to its GADT version.

Here's how `Maybe` is written using standard `data` syntax:

```haskell
data Maybe a =
    Nothing
  | Just a
```

When we turn on `GADTs` we can write the exact same type like this instead:

```haskell
data Maybe a where
  Nothing :: Maybe a
  Just :: a -> Maybe a
```

This slightly different syntax, which looks a bit foreign at first, is really just spelling out the type of constructors as though they were functions!

Compare the definition with the type of each constructor:

```haskell
>>> :t Nothing
Nothing :: Maybe a
>>> :t Just
Just :: a -> Maybe a
```

Each argument to the function represents a "slot" in the constructor.

But of course there's more than just the definition syntax! Why use GADTs? They bring a few upgrades over regular `data` definitions.  GADTs are most often used for their ability to **include constraints over polymorphic types** in their constructor definitions. This means you can write a type like this:

```haskell
{-# LANGUAGE GADTs #-}

data HasEq a where
  HasEq :: Eq a => a -> HasEq a
```

Where the `Eq a` constraint gets "_baked in_" to the constructor such that we can then write a function like this:

```haskell
checkEq :: HasEq a -> HasEq a -> Bool
checkEq (HasEq one) (HasEq two) = one == two
```

We don't need to include an `Eq a` constraint in the type because GHC knows that
it's impossible to construct `HasEq` without one, and it carries that constraint *with the value* in the constructor!

In this post we'll be using a technique which follows (perhaps unintuitively) from this; take a look at this type:

```haskell
data IntOrString a where
  AnInt :: Int -> IntOrString Int
  AString :: String -> IntOrString String
```

Notice how each constructor fills in a value for the polymorphic `a` type? E.g. `IntOrString Int` where `a` is now `Int`? GHC can use this information when it's matching constructors to types. It lets us write a silly function like this:

```haskell
toInt :: IntOrString Int -> Int
toInt (AnInt n) = n
```

Again, this doesn't seem too interesting, but there's something unique here.  It **looks** like I've got an incomplete implementation for `toInt`; it lacks a case for the `AString` constructor! However, GHC is smart enough to realize that any values produced using the `AString` constructor MUST have the type `IntOrString String`, and so it knows that I don't need to handle that pattern here, in fact if I **do** provide a pattern match on it, GHC will display an "inaccessible code" warning!

The really nifty thing is that we can choose whether to be polymorphic over the argument or not in each function definition and GHC will know which patterns can appear in each case. This means we can just as easily write this function:

```haskell
toString :: IntOrString a -> String
toString (AnInt n) = show n
toString (AString s) = s
```

Since `a` might be `Int` OR `String` we need to provide an implementation for **both** constructors here, but note that EVEN in the polymorphic case we still know the type of the value stored in each constructor, we know that `AnInt` holds an `Int` and `AString` holds a `String`.

If you're a bit confused, or just generally unconvinced, try writing `IntOrString`, `toInt` and `toString` in a type-safe manner using a regular `data` constructor, it's a good exercise (it won't work 😉). Make sure you have `-Wall` turned on as well. .

## GADTs and CSVs

After that diversion, let's dive into writing a new CSV type!

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

data CSV index where
  NamedCsv    :: [String] -> [[String]] -> CSV String
  NumberedCsv ::             [[String]] -> CSV Int

-- A side-effect of using GADTs is that we need to use standalone deriving 
-- for our instances.
deriving instance Show (CSV i)
deriving instance Eq   (CSV i)
```

This type has two constructors, one for a CSV with headers and one without.  We're specifying a polymorphic `index` type variable and saying that CSVs with headers are specifically indexed by `String` and CSVs without headers are indexed by `Int`. Notice that it's okay for us to specify a specific type for the `index` parameter even though it's a phantom-type (i.e. we don't actually store the `index` type inside our structure anywhere).

Let's implement our CSV functions again and see how they look.

We still need the end-user to specify whether to parse headers or not, but we can use another **GADT** to reflect their choice in the type, and propagate that to the resulting CSV. Here's what a CSV selector type looks like where each constructor carries some type information with it (i.e. whether the resulting CSV is either String or Int indexed).

```haskell
data CSVType i where
  Named :: CSVType String
  Numbered :: CSVType Int

deriving instance Show (CSVType i)
deriving instance Eq (CSVType i)
```

Now we can write `decode` like this:

```haskell
decode :: CSVType i -> String -> Maybe (CSV i)
decode Named s = case splitOn ',' <$> lines s of
    (h:xs) -> Just $ NamedCsv h xs
    _ -> Nothing
decode Numbered s = Just . NumberedCsv . fmap (splitOn ',') . lines $ s
```

By accepting `CSVType` as an argument it acts as a proxy for the type information we need.  We can provide then provide a separate implementation for each csv-type easily, and the index type provided on the `CSVType` option is propagated to the result, thus determining the type of the output CSV too!

Now for `getColumnByIndex` and `getColumnByNumber`; in the typeclass version we needed to provide an implementation for each class instance, using GADTs we can collapse everything down to a single implementation for function.

Here's `getColumnByIndex`:

```haskell
getColumnByIndex :: i -> CSV i -> Maybe [String]
getColumnByIndex  columnName (NamedCsv headers rows) = do
    columnIndex <- elemIndex columnName headers
    traverse (safeIndex columnIndex) rows
getColumnByIndex n (NumberedCsv rows) = traverse (safeIndex n) rows
```

The type signature says, if you give me the index type which matches the index to the CSV you provide, I can get you that column if it exists. It's smarter than it looks!

Even though the GADT constructor comes after the first argument, by pattern matching on it we can determine the type of `i`, and we then know that the first argument must match that `i` type. So when we match on `NamedCsv` the first argument is a `String`, and when we match on `NumberedCsv` it's guaranteed to be an `Int`

In the original "simple" CSV implementation you could try indexing into a numbered CSV with a `String` header and it would always return a `Nothing`, now it's actually a type error; we've prevented a whole failure mode!


```haskell
-- Decode our input into a CSV with numbered columns
>>> let Just result = decode Numbered input
>>> result
NumberedCsv [
  ["Name","Age","Home"],
  ["Luke","19","Tatooine"],
  ["Leia","19","Alderaan"],
  ["Han","32","Corellia"]]
-- Here's what happens if we try to write the wrong index type!
>>> getColumnByIndex "Name" result
    • Couldn't match type ‘Int’ with ‘String’
      Expected type: CSV String
        Actual type: CSV Int
```

It works fine if provide an index which matches the way we decoded:

```haskell
-- By number using `Numbered`
>>> decode Numbered input >>= getColumnByIndex 0
Just ["Name","Luke","Leia","Han"]
-- ...Or by header name using `Named`
>>> decode Named input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]
```

When indexing by number we can ignore the index type of the CSV entirely, since we know we can index either a Named or Numbered CSV by column number regardless.

```haskell
getColumnByNumber :: Int -> CSV i -> Maybe [String]
getColumnByNumber n (NamedCsv _ rows) = traverse (safeIndex n) rows
getColumnByNumber n (NumberedCsv rows) = traverse (safeIndex n) rows
```

In an earlier attempt we ran into problems writing `getHeaders`, since we **knew** intuitively that it should always be safe to return the headers from a "Named" csv, but we needed to introduce a `Maybe` into the type since we couldn't be sure of the type of the CSV argument!

Now that the CSV has the index as part of the type we can solve that handily by restricting the possible inputs the correct CSV type:

```haskell
getHeaders :: CSV String -> [String]
getHeaders (NamedCsv headers _) = headers
```

We don't need to match on `NumberedCsv`, since it has type `CSV Int`, and that omission allows us to remove the need for a `Maybe` from the signature. Pretty slick!

This is the brilliance of **GADTs** in this approach, we can be general when we want to be general, or specific when we want to be specific.

The interfaces provided by each approach look relatively similar at the end of the day. The typeclass signatures have a fully polymorphic variable with a type constraint AND a type family, whereas the GADT signatures are simpler, including only a polymorphic index type, and the consumers of the library won't need to know anything about GADTs in order to use it.

The typeclass approach:

```haskell
decode :: IsCSV c => String -> Maybe c
getColumnByIndex :: IsCSV c => Index c -> c -> Maybe [String]
getColumnByNumber :: IsCSV c => Int -> c -> Maybe [String]
getHeaders :: IsCSV c => c -> Maybe [String]
```

The GADT approach:

```haskell
decode :: CSVType i -> String -> Maybe (CSV i)
getColumnByIndex :: i -> CSV i -> Maybe [String]
getColumnByNumber :: Int -> CSV i -> Maybe [String]
getHeaders :: CSV String -> [String]
```

Though similar, I find the GADT version easier to understand as a consumer, everything you need to know is available to you, and you can look up the `CSV` type to learn more about how to build one, or which types are available.

The GADT types also result in simpler type errors when something goes wrong.

Here's one common problem with the **typeclass approach**, decode has a **polymorphic result** and `getColumnByIndex` has a polymorphic argument, GHC can't figure out what the intermediate type should be if we string them together:

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

We can fix this with an explicit type application, but that requires us to know the underlying type that implements the instance.

```haskell
>>> type Named = ([String], [[String]])
>>> decode @Named input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]
```

If we mismatch the index type here, even when providing an explicit type annotation, we get a slightly confusing error since it still mentions a type family:

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

Compare these to the errors generated by the GADT approach; first we'll chain `decode` with `getColumnByIndex`:

```haskell
>>> decode Named input >>= getColumnByIndex "Name"
Just ["Luke","Leia","Han"]
```

There's no ambiguity here! We only have a single CSV type to choose, and the "index" type variable is fully determined by the `Named` argument. Very nice!

What if we try to index by number instead?

```haskell
>>> decode Named input >>= getColumnByIndex (1 :: Int)
error:
    • Couldn't match type ‘String’ with ‘Int’
      Expected type: CSV String -> Maybe [String]
        Actual type: CSV Int -> Maybe [String]
    • In the second argument of ‘(>>=)’, namely
        ‘getColumnByIndex (1 :: Int)’
      In the expression:
        decode Named input >>= getColumnByIndex (1 :: Int)
      In an equation for ‘it’:
          it = decode Named input >>= getColumnByIndex (1 :: Int)
```

It clearly outlines the expected and actual types:

```haskell
      Expected type: CSV String -> Maybe [String]
        Actual type: CSV Int -> Maybe [String]
```

Which should be enough for the user to spot their mistake and patch it up.

## Next steps

Still unconvinced? Try taking it a step further!

Try writing `getRow` and `getColumn` functions for both the typeclass and GADT approaches.  The row that's returned should support type-safe index by `String` or `Int` depending on the type of the source CSV.

E.g. the GADT version should look like this:

```haskell
>>> decode Named input >>= getRow 1 >>= getColumn "Name"
Just "Leia"
>>> decode Numbered input >>= getRow 1 >>= getColumn 0
Just "Leia"
```

You'll likely run into a rough patch or two when specifying different Row result types in the typeclass approach (but it's certainly possible, good luck!)

## Conclusion

This was just a peek at how typeclasses and GADTs can _sometimes_ overlap in the design space. When trying to decide whether to use GADTs or a typeclass for a given problem, try asking the following question:

Will users of my library need to define instances for their own datatypes?

If the answer is **no**, a GADT is often clearer, cleaner, and has better type inference properties than the equivalent typeclass approach!

For a more in-depth "real world" example of this technique in action check out my `lens-csv` library. It provides lensy combinators for interacting with either of named or numbered CSVs in a streaming fashion, and  uses the **GADT** approach to (I believe) great effect.

Enjoy playing around!
