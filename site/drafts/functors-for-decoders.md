---
title: "Functors for Decoders"
author: Chris Penner
date: Nov 11, 2025
tags: [programming, haskell]
description: "A practical example of Yoneda for solving everyday problems"
image: parallel-pipes.jpg
---

Maybe I hang out in weird circles, but I heard the word "Yoneda" thrown around a lot
before I really understood what people were talking about.

Recently, this concept provided me with some actual utility at work, so it seems like a good
time to explain how it this concept can be useful in practice.

There are many posts about Yoneda from a Category Theory point of view, this post isn't that,
it sidesteps the Category Theory in favor of a practical example, if you want to understand 
more about how Yoneda works, where it came from, and its relevance in Category Theory, 
you can continue your reading elsewhere after you're finished here.

## The Problem

I'm going to jump straight into the actual problem I was trying to solve.

I use `hasql` as a PostgreSQL client in Haskell, and my task at the time required 
processing a very large query which I wanted to stream from the DB to my app to 
minimize memory usage. 

There's the `hasql-cursor-query` library for this, however the interface is unfortunately 
incompatible with the `hasql-interpolate` library I already use (and prefer) when building queries.
I decided to roll my own cursor type using `hasql`'s lower-level API.

I wanted a simple interface, some sort of `Cursor result` type which you could fold over in 
batches. This was easy enough to mock up in a way that was compatible with `hasql-interpolate`.
Note that the following code uses some custom Unison Share imports, so it may not be directly
runnable for you, but you should get the gist.

```haskell
newtype PGCursor a = PGCursor
  { cursorName :: Text
  }

newRowCursor :: Text -> Sql -> Transaction (PGCursor row)
newRowCursor namePrefix query =
  do
    uuid <- transactionUnsafeIO $ randomIO @UUID
    -- Hack together a cursor name that's guaranteed to be unique.
    let cursorName = Text.filter (\c -> Char.isAlphaNum c || c == '_') (namePrefix <> "_" <> into @Text uuid)
    let declaration = fromString $ "DECLARE " <> Text.unpack cursorName <> "\n"
    execute_ $
      declaration
        <> [sql|
      BINARY
      NO SCROLL
      CURSOR
      WITHOUT HOLD
      FOR ^{query}
    |]
    pure $ PGCursor cursorName
```

This allows us to write some code to fetch and process rows from the cursor.
Don't worry about the code too much, the types tell the story.

```haskell
-- | Fetch UP TO the next N results from the cursor. If there are no more rows, returns Nothing.
fetchN :: (DecodeRow r) => PGCursor r -> Int32 -> Transaction (Maybe (Vector r))
fetchN (PGCursor cursorName) n = do
  -- PG doesn't allow bind params for limits or cursor names, so we need to build it as a simple string.
  -- We're safe from injection here because `n` is just an int, and we guarantee the
  -- cursorName is safe at construction time.
  let sql = fromString . Text.unpack $ Text.intercalate " " ["FETCH FORWARD", tShow n, "FROM", cursorName]
  rows <- queryVectorRows sql
  if null rows
    then do
      execute_ $ fromString $ "CLOSE " <> Text.unpack cursorName
      pure Nothing
    else pure $ Just rows


-- | Fold over the cursor in batches of N rows.
foldBatched :: (QueryM m, Monoid a, DecodeRow r) => PGCursor r -> Int32 -> (Vector r -> m a) -> m a
foldBatched cursor batchSize f = do
  batch <- fetchN cursor batchSize
  case batch of
    Nothing -> pure mempty
    Just rows -> do
      acc <- f rows
      (acc <>) <$!> foldBatched cursor batchSize f
```

Okay this is great, and works fine, but the UX is a bit lacking for a couple reasons.
Firstly, this is Haskell, I'd love to be able to map over the cursor to convert it from 
the tuple types I usually have `hasql` Decoders for into my domain types, typically records or Union types.
But alas, there's no sensible `Functor` instance for `PGCursor` which actually does what we want, the row param is just a phantom type. 
We could fmap or coerce it however we like, but at the end of the day we still need the 
`DecodeRow r` constraint when we call `fetchN` and `foldBatched`, so we need that instance on whatever the destination type is.

The second issue is similar, what if I want to create a cursor for something like a column type instead of a row type?
In the current setup, I need a new version of every function in the interface for columns!

In Unison Share I often construct the same domain type from many different queries, so I don't want to define a single `DecodeRow` instance for my domain type, I'd rather use the `DecodeRow` of a simple tuple of columns, then just `fmap` that tuple into whatever I like.
What I really want is to somehow bake in the `DecodeRow` constraint when I construct the cursor, since the query itself knows the format of the data, and then be able to `fmap` over the cursor into any Haskell value I like, no concerning myself with `DecodeRow` instances at the call site.


This exact situation is where Yoneda comes in.

We can redefine the cursor like this:

```haskell
data PGCursor a = PGCursor
  { cursorName :: Text,
    decodeRow :: forall row. DecodeRow row => (row -> a)
  }
```

This _bakes in_ the `DecodeRow` constraint at construction time, and includes a simple mapping function in the type which makes implementing Functor trivial:

```haskell
instance Functor PGCursor where
  fmap f (PGCursor name decodeRow) =
    PGCursor name (f . decodeRow)
```

Now we can tweak our functions like this:

```haskell
fetchN :: PGCursor a -> Int32 -> Transaction (Maybe (Vector a))
```

