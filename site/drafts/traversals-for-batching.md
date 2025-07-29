---
title: "Traversals for batch work"
author: Chris Penner
date: May 7, 2025
tags: [programming, haskell]
description: "Techniques for lateralizing nested code"
image: flux-monoid/flux.jpg
---

This article is about a code-transformation technique I used to get 100x-300x performance improvements when loading Unison code from Postgres in Unison Share. I haven't seen it documented anywhere else, so wanted to share the trick!

It's a perennial annoyance when I'm programming that often the most readable way to write some code
is also directly at odds with being performant. A lot of data has a tree structure, and so working with this data is usually most simply expressed as a series of nested function calls. Nested function calls are a reasonable approach when executing CPU-bound tasks,
but in webapps we're often querying or fetching data along the way.
In a nested function structure we'll naturally end up interleaving a lot of one-off data requests. In most cases these data requests will block
execution until a round-trip to the database fetches the data we need to proceed.

In Unison Share, I often need to hydrate an ID into an AST structure which represents a chunk of code, and each reference in that code will often contain some metadata or information of its own. We split off large text blobs and external code references from the AST itself, so sometimes these fetches will proceed in layers, e.g. fetch the AST, then fetch the text literals referenced in the tree, then fetch the code referenced by the tree, etc.

When hydrating a large batch of code definitions, if each definition takes N database calls, loading M definitions is NxM database round-trips, NxM query plans, and potentially NxM index or table scans! If you make a call for each text ID or external reference individually, then this scales even worse.

This post investigates a technique I've crafted for iteratively evolving **linear, nested** codepaths into **batched** versions of the
same codepaths which allow you to keep the same nested code structure, avoiding the need to restructure the whole codebase. It also provides a trivial mechanism for **deduplicating** data requests, and even allows using the exact same codepath for loading 0, 1, or many entities in a typesafe way. First a quick explanation of how I ended up in this situation.


## Case study: Unison Share definition loading

I'm in charge of the [Unison Share](https://share.unison-lang.org/) code-hosting and collaboration platform.
The codebase for this webapp started its life as a webapp by collecting bits and pieces of code from the UCM CLI application. UCM uses SQLite, so the first iteration was minimal rewrite which simply replaced SQLite bits with equivalent Postgres calls, but the codepaths themselves were left largely the same.

SQLite operates in-process and loads everything from memory or disk, so for our intents and purposes in UCM it has essentially no latency. As a result, most code for loading definitions from the user's codebase in UCM was written simply and linearly, loading the data only as it is needed. E.g. we may have a method `loadText :: TextId -> Sqlite.Transaction Text`, and if you want to load many text references it was perfectly reasonable to just traverse `loadText` over a list of IDs.

Not all databases have the same tradeoffs though! In the Unison Share webapp we now use a Postgres Database, which means the database has a network call and round-trip latency for each and every query.
We now pay a fixed round-trip latency cost on every query that simply wasn't a factor before.
Something simple like`traverse loadText textIds` is now performing **hundreds** of _sequential_ database calls and individual text index lookups! Postgres doesn't know anything about which query we'll run next, so it can't optimize this at all (aside from warming up caches) That's clearly not good.

To optimize for Postgres we'd much prefer to make one large database call which collects all the `TextId`s and returns all the `Text` values in a single query, this allows Postgres to save a lot of work by finding all text values in a single scan, and means we only pay for a single round-trip delay rather than one delay per text.

Here's a massively simplified sketch of what the original naive linear code looked like:

```haskell
loadTerm :: TermReference -> Transaction (AST TermInfo Text)
loadTerm ref = do
  ast <- loadAST ref
  bitraverse loadTermInfo loadText ast

loadTermInfo :: TermReference -> Transaction TermInfo
loadTermInfo ref =
  queryOneRow [sql| SELECT name, type FROM terms WHERE ref = #{ref} |]

loadText :: TextId -> Transaction Text
loadText textId =
  queryOneColumn [sql| SELECT text FROM texts WHERE id = #{textId} |]
```

We really want to load all the Texts in a single query, but the TextIds aren't
just sitting in a nice list, they're nested within the AST structure.

Assuming your DB query tool has some way to accept and return arrays, I think the
approach folks would commonly take here would be something like the following:

```haskell
batchLoadASTTexts :: AST TermReference TextId -> Transaction (AST TermInfo Text)
batchLoadASTTexts ast = do
  let textIds = Foldable.toList ast
  texts <- fetchTexts textIds
  for ast \textId ->
    case Map.lookup textId texts of
      Nothing -> throwError $ MissingText textId
      Just text -> pure text
  where
    fetchTexts :: [TextId] -> Transaction (Map TextId Text)
    fetchTexts textIds = do
      resolvedTexts <- queryListColumns [sql|
        SELECT id, text FROM texts WHERE id = ANY(#{toArray textIds})
      |]
      pure $ Map.fromList resolvedTexts
```

This solves the biggest problem, most importantly it reduces N queries down to a single batch query which is already a huge improvement, but it's a bit of boilerplate, and we'd need to write a custom version of this for each container we want to batch load texts from.

Clever folks will realize that we actually don't care about the "AST" structure at all, we only need a container which is Traversable, so we can generalize over that:

```haskell
batchLoadTexts :: Traversable t => t TextId -> Transaction (t Text)
batchLoadTexts textIds = do
  resolvedTexts <- fetchTexts textIds
  pure $ fmap (\textId -> case Map.lookup textId resolvedTexts of
    Nothing -> throwError $ MissingText textId
    Just text -> text) textIds
  where
    fetchTexts :: [TextId] -> Transaction (Map TextId Text)
    fetchTexts textIds = do
      resolvedTexts <- queryListColumns [sql|
        SELECT id, text FROM texts WHERE id = ANY(#{toArray textIds})
      |]
      pure $ Map.fromList resolvedTexts
```

This is much better, now we can use this on any form of Traversable, meaning we can now batch load from ASTs, lists, vectors, Maps, and can even just use `Identity` to re-use our query logic for a single ID like this:

```haskell
loadText :: TextId -> Transaction Text
loadText textId = do
  Identity text <- batchLoadTexts (Identity textId)
  pure text
```

However, this approach still requires that the IDs you want to batch load are the focus of some Traversable instance; what if your structure contains a half-dozen different ID types?
Bitraversable can handle up to two parameters, but after that you're back to writing bespoke functions for your container types.

For instance, how would we use this technique to batch load our `TermInfo` from the AST's `TermReference`s?

```haskell
-- Assume we've written these batched term and termInfo loaders:
batchLoadTexts :: Traversable t => t TextId -> Transaction (t Text)
batchLoadTermInfos :: Traversable t => t TermReference -> Transaction (t TermInfo)

loadTerm :: TermReference -> Transaction (AST TermInfo Text)
loadTerm termRef = do
  ast <- loadAST termRef
  astWithText <- batchLoadTexts ast
  ??? astWithText -- How do we load the TermInfos in here?
```

We're getting closer, but Traversable instances just aren't very adaptable, the relevant ID must always be in the final parameter of the type. 
In this case you could get by using [Flip](https://hackage.haskell.org/package/bifunctors-5.6.2/docs/Data-Bifunctor-Flip.html) wrapper, but it's not going to be very readable and this technique doesn't scale past two parameters.

We need some way to define and compose bespoke Traversable instances for any given situation.

## Custom Traversals

In its essence, the Traversable type class is just a way to easily provide a canonical implementation of `traverse` for a given type:

```haskell
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```

As it turns out, we don't need a type class in order to construct and pass functions of this type around, we can define them ourselves.

With this signature though it's still requiring that the elements being traversed are the final type parameter of the container 't'; we need a more general version. We can instead use this:

```haskell
type Traversal s t a b = Applicative f => (a -> f b) -> s -> f t
```

It looks very similar, but note that `s` and `t` are now concrete types of kind `*`, they don't take a parameter, which means we can pick any fully parameterized type we like for `s` and `t` which focus some other type `a` and convert or hydrate it into `b`.

E.g. If we want a traversal to focus the `TermReference`s in an `AST` and convert them to `TermInfo`s, we can write:

```haskell
Traversal (AST TermReference text) (AST TermInfo text) TermReference TermInfo

-- Which expands to the function type:

Applicative f => (TermReference -> f TermInfo) -> AST TermReference text -> f (AST TermInfo text)
```

If you've ever worked with optics or the `lens` library before this should be looking mighty familiar, we've just derived `lens`'s [`Traversal`](https://hackage-content.haskell.org/package/lens-5.3.5/docs/Control-Lens-Combinators.html#t:Traversal) type!

Most optics are essentially just generalized traversals, we can write one-off traversals for any situation we might need, and can trivially compose small independent traversals together to create more complex traversals.

Let's rewrite our batch loaders to take an explicit Traversal argument rather than using a type class.

```haskell
import Control.Lens qualified as Lens
import Data.Functor.Contravariant

-- Take a traversal, then a structure 's', and replace all TextIds with Texts to
-- transform it into a 't'
batchLoadTextsOf :: Lens.Traversal s t TextId Text -> s -> Transaction t
batchLoadTextsOf traversal s = do
  let textIds = toListOf (traversalToFold traversal) s
  resolvedTexts <- fetchTexts textIds
  Lens.forOf traversal s $ \textId -> case Map.lookup textId resolvedTexts of
    Nothing -> throwError $ MissingText textId
    Just text -> pure text
  where
    fetchTexts :: [TextId] -> Transaction (Map TextId Text)
    fetchTexts textIds = do
      resolvedTexts <- queryListColumns [sql|
        SELECT id, text FROM texts WHERE id = ANY(#{toArray textIds})
      |]
      pure $ Map.fromList resolvedTexts

traversalToFold ::
  (Applicative f, Contravariant f) =>
  Lens.Traversal s t a b ->
  Lens.LensLike' f s a
traversalToFold traversal f s = phantom $ traversal (phantom . f) s
```

The `*Of` naming convention comes from the `lens` library. A combinator ending in `Of` takes 
an traversal as an argument.

It's a bit unfortunate that we need `traversalToFold`, it's just a quirk of how Traversals and Folds are implemented in the lens library, but don't worry we'll replace it with something better soon.

Now we can pass any custom traversal we like into `batchLoadTexts` and it will batch up the IDs and hydrate them in-place.

Let's write the AST traversals we need:

```haskell
astTexts :: Traversal (AST TermReference TextId) (AST TermReference Text) TextId Text
astTexts = traverse

astTermReferences :: Traversal (AST TermReference TextId) (AST TermInfo Text) TermReference TermInfo
astTermReferences f = bitraverse f pure
```

Here we can just piggy-back on existing `traverse` and `bitraverse` implementations, but if you need to write your own, I included a small guide on writing your own custom Traversals with the [traversal](https://hackage-content.haskell.org/package/lens-5.3.5/docs/Control-Lens-Traversal.html#v:traversal) method in the `lens` library, go check that out.

With this, we can now batch load both the texts and term infos from an AST in one pass each.

```haskell
loadTerm :: TermReference -> Transaction (AST TermInfo Text)
loadTerm termRef = do
  ast <- loadAST termRef
  astWithText <- batchLoadTextsOf astTexts ast
  hydratedAST  <- batchLoadTermInfosOf astTermReferences astWithText
  pure hydratedAST
```

## Scaling up

Okay now we're cooking, we've reduced the number of queries per term from `1 + numTexts + numTermRefs` down to a flat `3` queries per term, which is a huge improvement, but there's more to do.

What if we need to load a whole batch of asts at once? Here's a first attempt:

```haskell
-- Assume these batch loaders are in scope:
batchLoadTermASTs :: Traversal s t TermReference (AST TermReference TextId) -> s -> Transaction t
batchLoadTermInfos :: Traversal s t TermReference TermInfo -> s -> Transaction t
batchLoadTexts :: Traversal s t TextId Text -> s -> Transaction t

batchLoadTerms :: Map TermReference TextId -> Transaction (Map TermReference (AST TermInfo Text))
batchLoadTerms termsMap = do
  termASTsMap <- batchLoadTermASTs traverse termsMap
  for termASTsMap \ast -> do
    astWithTexts <- batchLoadTexts astTexts ast
    hydratedAST <- batchLoadTermInfos astTermReferences astWithTexts
    pure hydratedAST
```

This naive approach loads the asts in a batch, but then traverses over the resulting ASTs batch loading the terms and texts:
This is better than no batching at all, but we're still running queries in a loop.
2 queries for each term in the map is still `O(N)` queries, we can do better.

Luckily, Traversals are easily composable! We can effectively distribute the `for` loop 
into our batch calls by adding composing an additional `traverse` so each traversal is applied to every element of the outer map. 
In case you're not familiar with optics, just note that traversals compose from outer to inner from left to right, using `.`; it looks like this:

```haskell
batchLoadTerms :: Map TermReference TextId -> Transaction (Map TermReference (AST TermInfo Text))
batchLoadTerms termsMap = do
  termASTsMap <- batchLoadTermASTs traverse termsMap
  astsMapWithTexts <- batchLoadTexts (traverse . astTexts) termASTsMap
  hydratedASTsMap <- batchLoadTermInfos (traverse . astTermReferences) astsMapWithTexts
  pure hydratedASTsMap
```

If you want, you can even pipeline it like so:

```haskell
  batchLoadTermASTs traverse termsMap
    >>= batchLoadTexts (traverse . astTexts)
    >>= batchLoadTermInfos (traversed . astTermReferences)
```

It was a small change, but this performs _much_ better at scale, we went from `O(N)` queries to `O(1)` queries, that is, we now run EXACTLY 3 queries, no matter how many terms we're loading, pretty cool. In fact, the latter two queries have no data-dependencies on each other, so you can also pipeline them if your DB supports that, but I'll leave that as an exercise (or come ask me on [bluesky](https://bsky.app/profile/chrispenner.ca)).

That's basically the technique, the next section will show a few tweaks which help me to use it at application scale.

## Additional tips

Let's revisit the database layer where we actually make the batch query:

```haskell
import Control.Lens qualified as Lens
import Data.Functor.Contravariant

-- Take a traversal, then a structure 's', and replace all TextIds with Texts to
-- transform it into a 't'
batchLoadTextsOf :: Lens.Traversal s t TextId Text -> s -> Transaction t
batchLoadTextsOf traversal s = do
  let textIds = toListOf (traversalToFold traversal) s
  resolvedTexts <- fetchTexts textIds
  Lens.forOf traversal s $ \textId -> case Map.lookup textId resolvedTexts of
    Nothing -> throwError $ MissingText textId
    Just text -> pure text
  where
    fetchTexts :: [TextId] -> Transaction (Map TextId Text)
    fetchTexts textIds = do
      resolvedTexts <- queryListColumns [sql|
        SELECT id, text FROM texts WHERE id = ANY(#{toArray textIds})
      |]
      pure $ Map.fromList resolvedTexts

traversalToFold ::
  (Applicative f, Contravariant f) =>
  Lens.Traversal s t a b ->
  Lens.LensLike' f s a
traversalToFold traversal f s = phantom $ traversal (phantom . f) s
```

This pattern is totally fine, but it does involve materializing and sorting a Map of all the results, which also requires an Ord instance
on the database key we use. Here's an alternative approach:

```haskell
import Control.Lens qualified as Lens
import Data.Functor.Contravariant
-- Take a traversal, then a structure 's', and replace all TextIds with Texts to
-- transform it into a 't'
batchLoadTextsOf :: Lens.Traversal s t TextId Text -> s -> Transaction t
batchLoadTextsOf traversal s = do
  s & unsafePartsOf traversal %%~ \textIds -> do
      let orderedIds = zip [0 :: Int32 ..] textIds
      queryListColumns [sql|
        WITH text_ids(ord, id) AS (
          SELECT * unnest(#{toArray orderedIds}) AS ids(ord, id)
        )
        SELECT texts.text 
          FROM texts JOIN text_ids ON texts.id = text_ids.id;
        ORDER BY text_ids.ord ASC
      |]
```

Using `unsafePartsOf` allows us to act on the foci of a traversal _as though_ they were in a simple list.
The `unsafe` bit is that it will crash if we don't return a list with the exact same number of elements, so be aware of that, but 
it's the same crash we'd have gotten in our old version if an ID was missing a value.



As the guy who wrote a lenses textbook, I very often get some variant of the same comment, which goes something like this:
"Optics are cool and all, but I can almost always just spend a couple extra lines of code to accomplish the same result in a less confusing way."

Then they'll stare at me intently, expecting a refutation or a new pearl of wisdom depending on how generous they're feeling.

The truth is, this fictional individual is generally-speaking correct! Most uses of optics are pretty easily replaced by adding a chunk of more verbose code, and while whether it's "less confusing" will definitely depend on the reader's familiarity with optics in the first place, it's easy to see how this simple fact is enough to invalidate the need to learn optics at all for many people in many use-cases.

While personally, I think it's worth learning at least enough to understand traversals, I'd like to spend today showing how Traversals can be useful in a way that transcends a simple "do more work in fewer lines of code" situation. It's a pattern I've been using within Unison Share for a couple years now and it's helped me to write my database queries in a way that somehow accomplishes the trifecta of being more performant, more safe, AND more terse.

## What's the problem

When working on the server there are uncountable cases where I'll find myself with some data structure which contain IDs which point to some other data.
Maybe you've got a query which returns a list of User IDs which match a search; maybe you've got something like `Map SalesPersonId (Set SaleId)`, or a more complex ADT which has multiple copies of different IDs spread out within it, which could be as simple as the following model representing a sale between two user accounts:

```haskell
data Sale =
  { seller :: UserId
  , buyer :: UserId
  , cost :: Money
  }
```

When you inevitably need to display information like this on a web page or GUI you'll need to fill out these IDs with the data they represent, and to do that, we'll need to query the database.

In order to allow filling out the data without needing a ton of new types we can first abstract over the relevant fields using a type variable. Sticking with the sales example we could do this:

```haskell
data Sale user =
  { seller :: user
  , buyer :: user
  , cost :: Money
  } deriving stock (Functor, Foldable, Traversable)
```

Now if we've got a `[Sale UserId]` which we need to convert into `[Sale DisplayUser]` we could of course do the following:

```haskell
populateSaleUsers :: [Sale UserId] -> Transaction [Sale DisplayUser]
populateSaleUsers sales = do
  for sales \(Sale sellerId buyerId costId) -> do
    displaySeller <- fetchDisplayUser sellerId
    displayBuyer <- fetchDisplayUser buyerId
    pure $ Sale seller buyer cost
```

Or those who've played with Traversable before may recognize the following simplification:

```haskell
fetchDisplayUser :: UserId -> Transaction DisplayUser
fetchDisplayUser userId =
  queryOneRow [sql| SELECT name, avatar FROM users where id = #{userId} |]

populateSaleUsers :: [Sale UserId] -> Transaction [Sale DisplayUser]
populateSaleUsers = (traverse . traverse) fetchDisplayUser
```

The latter is terser, and either simpler to read or more complex to understand depending who you ask, but they both have the same significant flaw!

We've just implemented an instance of the infamous `N+1` query problem! If you haven't heard of it feel free to go read one of many hundreds of posts about it now; but the gist is that we're looping over many values and performing a _separate_ query for each one.
This means N _separate_ (usually **blocking**!) round-trips to the database, N _separate_ query plannings, and N _separate_ table lookups.

Databases are pretty good at optimizing queries doing the minimal amount of work possible, but only if they know about the whole problem in advance.
`populateSaleUsers` would be much more efficient if we could collect all the `UserId`s and resolve them to `DisplayUser`s in a single batch.

Here's how I would approach this without any special techniques:

```haskell
fetchDisplayUsers :: [UserId] -> Transaction [DisplayUser]
fetchDisplayUsers userIds =
    let orderedIds = zip [0 :: Int32 ..] userIds
    result <- queryListRows
    [sql|
        WITH user_ids(ord, user_id) AS (
            SELECT * FROM #{toTable orderedIds}
        )
        SELECT users.name
            FROM user_ids
            JOIN users ON users.id = user_ids.user_id
        ORDER BY ord
    |]
    if length result /= length userIds
    then error "displayUsersByIdOf: Missing user display info."
    else pure result

populateSaleUsers :: [Sale UserId] -> Transaction [Sale DisplayUser]
populateSaleUsers sales = do
  let allIds = sales & (foldMap . foldMap) \(Sale {seller, buyer})
                                             -> Set.fromList [seller, buyer]
  allDisplayUsers <- fetchDisplayUsers allIds
  let userMap = Map.fromList (zip allIds allDisplayUsers)
  sales & (traverse . traverse)
            \userId -> case Map.lookup userId userMap of
              Nothing -> error $ "Missing User for id: " <> show userId
              Just user -> pure user
```

It's not too bad, but the annoying part is that you'll need to repeat the junk inside `populateSalesUsers` each time
you want to populate a batch of users within a larger structure, which quickly gets tiresome and error prone.
Functional programmers are lazy (Haskellers especially so), and so are pretty likely to just fall back on the slower `N+1` query after a couple times doing the longer-but-more-efficient version. There's gotta be a better way!

## What's the solution

The root of the problem here is that we have a collection of IDs which we want to **traverse** in-place.
If we can find a way to abstract over where those ids exist and in what structure, we can write a batching mechanism that does things efficiently, but is still re-usable without much work!

As it turns out, `Traversal`s are exactly this sort of abstraction.

Here's how we can write that batched version of `fetchDisplayUsers` in a way that _abstracts_ over the actual location of the IDs by employing a `Traversal`.

```haskell
displayUsersByIdOf :: Traversal s t UserId DisplayUser -> s -> Transaction t
displayUsersByIdOf traversal s = do
  s & unsafePartsOf traversal %%~ \userIds -> do
      let orderedIds = zip [0 :: Int32 ..] userIds
      result <- queryListRows
        [sql|
            WITH user_ids(ord, user_id) AS (
                SELECT * FROM #{toTable orderedIds}
            )
            SELECT users.name
                FROM user_ids
                JOIN users ON users.id = user_ids.user_id
            ORDER BY ord
        |]
      if length result /= length userIds
        then error "displayUsersByIdOf: Missing user display info."
        else pure result
```

`displayUsersByIdOf` takes a **Traversal** which selects `UserId`s in ANY given structure according to the provided traversal, then
efficiently replaces them in-place with their corresponding `DisplayUser`.

The only new part from `fetchDisplayUsers` is the user of `unsafePartsOf`.
It's not as scary as it sounds; this utility alters a provided traversal so that instead of mapping over its foci one by one it collects them into a list, allows us to operate on them in a batch, then maps the elements of the resulting list back to the corresponding locations.

It's perfectly safe as long as we guarantee that the returned list is always the same length as the provided one, hence the sanity check at the end.

It's very nearly the same code as `fetchDisplayUsers`, but by abstracting over the traversal we can use it more adaptably.
To that end, we can even trivially rewrite both `fetchDisplayUser` and `fetchDisplayUsers` by passing `id` and `traversed` as the respective traversals like so:

```haskell
fetchDisplayUser :: UserId -> Transaction DisplayUser
fetchDisplayUser userId = displayUsersByIdOf id userId

fetchDisplayUsers :: [UserId] -> Transaction [DisplayUser]
fetchDisplayUsers userIds = displayUsersByIdOf traversed userIds
```

And the efficiently batched version of `populateSaleUsers` is just as short as the inefficient version, only now we pass the relevant traversal
down into `displayUsersByIdsOf`.

```haskell
populateSaleUsers :: [Sale UserId] -> Transaction [Sale DisplayUser]
populateSaleUsers sales = displayUsersByIdOf (traversed . traversed) sales
```

Elsewhere in the app we may have more complex structures, something like: `Map SalespersonId [ClientInfo UserId]`; but
since we can abstract over the selection of the `UserId`s we don't need to write complex `for` loops or rewrite any complex queries, we just specify how to traverse the `UserId`s.


```haskell
populateClientMapping :: Map SalespersonId [ClientInfo UserId]
populateClientMapping clientMap = displayUsersByIdOf (traversed . traversed . traversed) clientMap
```

If you've got multiple parameterized IDs you need to populate you can simply chain lookups providing the correct traversal for each type parameter you're targeting. Each chained lookup is a single additional query, which you could try to optimize away, but ultimately it's still leagues better than the `N` queries we started with.

The astute among you may notice this post doesn't address any handling of duplicate `UserId`s we may encounter in our structures.
The code we've written will work fine in spite of duplicates, but if you'd like to optimize them out of the query I'll leave it as an exercise to the reader. A hint is that you can de-duplicate everything within `displayUsersByIdOf` itself, meaning no boiler-plate leaking out into your call-sites.

## How'd we do?
