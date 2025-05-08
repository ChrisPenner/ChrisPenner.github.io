---
title: "Traversals for batch work"
author: Chris Penner
date: May 7, 2025
tags: [programming, haskell]
description: "A practical and under-used trick for employing traversals"
image: flux-monoid/flux.jpg
---

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
