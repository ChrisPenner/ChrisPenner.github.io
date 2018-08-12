---
title: "Typesafe Versioned APIs"
author: Chris Penner
date: Aug 4, 2018
tags: [programming, haskell]
description: Exploring writing multiple versions of your application in a typesafe way using data-kinds and functional dependencies
image: typesafe-api-versioning/numbers.jpg
---


Today we're going to look at the idea of using Haskell's type system to **specialize our app 
implementation according to type-level flags**. More specifically we're going
to look at a fun way to write a monadic action which alters its behaviour based
on which **version** of a system it's embedded in, simultaneously gaining ground on [the expression problem](https://en.wikipedia.org/wiki/Expression_problem) 
and giving us compile-time guarantees that we haven't accidentally mixed up code from different versions of our app!

The concrete example we'll be looking at is a simple web handler which returns
a JSON representation of a User; we'll start with a single possible
representation of the user, but will the evolve our system to be able to return
a different JSON schema depending on which **version** of the API the user has
selected.

Disclaimer; the system I present probably isn't a great idea in a large production app, but is a fun experiment to
learn more about higher kinded types and functional dependencies so we're going to do it anyways.
Let's dive right in!

## Starting App

Let's build a quick starting app so we have something to work with; I'll elide
all the web and http related bits, we'll have a simple handler that fetches a user and
our `main` will run the handler and print things out.

```haskell
-- You'll need to have the 'mtl' and 'aeson' packages in your project

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.IO.Class
import Data.Aeson

-- User -----------------------------------
data User = User
  { name :: String
  } deriving (Show)

instance ToJSON (User) where
  toJSON (User {name}) = object ["name" .= name]

-- App Monad --------------------------------
newtype AppM a = AppM
  { runApp :: IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- User Service --------------------------------
class (Monad m) =>
      MonadUserService m
  where
  getUser :: m User

instance MonadUserService AppM where
  getUser = return (User "Bob Johnson")

-- App -----------------------------------------
userHandler :: (MonadUserService m) => m Value
userHandler = do
  user <- getUser
  return $ toJSON user

app :: (MonadIO m, MonadUserService m) => m ()
app = do
  userJSON <- userHandler
  liftIO $ print userJSON

main :: IO ()
main = runApp app
```

Hopefully that's not too cryptic 😅

We've defined a simple user object and wrote an Aeson `ToJSON` instance for it
so we can serialize it. Then we wrote a newtype wrapper around `IO` which we
can use to implement various instances on; note that we use
`GeneralizedNewtypeDeriving` to get our `Monad` and `MonadIO` instances for free.

Next we define our interface for a User Service as the `MonadUserService`
typeclass; this has a single member: `getUser` which defines how to get a user
within a given monad. In our case we'll write the simplest possible
implementation for our service and just return a static "Bob Johnson" user.

Next up we have our handler which gets a user, serializes, then returns it. Lastly we've got an  `app` which calls the user then
prints it, and a `main` which runs the app.

Brilliant, we're all set up; let's run it and see what we get!

```haskell
> main
Object (fromList [("name",String "Bob Johnson")])
```

Chapter 2; wherein our API evolves
----------------------------------

They said we'd never succeed, but damn them all! In spite all of our investor's
criticisms our app is doing wonderfully! We have a whole 7 of users and are
making tens of dollars! Some users at large have requested the ability to get a
user's first and last name separately; but other users have legacy systems
built against our v1 API! Clearly it would be far too much work to duplicate
our entire user handler and make alterations for our v2 API, let's see if we
can **parameterize** our app over our **API version** and **defer** the choice of
app version (and implementation) until the last possible minute!

Like most Haskell refactors we can just start building what we want and let the
compiler guide the way; let's change our `User` data type to reflect the needs
of our users:

```haskell
-- First attempt
data User
  = UserV1 { name :: String }
  | UserV2 { firstName :: String
           , lastName :: String }
  deriving (Show)
```

This reflects the choice in our app that the user type could be either of the
two shapes; but there's a few problems with this approach. First and foremost
is that this means that at EVERY stage in the app where we use a `User` we need
to pattern match over the constructors and handle EVERY one; regardless of
which version we happen to be working with. Not only is this not what we
wanted, but as we add more versions later on the number of possible code paths
we need to handle explodes! One way we can avoid this chaos is to let the type system
know that our data is **versioned**. Enter `GADT`s!

GADT's with Phantom types
-------------------------

Take a gander at this:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

import GHC.TypeLits

data User (v :: Nat) where
  UserV1 :: { name :: String} -> User 1
  UserV2
    :: { firstName :: String
       , lastName :: String}
    -> User 2
```

If you haven't worked with `GADT`s before this may all look a bit strange,
let's break it down a bit:

First off; in order to even talk about `GADT`s we need the `GADTs` language
extension; this unlocks the `data ... where` syntax! There's a bit more to it,
but the basic idea is that this allows us to effectively specify our data
constructors like we would normal functions in haskell. This means we can
specify constaints on our parameters, and in our case we can specialize the
types of the resulting value based on which particular constructor was used. So
if someone uses the `UserV1` constructor they MUST get a `User 1`, and
similarly with `UserV2`. The compiler remembers this info and in our case can
actually tell that if we have a function which accepts a `User 1` that we only
need to match over the `UserV1` constructor since any values of `User 1` MUST
have been constructed using `UserV1`.

Maybe I'm getting ahead of myself; how is it we can suddenly have numbers
like `1` and `2` in our types? The answer lies within the `(v :: Nat)`
annotation. This is a **Kind Signature**, and as such naturally requires the
`KindSignatures` extension. Others have written more exhaustively on the
subject, but the basic idea is that `Nat` is a kind, i.e. a 'type' for types.
This means that the `v` parameter can't take on just any type, but only types
that are part of the `Nat` kind, which corresponds to the natural (aka
non-negative) integers. This is handy, because it means people can't create a
user with a version number of `String` or `()` or something silly like that.
Lastly we need the `DataKinds` extension to allow us to use Data Constructors
in our types; once that's enabled we can import `GHC.TypeLits` and use integer
literals in our types and GHC will figure it all out.

The `v` paramter of our user is also something called a "phantom type". It's a type
parameter on a data type that doesn't actually have an associated value in the
right hand side of the data definition. These sorts of things are useful for
adding additional information at the type level.

Step one done! We've successfully parameterized our datatype over a version
number at the type level! At this point your compiler is probably bugging you
about the fact that the `User` constructor no longer exists; we originally
implemented the `ToJSON` class for the base User type, but now User needs an
additional type parameter. This is good! It means we can implement a different
instance for each version of user we have; which is basically what we wanted to
do in the first place!

Let's alter our `ToJSON` instance so it has a single name parameter for v1 and
a separate first and last name for v2!

```haskell
{-# LANGUAGE FlexibleInstances #-}
-- ...
instance ToJSON (User 1) where
  toJSON (UserV1 {name}) = object ["name" .= name]

instance ToJSON (User 2) where
  toJSON (UserV2 {firstName, lastName}) =
    object ["firstName" .= firstName, "lastName" .= lastName]
```

Here we're specifying **different instances** of `ToJSON` for the different
members of our `User` datatype. Note that, as promised, the compiler KNOWS that
only the matching constructor needs to be matched on and that a `UserV1` won't
show up in an instance for `User 2`. We'll need `FlexibleInstances` turned on so GHC can handle
complex types like `User 1` in an instance definition.

Next it's time to fix up our `MonadUserService` class, we know that `getUser` needs to return
a user, but which user type should it return? We can imagine someone implementing a `MonadUserService` for `User 1`
and also for `User 2`, so it would be nice if instances could specify which version they want to work with. To
accomplish that we can add an additional parameter to the class:


```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
-- ...
class (Monad m) =>
      MonadUserService v m
  where
  getUser :: m (User v)

instance MonadUserService 1 AppM where
  getUser = return (UserV1 "Bob Johnson")

instance MonadUserService 2 AppM where
  getUser = return (UserV2 "Bob" "Johnson")
```

Just like `ToJSON` we can now implement the typeclass instance differently for
each version of our user. We'll need `MultiParamTypeClasses` to add the `v`
parameter to our typeclass.

## Generalizing the handler and app over version

We're moving along nicely! Next we need our `userHandler` and `app` to know
about version numbers, however this layer of our app doesn't really care which
exact version of user it's working with, mostly it just cares that certain
instances exist for that user. Ideally we can write versions of these that work
for either of our user versions all at once.

The first step is to introduce our new paramterized typeclasses:

```haskell
{-# LANGUAGE FlexibleContexts #-}

userHandler :: (ToJSON (User v), MonadUserService v m) => m Value
userHandler = do
  user <- getUser
  return $ toJSON user

app :: (ToJSON (User v), MonadIO m, MonadUserService v m) => m ()
app = do
  userJSON <- userHandler
  liftIO $ print userJSON
```

Now we run into a bit of a problem;

```
    • Could not deduce (MonadUserService v0 m)
      from the context: (MonadIO m, MonadUserService v m)
        bound by the type signature for:
                   app :: forall (m :: * -> *) (v :: Nat).
                          (MonadIO m, MonadUserService v m) =>
                          m ()
        at /Users/cpenner/dev/typesafe-versioning/src/Before2.hs:54:8-48
      The type variable ‘v0’ is ambiguous
    • In the ambiguity check for ‘app’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        app :: (MonadIO m, MonadUserService v m) => m ()
```

It's telling us it can't tell which user version we want it to use! So there
are a few ways we can fix this; one way would be to specify the specific type
that we'd like `v` to be each time it's used; we can enable
`AllowAmbiguousTypes`, `TypeApplications` and `ScopedTypeVariables` to try
that; but this ends up being pretty verbose and isn't the nicest to work with.
We won't dive into that possibility, but I'd recommend you give it a try though
if you like a challenge!

The other option we have is to clear up the disambiguity by giving the type
system another way to determine what `v` should be; in our case the monad `m`
is pervasive throughout our app, so if we can somehow infer `v` from `m`
then we can save ourselves a lot of trouble. We're already associating the two
within the typeclass definition `class (Monad m) => MonadUserService v m`;
however the type system recognizes that there could be an instance for several
different values of `v`; and of course we've implemented exactly that! 

The way to fix this is to tell the type system that there's
**one-and-only-one** `v` for each `m` using **FunctionalDependencies**; and then find a way to encode the `v`
inside the `m` so we can still run the different versions of our app.

Lets add a new extension and alter our typeclass appropriately:

```haskell
{-# LANGUAGE FunctionalDependencies #-}

class (Monad m) => MonadUserService v m | m -> v
  where
  getUser :: m (User v)
```

We've added a the `| m -> v` annotation which reads something like "... where
`m` determines `v`". Adding this annotation allows us to avoid the
`Ambiguous Type` errors because we've told the type system that for any given
`m` there's only one `v`; so if it knows `m`, (which in our case it does) then
it can safely determine exactly which `v` to use.

Now you'll probably see something like this:

```
Functional dependencies conflict between instance declarations:
    instance MonadUserService 1 AppM
    -- Defined at ...
    instance MonadUserService 2 AppM
    -- Defined at ...
```

We told the type system there'd only be a single `v` for every `m`; then
immediately gave it two instances for `AppM`; GHC caught us lying! That's okay,
GHC will forgive us if we can somehow make the two `m`'s different!
We can do this by adding a phantom type to the `AppM` monad which simply denotes which
version we're working with; let's try editing our `AppM` monad like this:

```haskell
newtype AppM (v :: Nat) a = AppM
  { runApp :: IO a
  } deriving (Functor, Applicative, Monad, MonadIO)
```

We've added the `(v :: Nat)` type argument here, it doesn't show up any where
in our data, meaning it's a **Phantom Type** which is just there to help use
denote something at the type level, in this case we denote which user version we're currently
working with. Now we can add that additional info to our `MonadUserService` instances:

```haskell
instance MonadUserService 1 (AppM 1) where
  getUser = return (UserV1 "Bob Johnson")

instance MonadUserService 2 (AppM 2) where
  getUser = return (UserV2 "Bob" "Johnson")
```

It seems a bit redundant, but it gets us where we're going!

Not done yet! We still need to tell GHC which version of our `app` we want to run! You can
use `TypeApplications` for this if you like, but the easier way is to just specify with a type annotation:

```haskell
main :: IO ()
main = runApp (app :: AppM 1 ())
```

Try running the different versions and see what you get!

```haskell

> runApp (app :: AppM 1 ())
Object (fromList [("name",String "Bob Johnson")])
> runApp (app :: AppM 2 ())
Object (fromList [("lastName",String "Johnson"),("firstName",String "Bob")])
```

That should do it! We can quickly and easily switch between versions of our app by changing the type annotation; if we
like we could even write some aliases to help out:

```haskell
appV1 :: AppM 1 ()
appV1 = app

appV2 :: AppM 2 ()
appV2 = app
```

Nice! Now we can write our app in such a way that it's **generic** and **polymorphic** over the **version** of user
when that part of the app doesn't care which version it is, but we can still **specialize** to a specific user version
when needed by using specific typeclasses or by pattern matching on the User constructor. The type system will
guarantee that we never accidentally switch between user versions in the middle of our app; and we can defer the choice
of version until the last possible second (at the top level call site). Sounds like a win to me! 

Hope you learned something!


## Bonus Section: Asserting Version Compatibility

If you take this pattern even further you might end up with multiple versions in your app; something like this:

```haskell
AppM (userVersion :: Nat) (postVersion :: Nat) a = AppM
```

This works fine of course, but as the number of version parameters grows it gets tough to keep track of which versions
are compatible with each other, maybe `userVersion == 2` is only compatible with a `postVersion >= 3`? Here's a fun
trick using `ConstraintKinds` and `TypeFamilies` to let us easily assert that our app is never run with incompatible
versions:


```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind

type family Compatible (userVersion :: Nat) (postVersion :: Nat) :: Constraint where
  Compatible 1 1 = ()
  Compatible 2 3 = ()
  Compatible 2 4 = ()
  Compatible a b = TypeError (Text "userVersion " :<>: ShowType a 
                         :<>: Text " is not compatible with postVersion " :<>: ShowType b)
```

You may need to dig into this a bit on your own to understand it fully, but the basic idea is that it's a function over
types which when given two versions will either result in an empty constraint (i.e. `()`) which will allow compilation
to continue, or will result in a failing `TypeError` and will print a nice error message to the user. You can use it
like this:

```haskell
runAppWithCheck :: Compatible userVersion postVersion => AppM userVersion postVersion a -> IO a
-- We only really need the additional type information, under the hood we can just call `runApp`
runAppWithCheck = runApp
```

Now if you try to run your app with incompatible versions you'll get a nice error something like:

```
error:
    • userVersion 2 is not compatible with postVersion 1
    • In the expression: runAppWithCheck (app :: AppM 2 1 ())
      In an equation for ‘main’:
          main = runAppWithCheck (app :: AppM 2 1 ())
   |
   | main = runAppWithCheck (app :: AppM 2 1 ())
```

Good stuff! You can even use `DataKinds` to add a little structure to your version numbers so you can't accidentally
mix up your userVersions with your postVersions, but I'll leave that for you to figure out 😉
