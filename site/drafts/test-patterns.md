---
title: "Mocking Effects using Constraints"
author: Chris Penner
date: Sep 3, 2018
tags: [programming, haskell, testing]
description: ""
image: update-monad/change.jpg
---

## Refresher on granular classes

I've been seeing a lot of talk on the [Haskell
subreddit](https://reddit.com/r/haskell) about how to properly test haskell
applications; particular haw to test actions which require effects. I've seen a
lot of confusion/concern about writing your own monad transformers in tests.
This post is my attempt to clear up some confusion and misconceptions and show
off my particular ideas for testing using `mtl-style` constraints.

First things first, what do I mean by `mtl-style` constraints? I'd recommend you
consult [MonadIO Considered
Harmful](https://chrispenner.ca/posts/monadio-considered-harmful), it's a post
I wrote on the topic almost exactly a year ago. Here's the spark-notes version:

-   Monads with semantics attached should define a `Monad*` typeclass for
    interacting with those constraints (E.g. `MonadState`, `MonadReader`)
-   Actions which require effects should use typeclass constraints instead of
    using concrete monads (E.g. `myAction :: MonadReader AppEnv m => m ()`
    rather than `myAction :: AppM ()`)
-   Your app should break up 'big' monad classes into smaller ones with clearer
    semantics and intent. (E.g. Break down `MonadIO` into `MonadHttp` and
    `MonadFilesystem`, etc.)

Okay; hopefully you get the idea! This post is going to build on that point and we'll explore
how to employ this constraint-based design for testing! 

Let's start by looking at how someone may have written a really simple app and some functions
for working with their database.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Other where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Char

data DBError =
  DBError String
  deriving (Eq, Show)

newtype AppM a = AppM
  { runAppM :: ExceptT DBError IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError DBError)

type Key = String

-- Fake DB ----------------------------
-- We'll simplify and say our database just contains strings for now
getByKeyIO :: Key -> IO String
getByKeyIO = undefined

storeByKeyIO :: Key -> String -> IO Result
storeByKeyIO = undefined

data Result
  = Success
  | Fail String
---------------------------------------

getEntity :: Key -> AppM String
getEntity key = liftIO $ getByKeyIO key

-- Our fictional database returns either Success or Fail from our 'store' database call
-- We just convert it to a DB error and throw it.
storeEntity :: Key -> String -> AppM ()
storeEntity key value = do
  result <- liftIO $ storeByKeyIO key value
  case result of
    Success -> return ()
    Fail msg -> throwError $ DBError msg

-- Get a string by key, uppercase it, then store it back.
upperCase :: Key -> AppM ()
upperCase key = do
  thing <- getEntity key
  storeEntity key (fmap toUpper thing)
```

Cool! This looks pretty normal, we have a primary app monad and we can get and
store strings in our database via IO using it!

Now let's say we want to unit test our `upperCase` action! Let's set it up:

```haskell
-- Spec.hs
import Control.Monad.Except
import Control.Monad.IO.Class
import Other
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "upperCase" $ do
      it "uppercases the value stored at the given key in the DB" $ do
        result <-
          liftIO . runExceptT . runAppM $ do
            storeEntity "my-key" "value"
            upperCase "my-key"
            getEntity "my-key"
        result `shouldBe` Right "VALUE"
```

Well, this should work (except for the fact that our DB is unimplemented); but
we're using `IO` directly in tests; not only is this going to be slow; but it
means we need to have a database running somewhere, and that the tests might
pass or fail depending on the contents of that database! Clearly that's not
ideal! We really only want to test the semantics of `upperCase` and how it
**glues together** the interface of our database; we don't really care which
database it's operating over. Let's see if we can use typeclasses to let us
swap out the database implementation in our tests!

Let's extract a typeclass for the semantics of working with our database:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
type Key = String
class MonadError DBError m => MonadDB m v | m -> v where
  getEntity :: Key -> m v
  storeEntity :: Key -> v -> m ()
```

Okay, so our database typeclass has two actions; we can store some value in the
DB by a string key and we can extract something from the DB using a key. I see
a lot of folks put a lot of extra constraints on their typeclasses that don't
really need to be there. For instance, many people will think "Oh! I need to
make network calls to read the database, I should add a MonadIO constraint to
the typeclass!". We have to ask "is this constraint actually part of the
semantics of the class?". In this case I'd say that `MonadDB` is concerned
about storing and getting values, and part of storing and getting values is the
fact that these operations might fail so I think a `MonadError` constraint is
reasonable, for greater composability we would want to be more generic in our
error type, but that's a topic for another whole blog post. When it comes to
whether we use IO or some pure solution to implement the methods that decision
is up to the implementor and really has nothing to do with the semantics of the
class. Leaving extra constraints off of the typeclass is less code, and more
flexible.

Okay, so we've got our class; in order to use it in `AppM` like we did before
we'll need an instance for it. This replaces our top level functions from earlier:

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

instance MonadDB AppM String where
  getEntity key = liftIO $ getByKeyIO key
  storeEntity key value = do
    result <- liftIO $ storeByKeyIO key value
    case result of
      Success -> return ()
      Fail msg -> throwError $ DBError msg

upperCase :: (MonadDB m String) => Key -> m ()
upperCase key = do
  thing <- getEntity key
  storeEntity key (fmap toUpper thing)

```

Notice how we defined a new `mtl-style` typeclass but actually **didn't** have
to define any sort of monad transformer? This is totally fine! Monad
transformers are typically used for **implementing** semantics in a concrete
way. Our `MonadDB` defines semantics; how you actually implement them is totally up to the
implementor of the instance, and since there are many possible MonadDB implementations, providing
a `DatabaseT` transformer probably won't help anyone.

Secondly, unlike `State`, `Reader`, `Writer` and friends this typeclass will
probably only ever be used **inside** our application, it's not part of an
external library. For our purposes it's sufficient to implement it concretely
against `AppM`, the only stack we really care about!

You'll notice that when we implement `MonadDB` against `AppM` we use `liftIO`
from `MonadIO` even though we don't have a `MonadIO` constraint on the
`MonadDB` typeclass. We know what our concrete monad is at instance time and
thus can use it without problems, specifying it as part of the typeclass would
gain us **nothing** and would **cost** us the ability to implement the
typeclass against non-IO monads! Best to leave off extraneous constraints from
your typeclasses!

Great! So this effectively gets us back to where we started, but our primitives
are bit more flexible now! We can use `upperCase` in ANY monad that knows how
to interact with a DB to store and fetch strings and which can handle possible
errors! The world is our oyster!

Let's jump back to our tests; now that we've abstracted the monad which `upperCase` uses
we can mock out the database implementation by making an instance of `MonadDB`!

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map as M

-- We'll use a Map as our database implementation, storing it in a state monad
-- We also add ExceptT so we can see if our DB failed to look something up!
newtype TestM a = TestM
  { runTestM :: ExceptT DBError (State (M.Map String String)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError DBError
             , MonadState (M.Map String String)
             )

-- We'll implement an instance of MonadDB for our TestM monad.
instance MonadDB TestM String where
  getEntity key = do
    db <- get
    case M.lookup key db of
      Nothing -> throwError . DBError $ "didn't find " ++ key ++ " in db"
      Just v -> return v
  storeEntity key value = modify (M.insert key value)

runTestM' :: M.Map String String -> TestM a -> Either DBError a
runTestM' db (TestM m) = flip evalState db . runExceptT $ m
```

Now we have a completely **pure** way of modeling our DB, which we can seed
with initial data, and we can even inspect the final state if we like! This
makes writing tests *so* much easier. We can re-write the `upperCase` test
without any IO! This means we have fewer dependencies, fewer unknowns, and can
more directly test the behaviour of the action which we actually care about.

```haskell
main :: IO ()
main =
  hspec $ do
    describe "upperCase" $ do
      it "uppercases the value stored at the given key in the DB" $ do
        let result =
              runTestM' mempty $ do
                storeEntity "my-key" "value"
                upperCase "my-key"
                getEntity "my-key"
        result `shouldBe` Right "VALUE"
```

Nifty!

## Parameterizing test implementations

The thing about **test**s is that you often want to **test** unique and
interesting behaviour! This means we'll probably want multiple implementations
of our mocked services which each behave differently. Let's say that we want to
test what happens if our DB fails on every single call? We **could** implement
a whole new `TestM` monad with a new instance for `MonadDB` which errors on
every call, and this would work fine, but in the real world we'll probably be
mocking out a half-dozen services or more! That means we'll need a half dozen
instances for each and every `TestM` we build! I don't feel like working
overtime, so let's see if we can come up with something cleaner. It's getting
tough to talk about this abstractly so let's expand our example to include at
least one other mocked service. We'll add some capability to our AppM to handle
input and output from the console!

```haskell
class MonadCli m
  -- 'print' something to output
  where
  say :: String -> m ()
  -- get a string from the user input
  listen :: m String

instance MonadCli AppM where
  say = liftIO . print
  listen = liftIO getLine
```

Now we can get something from the user and store it in the DB!

```haskell
storeName :: (MonadDB m String, MonadCli m) => m ()
storeName = do
  say "What's your name?"
  name <- listen
  storeEntity "name" name
```

Let's jump into testing it! To do so we'll need to make `TestM` an instance of
`MonadCli` too! Now that we have multiple concerns going on I'm going to use a
shared state and add some lenses to make working with everything a bit easier.
It's a bit of set-up up-front, but from now on adding additional functionality
should be pretty straight-forward!

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

-- We'll store all our mock data in this data type
data TestState = TestState
  { _cliInput :: [String]
  , _cliOutput :: [String]
  , _db :: Map String String
  } deriving (Show, Eq)

makeLenses ''TestState

newtype TestM a = TestM
  { runTestM :: ExceptT DBError (State TestState) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError DBError
             , MonadState TestState
             )


-- We'll use some lenses to help out here; if you're not familiar with lenses
-- don't worry too much, we're doing basically the same thing we did before,
-- just with a bit of nesting ;)
-- We can actually probably do most of it as a one-liner, but let's not get
-- carried away, this isn't a post on lens :3
instance MonadDB TestM String where
  getEntity key = do
    db' <- use db
    case M.lookup key db' of
      Nothing -> throwError . DBError $ "didn't find " ++ key ++ " in db"
      Just v -> return v
  storeEntity key value = db %= M.insert key value

instance MonadCli TestM where
  -- We'll just record things we say in our list of output
  say msg = cliOutput %= (++ [msg])
  -- We'll pull input from our state as long as we have some.
  listen = do
    inputs <- use cliInput
    case inputs of
      [] -> return "NO MORE INPUT"
      (msg:rest) -> cliInput .= rest >> return msg

emptyState :: TestState
emptyState = TestState {_cliInput = mempty, _cliOutput = mempty, _db = mempty}

runTestM' :: TestState -> TestM a -> Either DBError a
runTestM' startState (TestM m) = flip evalState startState . runExceptT $ m
```

Now we can test our `storeName` function!

```haskell
main :: IO ()
main =
  hspec $ do
    describe "storeName" $ do
      it "stores a name from user input" $ do
        let result =
              -- We'll seed a name as our cli input
              runTestM' (emptyState & cliInput .~ ["Steven"]) $
              -- Running storeName should store the cli input 
              -- in the DB under the "name" key!
              storeName >> getEntity "name"
        result `shouldBe` Right "Steven"
```

Hopefully that comes out green!

Great! So, like I said before we'd like to customize our implementations of
some of our mocked out services, let's say we want the DB to fail on every
call! One option would be to wrap `TestM` in a newtype and use
`deriving MonadCli` with `GeneralizedNewtypeDeriving` to get back our
implementation of `MonadCli` then write a NEW instance for `MonadDB` which
fails on every call. If we have to do this for every customized behaviour for
each of our services though this results in an `(n*k)` number of newtypes! We
need a different newtype for EACH pairing of every possible set of behaviours
we can imagine! Let's solve this problem the way we solve all problems in
Haskell: Add more type parameters!

## Phantom Data Kinds

Let's parameterize `TestM` with slots which represent possible implementations
of each service. To help users know how it works and also prevent incorrect
usage we'll qualify the parameters using `DataKinds`!


```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data DBImpl
  = DBUseMap
  | DBOnFire

data CliImpl
  = CliUseList
  | CliStatic

newtype TestM (db :: DBImpl) (cli :: CliImpl) a = TestM
  { runTestM :: ExceptT DBError (State TestState) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError DBError
             , MonadState TestState
             )

runTestM' :: TestState -> TestM db cli a -> Either DBError a
runTestM' startState (TestM m) = flip evalState startState . runExceptT $ m
```

Notice that we don't actually need to use these params inside the definition of
the `TestM` newtype; they're just there as annotations for the compiler, I call
these 👻 "Phantom Data Kinds" 👻.. Now let's update the instance we've defined
already to handle the type params, as well as add some new instances!

The instance signatures are the most important part here; but read the rest if you like 🤷‍♂️

```haskell
instance MonadDB (TestM DBUseMap cli) String where
  getEntity key = do
    db' <- use db
    case M.lookup key db' of
      Nothing -> throwError . DBError $ "didn't find " ++ key ++ " in db"
      Just v -> return v
  storeEntity key value = db %= M.insert key value

instance MonadDB (TestM DBOnFire cli) String where
  getEntity _ = throwError . DBError $ "🔥 DB is on FIRE! 🔥"
  storeEntity _ _ = throwError . DBError $ "🔥 DB is on FIRE! 🔥"

instance MonadCli (TestM db CliUseList) where
  say msg = cliOutput %= (msg :)
  listen = do
    inputs <- use cliInput
    case inputs of
      [] -> return "NO MORE INPUT"
      (msg:rest) -> cliInput .= rest >> return msg

instance MonadCli (TestM db CliStatic) where
  say _ = return ()
  listen = return "INPUT"
```

The cool thing about this is that each instance can choose an instance based on
one parameter while leaving an open type variable in the other slots! So for
our MonadDB implementation we can have a different implementation for each
value of the `db :: DBImpl` type param while not caring at all what's in the
`cli :: CliImpl` parameter! This means that we only need to implement each
behaviour once, and we can mix and match implementations for different services
at will! We *do* need to make sure that there's some way to actually implement
that behaviour against our `TestM`; but for the vast majority of cases you can
just carve out a spot in the `State` to keep track of what you need for your
mock. Using lenses means that adding something new won't affect existing
implementations.

Whoops; just about forgot, we need a way to pick which behaviour we want when
we're actually running our tests! `TypeApplications` are a huge help here! We
use `TypeApplications` to pick which `DBImpl` and `CliImpl` we want so that GHC
doesn't get mad at us about ambiguous type variables. Use them like this:

```haskell
{-# LANGUAGE TypeApplications #-}

runTestM' @DBUseMap @CliUseList
            (emptyState & cliInput .~ ["Steven"]) $
            storeName >> getEntity "name"
```

Now you can pretty easily keep a separate module where you define `TestM` and
all of its behaviours and instances, then just use Type Applications to
specialize your test monad when you run it to get the behaviour you want!

And that wraps up our dive into testing using `mtl-style` constraints! Thanks for joining me!

If you have questions or comments hit me up on
[Twitter](https://twitter.com/chrislpenner) or
[Reddit](https://www.reddit.com/user/ChrisPenner)!
