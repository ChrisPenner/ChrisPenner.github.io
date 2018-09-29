---
title: "Mocking Effects using Constraints and Phantom Data Kinds"
author: Chris Penner
date: Sep 3, 2018
tags: [programming, haskell, testing]
description: "We learn a method to mock multiple effects when writing tests without an exponential explosion of instances."
image: update-monad/change.jpg
---

This ended up being a pretty long post; if you're pretty comfortable with monad constraints and testing in Haskell
you may want to jump down to the Phantom Data Kinds section and get to the interesting stuff!

## Refresher on granular classes

I've been seeing a lot of talk on the [Haskell
subreddit](https://reddit.com/r/haskell) about how to properly test Haskell
applications; particular how to test actions which require effects. I've seen a
lot of confusion/concern about writing your own monad transformers in tests.
This post is my attempt to clear up some confusion and misconceptions and show
off my particular ideas for testing using `mtl-style` constraints. The ideas contained
here can also help you with writing multiple 'interpreters' for your monad stacks without
needing a `newtype` for each permutation of possible implementations.

First things first, what do I mean by `mtl-style` constraints? I'd recommend you
consult [MonadIO Considered
Harmful](https://chrispenner.ca/posts/monadio-considered-harmful), it's a post
I wrote on the topic almost exactly a year ago. Here's the spark-notes version:

-   Monads with semantics attached should define a `Monad*` type-class for
    interacting with those constraints (E.g. `MonadState`, `MonadReader`)
-   Actions which require effects should use type-class constraints instead of
    using concrete monads (E.g. `myAction :: MonadReader AppEnv m => m ()`
    rather than `myAction :: AppM ()`)
-   Your app should break up 'big' monad classes into smaller ones with clearer
    semantics and intent. (E.g. Break down `MonadIO` into `MonadHttp` and
    `MonadFilesystem`, etc.)


Okay, so assuming we're all on board with writing our code polymorphically
using Monad Constraints, what's the problem? Well, the reason we're doing it
polymorphically is so we can specialize the monad to **different
implementations** if we want! This is one way to implement the [**dependency
injection**](https://en.wikipedia.org/wiki/Dependency_injection) pattern in
Haskell; and lets us substitute out the implementation of our monadic effects
with 'dummy' or 'mock' versions in tests.

The trick is that we run into a lot of annoying repetition and boiler-plate which gets out of control as we scale up
the number of effects we use. To show the problem let's assume we have some action that does something, and needs the
following three constraints which you can assume are type-classes we've defined using the 'granular mtl' style:

```haskell
myAction :: (MonadFileSystem m, MonadDB m, MonadLogger m) => m ()
```

Now, assume we've already implemented `MonadFileSystem`, `MonadDB`, and `MonadLogger` for our application's main
monad, but when we test it we probably don't want to hit our real DB or file-system so we should probably mock those
out. We'll need a new monad type to implement instances against:

```haskell
data TestState = TestState 
    { fakeFilesystem :: Map String String
    , fakeDB :: Map String String
    , logs :: [String]
    }

newtype TestM a = TestM (State TestState a)

instance MonadFileSystem TestM where
-- ...
instance MonadDB TestM where
-- ...
instance MonadLogger TestM where
-- ...
```

I'm not getting into many details yet and have elided the implementations here
for brevity, but hopefully that shows how you could implement those interfaces
in terms of some pure monad stack like `State` in order to more easily write
tests. BUT! What if for a new test we want the file-system to behave
differently and fail on every request to read a file? We could add a boolean
into the state that dictates this behaviour, but that will definitely
complicate the implementation of our instance, we could add a newtype wrapper
which has a different `MonadFileSystem` instance, but we'd need to regain all
our instances for the other type-classes again! We can use
`GeneralizedNewtypeDeriving` to help, but say we now want multiple behaviours
for our MonadDB instance! Things get out of control really quickly, this post
investigates a (slightly) cleaner way to go about this.

Our goals are as follows:

- I want to write exactly 1 instance definition per type-class behaviour I want
- Adding a new effect or behaviour shouldn't require any newtypes.
- I should be able to easily choose a set of behaviours for each of my effects each time I run a test.

That's a tall order! Let's dig in and see if we can manage it!

Case Study
----------

This topic is tough to explain without concrete examples, so bear with me while
we set some things up. Let's start by looking at how someone may have written a
really simple app and some functions for working with their database.

Here's our base monad type:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Except
import Control.Monad.IO.Class

data DBError =
  DBError String
  deriving (Eq, Show)

-- Our application can access the database via IO and possibly throw DB errors.
newtype AppM a = AppM
  { runAppM :: ExceptT DBError IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError DBError)

```

We've abstracted over our database actions already with the following type-class:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
type Key = String

-- Our database associates string keys to some value type.
-- The specific value we can fetch is dependent on the monad
-- (but we'll just use strings for simplicity in our case)
class (MonadError DBError m) => MonadDB m v | m -> v where
  getEntity :: Key -> m v
  storeEntity :: Key -> v -> m ()

instance MonadDB AppM String where
-- ...
-- Assume we've written some instance for interacting with our DB via IO, 
-- which returns any errors via ExceptT.
```

Cool! This looks pretty normal, we have a primary app monad and we can get and
store strings in our database via IO using it!

Now that we've got our basic DB interface let's say we want to write a more
complex action using it:

```haskell
-- given a database key and a monad which can interact with a database containing strings
-- we can look up the value, uppercase it, then write it back.
upperCase :: (MonadDB m String) => Key -> m ()
upperCase key = do
  thing <- getEntity key
  storeEntity key (fmap toUpper thing)
```

It's a pretty simple action, but we should probably add some unit tests! Let's set it up using our AppM instance!

```haskell
-- Spec.hs
main :: IO ()
main =
  hspec $ do
    describe "upperCase" $ do
      it "uppercases the value stored at the given key in the DB" $ do
        result <-
          -- we unpack AppM, run the exceptT, then lift it into the IO part of our spec
          liftIO . runExceptT . runAppM $ do
            storeEntity "my-key" "value"
            upperCase "my-key"
            getEntity "my-key"
        result `shouldBe` Right "VALUE"
```

Well, this should work, but we're using `IO` directly in tests; not only is
this going to be slow; but it means we need to have a database running
somewhere, and that the tests might pass or fail depending on the initial state
of that database! Clearly that's not ideal! We really only want to test the
semantics of `upperCase` and how it **glues together** the interface of our
database; we don't really care which database it's operating over.

Our `uppercase` action is polymorphic over the monad it uses, so that means we
can write a new instance for `MonadDB` and get it to use that in the tests
instead!

```haskell
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
using `State` instead of `IO`! This means we have fewer dependencies, fewer
unknowns, and can more directly test the behaviour of the action which we
actually care about.

Here's the re-written spec, the test itself is the same, but we no longer run
it in IO:

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
overtime, so let's see if we can knock down the boilerplate by an order of
magnitude. It's getting tough to talk about this abstractly so let's expand our
example to include at least one other mocked service. We'll add some capability
to our AppM to handle input and output from the console!

```haskell
class MonadCli m where
  -- 'print' something to output
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


runTestM' :: TestState -> TestM a -> Either DBError a
runTestM' startState (TestM m) = flip evalState startState . runExceptT $ m

instance MonadDB TestM String where
-- Implementation here's not important, assume we do pretty much the same thing as earlier

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

-- This DB mock tests how actions react if every call to the DB fails
instance MonadDB (TestM DBOnFire cli) String where
  getEntity _ = throwError . DBError $ "🔥 DB is on FIRE! 🔥"
  storeEntity _ _ = throwError . DBError $ "🔥 DB is on FIRE! 🔥"

-- A simple cli mock which pulls input from a list and 
-- stores printed strings in state for later inspection
instance MonadCli (TestM db CliUseList) where
  say msg = cliOutput %= (msg :)
  listen = do
    inputs <- use cliInput
    case inputs of
      [] -> return "NO MORE INPUT"
      (msg:rest) -> cliInput .= rest >> return msg

-- A simple cli mock which always returns the same thing
instance MonadCli (TestM db CliStatic) where
  say _ = return ()
  listen = return "INPUT"
```

The cool thing about this is that each instance can choose an instance based on
one parameter while leaving the type variable in the other slots unspecified! So for
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

Special thanks to Sandy Maguire A.K.A. isovector for proofreading and helping me vet my ideas!

If you have questions or comments hit me up on
[Twitter](https://twitter.com/chrislpenner) or
[Reddit](https://www.reddit.com/user/ChrisPenner)!

-------





```haskell
import Data.Char

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

