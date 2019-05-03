---
title: "Higher Kinded Option Parsing"
author: Chris Penner
date: Apr 8, 2019
tags: [haskell]
description: Collecting options for your application using Higher Kinded Data
image: postman/mail.jpg
---

Higher Kinded Data has piqued my interest lately as something that seems to
have a lot of potential applications, however the ergonomics of most of these
cases still need a little work. Today we'll look at one case where I think the 
end result ends up quite nice! Let's parse some options!

First the problem; if you've worked on a non-trivial production app you've
probably come across what I'll call the **Configuration Conundrum**.
Specifically, this is when an app collects configuration values from *many
sources*. To enumerate some possibilities, maybe it parses some options from
CLI flags, some from Environment Variables, yet more come from a configuration
file in JSON or YAML or TOML or whatever, or even worse it may come from
SEVERAL files. Working with these is a mess, option priority gets tricky, providing
useful error messages gets even harder, and the code for managing all these sources
is confusing, complicated, and spread out. Though we may not solve ALL these problems
today we'll build an approach that's modular and extensible enough that you can
mix and match bits and pieces to get whatever you need.

Here's a peek at how our configuration parsing code will end up:

```haskell
getOptions :: IO Options
getOptions = do
    configJson <- readConfigFile
    mOpts <- fold [cliOpts, envOpts, pure (jsonOpts configJson)]
    return $ mOpts `orDefault` defaultOpts
```

## Options of the high kind

If you're unfamiliar with HKD (higher kinded data); it's a very simple idea
with some mind bending implications. Many data types are parameterized by a
value type (e.g. `[a]` is a list is parameterized by the types of values it
contains); however HKD types are parameterized by some **wrapper** (typically a
functor, but not always) around the data of the record. Easiest to just show
an example and see it in practice. Let's define a very simple type to contain
all the options our app needs:

```haskell
data Options_ f =
    Options_
    { serverHost :: f String
    , numThreads :: f Int
    , verbosity  :: f Int
    }
```

Notice that each field is *wrapped* in `f`. I use a `_` suffix as a convention
to denote that it's an HKD type. `f` could be anything at all of the
kind `Type -> Type`; e.g. `Maybe`, `IO`, `Either String`, or even strange
constructions like `Compose Maybe (Join (Biff (,) IO Reader))` ! You'll
discover the implications as we go along, so don't worry if you don't get it
yet. For our first example we'll describe how to get options from Environment
Variables!

## Getting Environment Variables

Applications will often set configuration via Environment Variables; it's an
easy way to implicitly pass information into programs and is nice for sensitive
data like secrets. Let's build up our Options object from any Environment
Variables which may or may not exist. First we'll need a way to lookup an option
for a given key, and a way to convert it into the type we expect. You may want to
use something more sophisticated in your app; but for the blog post I'll just
lean on the `Read` typeclass to make a small helper.

```haskell
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

lookupEnv' :: Read a => String -> IO (Maybe a)
lookupEnv' envKey = do
    lookupEnv envKey >>= pure . \case
        Just x -> readMaybe x
        Nothing -> Nothing
```

This function looks up the given key in the System's environment, returning a
`Nothing` if it doesn't exist or fails to parse. We'll talk about more
civilized error handling later on, don't worry ;)

Now we can describe how to get each option in our type using this construct:

```haskell
-- This doesn't work!
envOpts :: Options_ ??
envOpts = 
    OptionsF
    { serverHost = lookupEnv' "SERVER_HOST"
    , numThreads = lookupEnv' "NUM_THREADS"
    , verbosity  = pure Nothing -- Don't set verbosity from environment
    }
```

Close; but if you'll note earlier, each field should contain the TYPE of the field
wrapped in something of kind `Type -> Type`. Here we've got `IO (Maybe a)` which
doesn't quite line up; but we can employ `Compose` here to collect both `IO` and `Maybe`
into a single Higher Kinded Type to serve as our `f`. Try the following instead:

```haskell
import Data.Functor.Compose (Compose(..))

lookupEnv' :: Read a => String -> (IO `Compose` Maybe) a
lookupEnv' envKey = Compose $ do
    ...

envOpts :: Options_ (IO `Compose` Maybe)
envOpts = 
    OptionsF
    { serverHost = lookupEnv' "SERVER_HOST"
    , numThreads = lookupEnv' "NUM_THREADS"
    , verbosity  = Compose . pure $ Nothing -- Don't set verbosity from environment
    }
```

I personally find it more readable to write `Compose` as infix, but suit
yourself :) Very cool; we've got a data type where each field represents a way
to get its value! It's clear at a glance that we haven't forgotton any fields
(if you have `-Wall` enabled you'd get a warning about missing fields!). We've
effectively turned the traditional `Options <$> ... <*> ...` design **inside out**.
It's much more declarative this way, and now that it's all collected as structured
data it'll be easier for us to work with from now on too!

Let's build another one!

## Parsing JSON/YAML Configs

JSON and YAML configuration files are getting pretty common these days. I
won't bother to dive into where to store them or how we'll parse them, let's assume
you've done that already and just pretend we've got ourselves an Aeson `Value` object
from some config file and we want to dump the values into our Options object.

We have two options here and I'll show both! The simplest is just to derive
`FromJSON` and cast the value directly into our type!

Unfortunately it's not quite so easy as just tacking on a `deriving FromJSON`;
Since GHC doesn't know what type `f` is, it has a tough time figuring out
how to derive FromJSON for all of the `f a` fields. No worries though; someone
thought of that! Time to pull in the [`barbies`](http://hackage.haskell.org/package/barbies)
library. An incredibly useful tool for working with HKD in general. Add the `barbies`
library to your project, then we'll derive a few handy instances:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
-- ...
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Data.Barbie
-- ...

-- Go back and add the deriving clause:
data Options_ f =
    Options_ {...}
    deriving (Generic, FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

deriving instance (AllBF Show f Options_) => Show (Options_ f)
deriving instance (AllBF Eq f Options_) => Eq (Options_ f)
deriving instance (AllBF A.FromJSON f Options_) => A.FromJSON (Options_ f)
```

Okay! Let's step through some of that. First we derive `Generic`, it's used by
both `aeson` and `barbies` for most of their typeclasses. We derive a bunch of
handy `xxxxB` helpers which also come from the `barbies` lib, we'll explain
them as we use them. The important part right now is the `deriving instance`
clauses. By using `AllBF` from `barbies` we assert that **all** the wrapped
fields of the typeclass adhere to some typeclass. For example,
`AllBF Show f Optoins_` says that `f a` is Showable for every field in our
product. More concretely, in our case `AllBF Show f Options` expands into
something equivalent to `(Show (f String), Show (f Int))` since we have fields
of type String and Int. Nifty! So we can now derive typeclasses with behaviour
dependent on the wrapping type. In most cases this works as expected, and can sometimes be really handy!

One example where this ends up being useful is `FromJSON`. The behaviour of our
`FromJSON` instance will depend on the wrapper type; if we choose `f ~ Maybe` then
all of our fields become optional! Let's use this behaviour to say that we want
to parse our JSON file into an `Optoins` object, but it's okay if fields are missing.

```haskell
import Control.Lens

jsonOptsDerived :: A.Value -> Options_ Maybe
jsonOptsDerived = fromResult . A.fromJSON
  where
    fromResult :: A.Result (Options_ Maybe) -> Options_ Maybe
    fromResult (A.Success a) = a
    fromResult (A.Error _) = buniq Nothing
```

A few things to point out here; we call
`fromJson :: FromJSON a => Value -> Result a` here, but we don't really need
the outer `Result` type; we'd prefer if the failure was localized at the
individual field level; simply using `Nothing` for fields which are missing. So
we use `fromResult` to unpack the result if successful, or to construct an
`Options_ Maybe` filled completely with `Nothing` if the parsing fails for some
reason (you'll probably want to come back an improve this error handling behaviour later).
You'll notice that nothing we do really has much to do with `Optoins_`; so let's
generalize this into a combinator we can re-use in the future:

```haskell
jsonOptsDerived :: (A.FromJSON (b Maybe), ProductB b) => A.Value -> b Maybe
jsonOptsDerived = fromResult . A.fromJSON
  where
    fromResult :: ProductB b => A.Result (b Maybe) -> b Maybe
    fromResult (A.Success a) = a
    fromResult (A.Error _) = buniq Nothing
```

`buniq` requires a `ProductB` constraint which asserts that the type we're
constructing is a Record type or some other Product since it wouldn't know
which constructor to instantiate if it were a Sum-type.


Okay, we've seen the generic version; here's a different approach where we can
choose HOW to deserialize each optoin from a given `Value`.

```haskell
import Data.Text.Lens
import Data.Aeson.Lens
import Control.Lens

jsonOptsCustom :: A.Value -> Options_ Maybe
jsonOptsCustom = bsequence
    Options_
    { serverHost = findField $ key "host"        . _String . unpacked
    , numThreads = findField $ key "num_threads" . _Number . to round
    , verbosity  = findField $ key "verbosity"   . _Number . to round
    }
      where
        findField :: Fold A.Value a -> Compose ((->) A.Value) Maybe a
        findField p = Compose (preview p)
```

Some of these types get a bit funky; but IMHO they wouldn't be too terrible
if this were all bundled up in some lib for readability. As I said earlier, some
of the ergonomics still have room for improvement.

Let's talk about what this thing does! First we import a bunch of lensy stuff;
then in each field of our options type we build a getter function from
`Value -> Maybe a` which tries to extract the field from the JSON Value.
`preview` happens to be a handy way to do this. I pulled out the `findField`
helper mainly to draw attention to the rather cryptic
`Compose ((->) A.Value) Maybe a` type. This is simply `Compose` wrapped around
a function `A.Value -> Maybe a`; why do we need the `Compose` here? 
What we REALLY want is `A.Value -> Option_ Maybe`; but since each field
has its own function and it's own path, in order to get the proper Kind in each
slot we must use `Compose`. Conveniently; `barbies` provides us with `bsequence`;
whose type looks like this:

```haskell
bsequence ::
  (Applicative f, TraversableB b) =>
  b (Compose f g) -> f (b g)
```

Or if we specialize it to this particular case:

```haskell
bsequence :: Options_ (Compose ((->) A.Value) Maybe) 
          -> A.Value -> Options_ Maybe
```

We use the `->` applicative (also known as `Reader`) to extract the function to
the outside of the structure! Hopefully this demonstrates the flexibility of
the technique, we can provide an arbitrary lens chain to extract the value for
each setting, maybe it's overkill in this case, but I can think of a few
situations where it would be pretty handy.

This is dragging on a bit; let's see how we can actually use these things!

## Combining Options Objects



