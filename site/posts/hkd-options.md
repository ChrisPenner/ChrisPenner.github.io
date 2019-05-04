---
title: "Higher Kinded Option Parsing"
author: Chris Penner
date: May 4, 2019
tags: [haskell]
description: Collecting options for your application from multiple sources using Higher Kinded Data
image: hkd-option-parsing/tools.jpg
---

Higher Kinded Data Types (HKDTs) have piqued my interest lately. They seem to
have a lot of potential applications, however the ergonomics still aren't so
great in most of these cases. Today we'll look at one case where I think the
end result ends up quite nice! Let's parse some options!

First the problem; if you've worked on a non-trivial production app you've
probably come across what I'll call the **Configuration Conundrum**.
Specifically, this is when an app collects configuration values from *many
sources*. Maybe it parses some options from CLI flags, some from Environment
Variables, yet more come from a configuration file in JSON or YAML or TOML or
or even worse it may pull from SEVERAL files. Working with these is a mess,
option priority gets tricky, providing useful error messages gets even harder,
and the code for managing all these sources is confusing, complicated, and
spread out. Though we may not solve ALL these problems today, we'll build an
approach that's modular and extensible enough that you can mix and match bits
and pieces to get whatever you need.

Here's an example of some messy code which pulls options from the environment
or from a JSON value file:

```haskell
getOptions :: IO Options
getOptions = do
    configJson <- readConfigFile
    mServerHostEnv <- readServerHostEnv
    mNumThreadsEnv <- readNumThreadsEnv
    let mServerHostJson = configJson ^? key "server_host" . _String . unpacked
    let mNumThreadsJson = configJson ^? key "num_threads" . _Number . to round
    return $ Options <$> fromMaybe serverHostDef (mServerHostEnv <|> mServerHostJson)
                     <*> fromMaybe numThreadsDef (mNumThreadsEnv <|> mNumThreadsJson)
```


Here's a peek at how our configuration parsing code will end up:

```haskell
getOptions :: IO Options
getOptions =
  withDefaults defaultOpts <$> fold [envOpts, jsonOptsCustom <$> readConfigFile]
```

This is slightly disingenuous as some of the logic is abstracted away behind the scenes
in the second example; but the point here is that the logic CAN be abstracted, whereas
it's very difficult to abstract over the steps in the first example without creating
a bunch of intermediate types.

## Kinds of Options

If you're unfamiliar with HKDTs (higher kinded data types); it's a very simple idea
with some mind bending implications. Many data types are parameterized by a
value type (e.g. `[a]` is a list is parameterized by the types of values it
contains); however HKDTs are parameterized by some **wrapper** type (typically a
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
to denote that it's a HKDT. `f` could be anything at all of the
kind `Type -> Type`; e.g. `Maybe`, `IO`, `Either String`, or even strange
constructions like `Compose Maybe (Join (Biff (,) IO Reader))` ! You'll
discover the implications as we go along, so don't worry if you don't get it
yet. For our first example we'll describe how to get options from Environment
Variables!

## Getting Environment Variables

Applications will often set configuration values using Environment Variables;
it's an easy way to implicitly pass information into programs and is nice for
sensitive data like secrets. We can describe the options in our HKDT in terms
of Environment Variables which may or may not exist. First we'll need a way to
lookup an option for a given key, and a way to convert it into the type we
expect. You may want to use something more sophisticated in your app; but for
the blog post I'll just lean on the `Read` typeclass to make a small helper.

```haskell
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)

readEnv :: Read a => String -> IO (Maybe a)
readEnv envKey = do
    lookupEnv envKey >>= pure . \case
        Just x -> readMaybe x
        Nothing -> Nothing
```

This function looks up the given key in the environment, returning a
`Nothing` if it doesn't exist or fails to parse. We'll talk about more
civilized error handling later on, don't worry ;)

Now we can describe how to get each option in our type using this construct:

```haskell
-- This doesn't work!
envOpts :: Options_ ??
envOpts =
    OptionsF
    { serverHost = readEnv "SERVER_HOST"
    , numThreads = readEnv "NUM_THREADS"
    , verbosity  = pure Nothing -- Don't read verbosity from environment
    }
```

Close; but if you'll note earlier, each field should contain field's underlying
type wrapped in some `f`. Here we've got `IO (Maybe a)`, we can't assign `f` to
`IO (Maybe _)` so we need to compose the two Functors somehow. We can employ
`Compose` here to collect both `IO` and `Maybe` into a single Higher Kinded
Type to serve as our `f`. Try the following instead:

```haskell
import Data.Functor.Compose (Compose(..))

-- Add a Compose to our helper
readEnv :: Read a => String -> (IO `Compose` Maybe) a
readEnv envKey = Compose $ do
    ...

envOpts :: Options_ (IO `Compose` Maybe)
envOpts =
    OptionsF
    { serverHost = readEnv "SERVER_HOST"
    , numThreads = readEnv "NUM_THREADS"
    , verbosity  = Compose $ pure Nothing -- Don't read verbosity from environment
    }
```

I personally find it more readable to write `Compose` as infix like this, but
some disagree. Very cool; we've got a version of our record where each field
contains an action which gets and parses the right value from the environment!
It's clear at a glance that we haven't forgotten to check any fields (if you
have `-Wall` enabled you'd get a warning about missing fields)! We've
effectively turned the traditional `Options <$> ... <*> ...` design **inside
out**. It's much more declarative this way, and now that it's all collected as
structured data it'll be easier for us to work with from now on too!

Let's build another one!

## Parsing JSON/YAML Configs

JSON and YAML configuration files are pretty common these days. I
won't bother to dive into where to store them or how we'll parse them, let's assume
you've done that already and just pretend we've got ourselves an Aeson `Value` object
from some config file and we want to dump the values into our options object.

We have two options here and I'll show both! The simplest is just to derive
`FromJSON` and cast the value directly into our type!

Unfortunately it's not quite so easy as just tacking on a `deriving FromJSON`;
Since GHC doesn't know what type `f` is it has a tough time figuring out what you
want it to do. If you try you'll get an error like:

```
• No instance for (FromJSON (f String))
```

We need to help out GHC a bit. No worries though; someone thought of that! Time
to pull in the [`barbies`](http://hackage.haskell.org/package/barbies) library.
An incredibly useful tool for working with HKDT in general. Add the `barbies`
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
both `aeson` and `barbies` for most of their type classes. We derive a bunch of
handy `*B` helpers (B for Barbies) which also come from the `barbies` lib,
we'll explain them as we use them. The important part right now is the
`deriving instance` clauses. `AllBF` from `barbies` asserts that **all** the
wrapped fields of the typeclass adhere to some type class. For example,
`AllBF Show f Options_` says that `f a` is Showable for every field in our
product. More concretely, in our case `AllBF Show f Options` expands into
something equivalent to `(Show (f String), Show (f Int))` since we have fields
of type `String` and `Int`. Nifty! So we can now derive type classes with
behaviour dependent on the wrapping type. In most cases this works as expected,
and can sometimes be really handy!

One example where this ends up being useful is `FromJSON`. The behaviour of our
`FromJSON` instance will depend on the wrapper type; if we choose `f ~ Maybe` then
all of our fields become optional! Let's use this behaviour to say that we want
to parse our JSON file into an `Options` object, but it's okay if fields are missing.

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
You'll notice that nothing we do really has much to do with `Options_`; so let's
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
constructing is a Record type or some other Product. This is required because
`buniq` wouldn't know which constructor to instantiate if it were a Sum-type.

Okay, we've seen the generic version; here's a different approach where we can
choose HOW to deserialize each option from the provided `Value`.

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
`preview` mixed with `Data.Aeson.Lens` happens to be a handy way to do this. I
pulled out the `findField` helper mainly to draw attention to the rather
cryptic `Compose ((->) A.Value) Maybe a` type signature. This is just `Compose`
wrapped around a function `A.Value -> Maybe a`; why do we need the `Compose`
here? What we REALLY want is `A.Value -> Option_ Maybe`; but remember that
every field MUST contain something that matches `f a` for some `f`. A function
signature like `A.Value -> Maybe a` doesn't match this form, but
`Compose ((->) A.Value) Maybe a` does (where
`f ~ Compose ((->) A.Value) Maybe`)! Ideally we'd then like to pull the
function bits to the outside since we'll be calling each field with the same
argument. Conveniently; `barbies` provides us with `bsequence`; whose type
looks like this:

```haskell
bsequence ::
  (Applicative f, TraversableB b) =>
  b (Compose f g) -> f (b g)
```

Or if we specialize it to this particular case:

```haskell
bsequence :: Options_ (Compose ((->) A.Value) Maybe)
          -> (A.Value -> Options_ Maybe)
```

We use the `->` applicative (also known as `Reader`) to extract the function
Functor to the outside of the structure! This of course requires that the
individual fields can be traversed; implying the `TraversableB` constraint.
Hopefully this demonstrates the flexibility of this technique, we can
provide an arbitrary lens chain to extract the value for each setting, maybe
it's overkill in this case, but I can think of a few situations where it would
be pretty handy.

While we're at it, let's `bsequence` our earlier Environment Variable object to
get the IO on the outside!

```haskell
envOpts :: IO (Options_ Maybe)
envOpts = bsequence
    Options_
    -- serverHost is already a string so we don't need to 'read' it.
    { serverHost = Compose . lookupEnv $ "SERVER_HOST"
    , numThreads = readEnv "NUM_THREADS"
    -- We can 'ignore' a field by simply returning Nothing.
    , verbosity    = Compose . pure $ Nothing
    }
```

This is dragging on a bit; let's see how we can actually use these things!

## Combining Options Objects

Now that we've got two different ways to collect options for our program let's
see how we can combine them. Let's write a simple action in IO for getting
and combining our options parsers.

```haskell
import Control.Applicative

-- Fake config file for convenience
readConfigFile :: IO A.Value
readConfigFile =
    pure $ A.object [ "host" A..= A.String "example.com"
                    , "verbosity" A..= A.Number 42
                    ]

getOptions :: IO (Options_ Maybe)
getOptions = do
    configJson <- readConfigFile
    envOpts' <- envOpts
    return $ bzipWith (<|>) envOpts' (jsonOptsCustom configJson)
```

Let's try it out, then I'll explain it.

```haskell
λ> getOptions
Options_ {serverHost = Just "example.com", numThreads = Nothing, verbosity = Just 42}

λ> setEnv "NUM_THREADS" "1337"
λ> getOptions
Options_ {serverHost = Just "example.com", numThreads = Just 1337, verbosity = Just 42}

-- We've set things up so that environment variables override our JSON config.
λ> setEnv "SERVER_HOST" "chrispenner.ca"
λ> getOptions
Options_ {serverHost = Just "chrispenner.ca", numThreads = Just 1337, verbosity = Just 42}
```

Now that we're combining sets of config values we need to decide what semantics
we want when values overlap! I've decided to use `<|>` from `Alternative` to
combine our `Maybe` values. This basically means "take the first non-Nothing
value and ignore the rest". That means in our case that the first setting to be
"set" wins out. `bzipWith` performs element-wise zipping of each element within
our `Options_` record, with the caveat that the function you give it must work
over any possible `a` contained inside. In our case the type is specialized to:

```haskell
bzipWith  :: (forall a. Maybe a -> Maybe a -> Maybe a)
          -> Options_ Maybe -> Options_ Maybe -> Options_ Maybe
```

Which does what we want. This end bit is going to get messy if we add any more
sources though, let's see if we can't clean it up! My first thought is that I'd
love to use `<|>` without any lifting/zipping, but the kinds don't line up;
`Options_` is kind `(Type -> Type) -> Type` whereas `Type -> Type` is required
by `Alternative`. How do we lift Alternative to Higher Kinds? Well we could try
something clever, **OR** we could go the opposite direction and use
`Alternative` to build a `Monoid` instance for our type; then use that to
combine our values!

```haskell
instance (Alternative f) => Semigroup (Options_ f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (Options_ f) where
  mempty = buniq empty
```

Now we have a Monoid for `Options_` whenever `f` is an `Alternative` such as
`Maybe`! There are many possible `Monoid` instance for HKDTs, but in our case
it works great! `Alternative` is actually just a Monoid in the category of
Applicative Functors, so it makes sense that it makes a suitable Monoid if we
apply it to values within an HKDT.

Let's see how we can refactor things.

```haskell
getOptions :: IO (Options_ Maybe)
getOptions = envOpts <> (jsonOptsCustom <$> readConfigFile)
```

Wait a minute; what about the `IO`? Here I'm actually employing `IO`s little
known Monoid instance! `IO` is a Monoid whenever the result of the `IO` is a
Monoid; it simply runs both `IO` actions then `mappends` the results
(e.g. `liftA2 (<>)`). In this case it's perfect! As we get even more possible
option parsers we could even just put them in a list and `fold` them together:
`fold [envOpts, jsonOptsCustom <$> readConfigFile, ...]`

But wait! There's more!

## Setting Default Values

We've seen how we can specify multiple **partial** configuration sources, but
at the end of the day we're still left with an `Options_ Maybe`! What if we
want to guarantee that we have a value for all required config values? Let's
write a new helper.

```haskell
withDefaults :: ProductB b => b Identity -> b Maybe -> b Identity
withDefaults = bzipWith fromMaybeI
  where
    fromMaybeI :: Identity a -> Maybe a -> Identity a
    fromMaybeI (Identity a) Nothing  = Identity a
    fromMaybeI _            (Just a) = Identity a
```

This new helper uses our old friend `bzipWith` to lift a slightly altered
`fromMaybe` to run over HKDTs! We have to do a little bit of annoying
wrapping/unwrapping of Identity, but it's not too bad. This function will take
the config value from any `Just`'s in our `Options_ Maybe` and will choose the
default for the `Nothing`s!

```haskell
import Data.Foldable
---
type Options = Options_ Identity

getOptions :: IO Options
getOptions =
  withDefaults defaultOpts <$> fold [envOpts, jsonOptsCustom <$> readConfigFile]
```

We introduce the alias `type Options = Options_ Identity` as a convenience.

## Better Errors

So far our system silently fails in a lot of places. Let's see how HKDTs can
give us more expressive error handling!

The first cool thing is that we can store error messages directly alongside
fields they pertain to!

```haskell
{-# LANGUAGE OverloadedStrings #-}
---
optErrors :: Options_ (Const String)
optErrors =
    Options_
    { serverHost = "server host required but not provided"
    , numThreads = "num threads required but not provided"
    , verbosity  = "verbosity required but not provided"
    }

```

If we use `Const String` in our HKDT we are saying that we actually don't care
about the type of the field itself, we just want to store a string no matter
what! If we turn on `OverloadedStrings` we can even leave out the `Const`
constructor if we like! But I'll leave that choice up to you.

Now that we've got errors which relate to each field we can construct a helpful
error message if we're missing required fields:

```haskell
import Data.Either.Validation
---
validateOptions :: (TraversableB b, ProductB b)
                => b (Const String)
                -> b Maybe
                -> Validation [String] (b Identity)
validateOptions errMsgs mOpts = bsequence' $ bzipWith validate mOpts errMsgs
  where
    validate :: Maybe a -> Const String a -> Validation [String] a
    validate (Just x) _          = Success x
    validate Nothing (Const err) = Failure [err]

```

`validateOptions` takes any traversable product HKDT with `Maybe` fields and an
HKDT filled with error messages inside `Const` and will return a `Validation`
object containing either a summary of errors or a validated `Identity` HKDT.
Just as before we use `bzipWith` with a function which operates at the level of
functors as a Natural Transformation; i.e. it cares only about the containers,
not the values. Note that Validation is very similar to the `Either` type, but
accumulates all available errors rather than failing fast. We use `bsequence'`
here, which is just like `bsequence`; but saves us the trouble of explicitly
threading an `Identity` into our structure. Check the docs in `barbies` if
you'd like to learn more.


```haskell
getOptions :: IO (Validation [String] Options)
getOptions =
  validateOptions optErrors <$> fold [envOpts, jsonOptsCustom <$> readConfigFile]
```

Now if we end up with values missing we get a list of errors!

```haskell
λ> getOptions
Failure ["num threads required but not provided"]
```

You can trust me that if we had more than one thing missing it would collect
them all. That's a lot of content all at once, so I'll leave some other
experiments for next time. Once you start to experiment a world of opportunities
opens up; you can describe validation, forms, documentations, schemas, and a bunch of other stuff I haven't even
though of yet!! A challenge for the reader: try writing a proper validator
using HKDTs which validates that each field fulfills specific properties. For
example, check that the number of threads is > 0; check that the host is non-empty,
etc. You may find the following newtype helpful ;)
`newtype Checker a = Checker (a -> Maybe String)`

## Bonus: Parsing CLI Options

Just for fun here's a bonus config source for getting options from the Command Line
using Optparse Applicative

```haskell
import Options.Applicative hiding (Failure, Success)
---
cliOptsParser :: Options_ Parser
cliOptsParser =
    Options_
    { serverHost =
          strOption (long "serverHost" <> metavar "HOST" <> help "host for API interactions")
    , numThreads =
          option auto
                 (long "threads" <> short 't' <> help "number of threads" <> metavar "INT")
    , verbosity  = option auto
                          (long "verbosity"
                           <> short 'v'
                           <> help "Level of verbosity"
                           <> metavar "VERBOSITY")
    }


mkOptional :: FunctorB b => b Parser -> b (Parser `Compose` Maybe)
mkOptional = bmap (Compose . optional)

toParserInfo :: (TraversableB b) => b (Parser `Compose` Maybe) -> ParserInfo (b Maybe)
toParserInfo b = info (bsequence b) briefDesc

cliOpts :: IO (Options_ Maybe)
cliOpts = execParser $ toParserInfo (mkOptional cliOptsParser)

getOptions :: IO (Validation [String] Options)
getOptions =
  validateOptions optErrors <$> fold [cliOpts, envOpts, jsonOptsCustom <$> readConfigFile]
```
