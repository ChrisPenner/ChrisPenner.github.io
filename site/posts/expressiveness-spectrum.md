---
title: "Monads are too powerful: The Expressiveness Spectrum"
author: Chris Penner
date: Sep 24, 2025
tags: [programming, haskell]
description: "Monads are a useful tool, but what costs do we pay for their expressive power?"
image: power-small.jpg
---

Okay, so you and I both know monads are great, they allow us to sequence effects
in a structured way and are in many ways a super-power in the functional-programming toolkit.
It's likely none of us would have even heard of Haskell without them.

It's my opinion, though, that monads are actually _too_ powerful for their own good. Or to be more clear, 
monads are more **expressive** than they need to be, and that we're paying hidden costs to gain expressive power that we rarely, if ever, actually use.

In this post we'll take a look at how different approaches to effects lie on the spectrum between expressiveness and strong static analysis, and how, just like 
Dynamic vs Statically typed programming languages, there's a benefit to limiting the number of programs you can write by adding more structure and constraints to your effects system.

## The Status Quo

A defining feature of the Monadic interface is that it allows the dynamic selection 
of effects based on **the results of previous effects**.

This is a huge boon, and is what allowed the construction of _real_ programs in Haskell without 
compromising on its goals of purity and laziness. 
This ability is what allows us to express normal programming workflows like fetching input from a user before 
deciding which command to run next, or fetching IDs from the database and then resolving those IDs with subsequent database calls.
This form of choice is necessary for writing most moderately complex programs.

Alas, as it turns out, this expressiveness isn't free! It exists on a spectrum. 
As anyone who's maintained any relatively complex JavaScript or Python codebase can tell you, the _ability_
to do anything at any time comes at a cost of readability, perhaps more relevant to the 
current discussion, at the cost of static analysis.

Allow me to present, in all its glory, the Expressiveness Spectrum:

```
Strong Static Analysis <+------------+------------+> Embarrassingly Expressive Code
```

As you can clearly see, as you gain more expressive power you begin to lose the ability to know what the heck your 
program could possibly do when it runs. 

This has fueled a good many debates among programming language connoisseurs, 
and it turns out that there's a similar version of the debate to be had within the realm of effect systems themselves.

In their essence, effect systems are just methods of expressing miniature programs
**within** your programming language of choice. These mini programs can be constructed,
analysed, and executed at runtime within the framework of the larger programming language, 
and the same Expressiveness Spectrum applies independently to them as well. That is, the more programs you 
allow your effect system to express, the less you can know about any individual program before you run it.

In the effect-system microcosm there are similar mini _compile time_ and _run time_ stages. 
As an example here's a simple Haskell program which constructs a chain of effects using a DSL:

```haskell
-- The common way to express effects in Haskell 
-- is with a Monadic typeclass interface.
class Monad m => ReadWrite m where
  readLine :: m String
  writeLine :: String -> m ()

-- We can write a little program builder which depends on 
-- input that may only be known at runtime.
greetUser :: ReadWrite m => String -> m () 
greetUser greeting = do
  writeLine (greeting <> ", what is your name?")
  name <- readLine
  writeLine ("Hello, " <> name <> "!")

-- We can, at run time, construct a new mini-program that the world has never seen before!
mkSimpleGreeting :: ReadWrite m => IO (m ())
mkSimpleGreeting = do 
  greeting <- readFile "greeting.txt"
  pure (greetUser greeting)
```

In this simplified example we clearly see that we can use our host languages features arbitrarily to construct a smaller 
program within our ReadWrite DSL. Our simple program here just reads a line of input from the user and then greets them by name.

This is all well and good in such a simple case, however if we expand our simple `ReadWrite` effect slightly by adding a new effect:

```haskell
class Monad m => ReadWriteDelete m where
  readLine :: m String
  writeLine :: String -> m ()
  deleteMyHardDrive :: m ()
```

Well now, if we're constructing or parsing programs of the `ReadWriteDelete` effect type at runtime, 
we probably want to be able to _know_ whether or not the program we're about to run contains a call to `deleteMyHardDrive` _before_ we actually run it.

We could of course simply abort execution or ignore requests to delete everything when we're running the effects in our host language, which is nice, 
but the fact remains that if our app is handed an arbitrary `ReadWriteDelete m => m ()` program at runtime, there's _no_ way to know whether or not it could possibly contain a call to `deleteMyHardDrive` without actually running the program, and even then, there's no way to know whether there's some __other__ possible execution path that we missed which _does_ call `deleteMyHardDrive`.

We'd really love to be able to _analyse_ the program and all of its possible effects _before_ we run anything at all.

## The Benefits of Static Analysis

Most programmers are familiar with the benefits of static analysis when applied to regular everyday programming languages.
It can catch basic errors like type-mismatches, incorrect function calls, and in some cases things like memory unsafety or race conditions.

We're typically after different kinds of benefits when analysing programs in our effect systems, but they are similarly useful!

For instance, given enough understanding of an effectful program we can 
perform code transformations like removing redundant calls, parallelizing independent workflows, caching results, and optimizing workflows into more efficient ones.

We can also gain useful knowledge, like creating a call graph for developers to better understand what's about to happen. Or perhaps analyzing the use of sensitive resources like the file system or network such that we can ask for approval before even beginning execution.

But as I've already mentioned, we can't do _most_ of these techniques in a Monadic effect system.
The monad interface itself makes it clear why this is the case:

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

We can see from `Bind` (`>>=`) that in order to know which effects (`m b`) will be executed next, we need to first execute the previous effect (`m a`) and then 
we need the host language (Haskell) to execute an arbitrary Haskell function. There's no way at all
for us to gain insight about what the results of that function might be without running it first.

Let's move a step towards the analysis side of the spectrum and talk about Applicatives...

## The origin of Applicatives

Applicatives are another interface for expressing effectful operations.

As far as I can determine, the first widespread introduction of Applicatives to programming was
in [Applicative Programming with Effects](https://www.staff.city.ac.uk/~ross/papers/Applicative.pdf), a 2008 paper by Conor McBride and Ross Paterson.

Take note that this paper was written _after_ Monads were already in widespread use,
and Applicatives are, by their very definition, **less expressive** than Monads. To be precise, Applicatives can
express _fewer effectful programs_ than Monads can. This is shown by the fact that every **Monad** implements the **Applicative** interface, but not every **Applicative** is a Monad.

Despite being _less expressive_ Applicatives are still very useful. 
They allow us to express programs with effects that aren't valid monads,
but they also provide us with the ability to better analyse which effects are part of an effectful program before running it.

Take a look at the Applicative interface:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

We can see that, unlike Monads, it affords no way to sequence effects such that future effects depend in any way on previously run effects.
The sequence of effects is determined entirely by the host language before we start to run the effects, and thus the sequence of effects
can be reliably inspected in advance.

This _limitation_, if you can even call it that, gives us a ton of utility in program analysis.
For any given sequence of Applicative Effects we can analyse it and produce a list
of all the planned effects before running any of them, then could ask the end-user for permission before running potentially harmful effects.

Let's see what this looks like for our ReadWrite effect.

```haskell
import Control.Applicative (liftA3)
import Control.Monad.Writer (Writer, runWriter, tell)

-- | We only require the Applicative interface now
class (Applicative m) => ReadWrite m where
  readLine :: m String
  writeLine :: String -> m ()

data Command
  = ReadLine
  | WriteLine String
  deriving (Show)

-- | We can implement an instance which runs a dummy interpreter that simply records the commands
-- the program wants to run, without actually executing anything for real.
instance ReadWrite (Writer [Command]) where
  readLine = tell [ReadLine] *> pure "Simulated User Input"
  writeLine msg = tell [WriteLine msg]

-- | A helper to run our program and get the list of commands it would execute
recordCommands :: Writer [Command] String -> [Command]
recordCommands w = snd (runWriter w)

-- | A simple program that greets the user.
myProgram :: (ReadWrite m) => String -> m String
myProgram greeting =
  liftA3
    (\_ name _ -> name)
    (writeLine (greeting <> ", what is your name?"))
    readLine
    (writeLine "Welcome!")

-- We can now run our program in the Writer applicative to see what it would do!
main :: IO ()
main = do
  let commands = recordCommands (myProgram "Hello")
  print commands

-- [WriteLine "Hello, what is your name?",ReadLine,WriteLine "Welcome!"]
```

Since this interface doesn't provide us with a `bind`, we can't use results from `readLine` in a future `writeLine` effect, which is a bummer. 
It's clear that Applicatives are less __expressive__ in this way,
but we _can_ run an analysis of a program written in the Applicative `ReadWrite` to see **exactly** which effects it will run, and which arguments each of them are provided with, before we execute anything for real.

I hope that's enough ink to convince you that it's always a simple matter of "more expressive is always better", but rather that expressiveness exists on a continuum between
ease of program analysis and expressiveness.

Expressive power comes at a cost, specifically the cost of analysis.

## Closer to the Sweet Spot

So clearly Applicatives are nice, but they're a pretty strong limitation and prevent us 
from writing a lot of useful programs. What if there was an interface somewhere on the spectrum between the two?

**Selective Applicatives** fit nicely between Applicatives and Monads.

If you haven't heard of them, this isn't a tutorial on Selective itself, so go read up on them [here](https://hackage.haskell.org/package/selective) if you like.

The interface for Selective Applicatives is similar to Applicatives, but they allow us to specify a known set of branching codepaths that our program _may_ choose between when executing.
Unlike the monadic interface, these branching paths need to be known and enumerated in advance, we can't make them up on the fly while running our effects.

This interface gets us _much_ closer to matching the
level of expressiveness we actually need for everyday programming while still granting us
most of the best benefits of program analysis.

Here's an example of what it looks like to analyse a `ReadWriteDelete` program using Selective Applicatives:

```haskell
import Control.Monad.Writer
import Control.Selective as Selective
import Data.Either
import Data.Functor ((<&>))

-- We require the Selective interface now
class (Selective m) => ReadWriteDelete m where
  readLine :: m String
  writeLine :: String -> m ()
  deleteMyHardDrive :: m ()

data Command
  = ReadLine
  | WriteLine String
  | DeleteMyHardDrive
  deriving (Show)

-- | "Under" is a helper for collecting the minimum number of selective effects.
instance ReadWriteDelete (Under [Command]) where
  readLine = Under [ReadLine]
  writeLine msg = Under [WriteLine msg]
  deleteMyHardDrive = Under [DeleteMyHardDrive]

-- | "Over" is a helper which collects all possible selective effects.
instance ReadWriteDelete (Over [Command]) where
  readLine = Over [ReadLine]
  writeLine msg = Over [WriteLine msg]
  deleteMyHardDrive = Over [DeleteMyHardDrive]

-- | A "real" IO instance
instance ReadWriteDelete IO where
  readLine = getLine
  writeLine msg = putStrLn msg
  deleteMyHardDrive = putStrLn "Deleting hard drive... Just kidding!"

-- | A program using Selective effects
myProgram :: (ReadWriteDelete m) => m String
myProgram =
  let msgKind =
        Selective.matchS
          -- All the valid values we expect and should consider during static analysis
          (Selective.cases ["friendly", "mean"])
          -- The action we run to get the input
          readLine
          -- What to do with each input
          ( \case
              "friendly" -> writeLine ("Hello! what is your name?") *> readLine
              "mean" -> writeLine ("Hey doofus, what do you want? Too late. I deleted your hard-drive. How do you feel about that?") *> deleteMyHardDrive *> readLine
              -- This can't actually happen.
              _ -> error "impossible"
          )
      prompt = writeLine "Select your mood: friendly or mean"
      fallback =
        (writeLine "That was unexpected. You're an odd one aren't you?")
          <&> \() actualInput -> "Got unknown input: " <> actualInput
   in prompt
        *> Selective.branch
          msgKind
          fallback
          (pure id)

allPossibleCommands :: Over [Command] x -> [Command]
allPossibleCommands (Over cmds) = cmds

minimumPossibleCommands :: Under [Command] x -> [Command]
minimumPossibleCommands (Under cmds) = cmds

runIO :: IO String
runIO = myProgram

-- | We can now run our program in the Writer applicative to see what it would do!
main :: IO ()
main = do
  let allCommands = allPossibleCommands myProgram
  let minimumCommands = minimumPossibleCommands myProgram
  putStrLn "All possible commands:"
  print allCommands
  putStrLn "Minimum possible commands:"
  print minimumCommands

-- All possible commands:
-- [ WriteLine "Select your mood: friendly or mean"
-- , ReadLine
-- , WriteLine "Hey doofus, what do you want? Too late. I deleted your hard-drive. How do you feel about that?"
-- , DeleteMyHardDrive
-- , ReadLine
-- , WriteLine "Hello! what is your name?"
-- , ReadLine
-- , WriteLine "That was unexpected. You're an odd one aren't you?"
-- ]
--
-- Minimum possible commands:
-- [ WriteLine "Select your mood: friendly or mean"
-- , ReadLine
-- ]
```

Okay, so now you've read a program which uses the full power of Selective applicative to _branch_ based on the results of previous effects. 

We can branch on user input to select either a friendly or mean greeting style, so it's clearly more expressive than the Applicative version, 
but it's also pretty obvious that this is the clunkiest option available. It's a bit tricky to write, and is also pretty tough to read.

We can now _branch_ on user input, but since we need to pre-configure an explicit branch for every possible
input we want to handle, we can't even write a simple program which echos back whatever the user types in, or even one that greets them by name.
There are clearly still some substantial limitations on which programs we can express here.

However, let's look on the bright side for a bit, similar to our approach with Applicatives we can analyse the commands our program may run. 
This time however, we've got branching paths in our program.

The selective interface gives us two methods to analyse our program:

* The `Under` newtype will let us collect the minimum possible sequence of of effects that our program will run no matter what inputs it receives.
* The `Over` newtype instead collects the list of _all_ possible effects that our program could possibly encounter if it were to run through all of its branching paths.

This isn't as usful as receiving, say, a graph representing the possible execution paths, but it does give us enough information to give users a warning aobut what a program might possibly do, we can let them know that hey, I don't know exactly what will cause it, but this program has the ability to delete your hard-drive.

You can of course write additional Selective interfaces, or use the Free Selective to re-write Selective computations in order to optimize or memoize them as you wish just like you can with Applicatives.

It's clear at this point that Selectives are another good tool, but the limitations are still too severe:

* We can't use results from previous effects in future effects.
* We can't express things like loops or recursion which require effects
* Branching logic like case-statements are expressible, but very cumbersome.
* The syntax for writing programs using Selective Applicatives is a bit rough, and there's no do-notation equivalent.

## In search of the true sweet spot

This isn't a solved problem yet, but don't worry, there are yet more methods of sequencing effects.

It may take me another 5 years to finally finish it, but at some point we'll continue this journey and explore how we can 
sequence effects using a hierarchy of Category classes instead, and how this may help us find a more tenable middle-ground on our Expressiveness Spectrum. 
A place where we can analyze possible execution paths without sacrificing the ability to write the programs we need.

I hope this blog post helps others to understand that while Monads were a huge discovery to the benefit of functional programming, 
that we should keep looking for abstractions which are a better fit for the problems we generally face in day-to-day programming.
