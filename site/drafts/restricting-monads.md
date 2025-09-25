---
title: "Monads are too powerful"
author: Chris Penner
date: Sep 15, 2025
tags: [programming, haskell]
description: "Monads are a useful tool, but what costs do we pay for their expressive power?"
image: pipes.jpg
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
and it turns out that there's a similar version of the debate to be had within the realm of effect systems themselves!

In their essence, effect systems are just methods of expressing miniature programs
**within** your programming language of choice. These mini programs can be constructed,
analysed, and executed at runtime within the framework of the larger programming language, 
and the same Expressiveness Spectrum applies independently to them as well! That is, the more programs you 
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
program within our ReadWrite DSL. Our simple program here just reads a line of input from the user and then greats them by name.

This is all well and good in such a simple case, however if we expand our simple `ReadWrite` effect slightly by adding a new effect:

```haskell
class Monad m => ReadWriteDelete m where
  readLine :: m String
  writeLine :: String -> m ()
  deleteMyHardDrive :: m ()
```

Well now, if we're constructing or parsing programs of the `ReadWriteDelete` effect type at runtime, 
we probably want to be able to _know_ whether or not the program we're about to run contains a call to `deleteMyHardDrive` _before_ we actually run it!

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

But as I've already mentioned, we can't do _most_ of these techniques in a Monadic effect system!
The monad interface itself makes it clear why this is the case🀄

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

We can see from `Bind` (`>>=`) that in order to know which effects (`m b`) will be executed next, we need to first execute the previous effect (`m a`) and then 
we need the host language (Haskell) to execute an arbitrary Haskell function. There's no way at all
for us to gain insight about what the results of that function might be without running it first!

Let's move a step towards the analysis side of the spectrum and talk about Applicatives!

## The origin of Applicatives

Applicatives are another interface for expressing effectful operations.

As far as I can determine, the first widespread introduction of Applicatives to programming was
in [Applicative Programming with Effects](https://www.staff.city.ac.uk/~ross/papers/Applicative.pdf), a 2008 paper by Conor McBride and Ross Paterson.

Take note that this paper was written _after_ Monads were already in widespread use,
and Applicatives are, by their very definition, **less expressive** than Monads. To be precise, Applicatives can
express _fewer effectful programs_ than Monads can. This is shown by the fact that every **Monad** implements the **Applicative** interface, but not every **Applicative** is a Monad.

Despite being _less expressive_ Applicatives are still very useful! 
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
can be reliably inspected in advance!

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

## Understanding the blindness of monadic binds

If we take a step back and think about most of the programs we write as software developers, a few things are pretty clear;

* We often need to branch our logic based on the results of previous effects.
* We almost _never_ choose to execute completely _arbitrary_ sets of effects based on the results of previous effects.

Unless you're writing an interpreter that needs to load and eval strings of JavaScript or something, it's extremely unlikely
that you don't know in advance exactly which effects your program _may_ execute. You probably have a strong understanding of every _possible_ codepath,
the only unknown is which of many pre-defined branching paths any given execution will take.

Defining a small DSL, we can express this idea pretty clearly.

Here's a small CLI DSL which has a few possible effects:

* Get user input
* Read a file
* Write a file
* Echo to stdout
* Delete my entire hard drive

```haskell
data CommandF r
  = GetInput ([String] -> r)
  | ReadFile FilePath (String -> r)
  | WriteFile FilePath String r
  | Echo String r
  | DeleteMyHardDrive r
  deriving (Functor)

-- Free Monad constructors
type Command = Free CommandF

getInput :: Command [String]
getInput = liftF (GetInput id)

readFile :: FilePath -> Command String
readFile path = liftF (ReadFile path id)

writeFile :: FilePath -> String -> Command ()
writeFile path content = liftF (WriteFile path content ())

echo :: String -> Command ()
echo msg = liftF (Echo msg ())

deleteMyHardDrive :: Command ()
deleteMyHardDrive = liftF (DeleteMyHardDrive ())
```

Here's a small program written in this DSL:

```haskell
myProgram :: Command ()
myProgram = do
  input <- getInput
  case input of
    ["read", path] -> do
      content <- readFile path
      echo content
    ["write", path, content] -> do
      writeFile path content
      echo "File written."
    _ -> do
      echo "Unknown command."
      deleteMyHardDrive
```

This example is obviously contrived for the sake of simplicity and pedagogy of course, but if I'm about to execute a `Command` program at runtime, 
it'd be very nice to first be able to programmatically analyse all the possible effect chains, in this case discovering that one of them does indeed trigger `deleteMyHardDrive`.

However, since `getInput` uses `bind` to pass its result into a lambda, there's no way to analyze anything past that point without actually running things.
We could of course use the power of the Free monad to interpret the program in steps and we could bail or error if we find a `DeleteMyHardDrive` effect, but in 
real systems this would still require _actually_ running the program otherwise we won't know which branches would be taken!

So, in short, Monads are very expressive, but severely limit our analysis, but Applicatives don't let us depend on results of
previous effects, which is a deal-breaker in most real-world systems. Clearly the sweet spot must be somewhere in between!

## The Sweet Spot

**Selective Applicatives** fit nicely into the continuum between Applicatives and Monads.

If you haven't heard of them, go read up on them [here](https://hackage.haskell.org/package/selective).

The interface for Selective Applicatives allow us to specify a statically known set of branching codepaths and effects that our program _may_ execute,
but it leaves the actual branching to runtime.

This interface gets us _much_ closer to matching the
level of expressiveness we need for everyday programming while still granting us
most of the best benefits of program analysis.

With Selective Applicatives we can do things like crawl the expression to see
the union of all possible effects we may run, we can transform the effect graph to perform optimizations like memoizing duplicated
effects in advance, we can display a flow chart of all possible program executions to the user, we could check all the dependencies of a build-system defined in a Selective Applicative such that we can determine whether any of their inputs have changed, the list goes on.
These are all things we can't easily do once we've `bound` ourselves into a corner with Monads.

However, there are unfortunately a lot of problems with Selective Applicatives too:

* We can't express things like loops or recursion which contain effects
* Branching logic like case-statements are expressible, but very cumbersome as they're expressed as a sequence of
  branching binary choices.
* There's no good syntax for expressing Selective Applicatives like there is with do-notation for Monads and ApplicativeDo.
* None of Functors, Applicative, Selective Applicative Functors, OR Monads have any structured representation of inputs or the flow of data from one effectful computation to the next.

## In conclusion (for now)

I hope this blog post helps others to understand that while Monads were a huge discovery to the benefit of functional programming, 
we shouldn't stop looking for abstractions which are a better fit for the problems we generally face in day-to-day programming.

Selective Applicatives are a great step in the right direction, 
but are unfortunately under-utilized, aren't part of Haskell s Functor-Applicative-Monad Hierarchy, 
and don't have their own syntax. 

This is just one post in a series I have planned, but I've noticed that multiple years of thinking on these problems have passed 
without any productive output, so this first post is an attempt to get the ball rolling and hopefully I'll follow up with 
more posts exploring this space soon.

Stay tuned!

