---
title: "Monads are too powerful"
author: Chris Penner
date: Sep 15, 2025
tags: [programming, haskell]
description: "Monads are a useful tool, but what we gain in power we lose in programmatic analysis."
image: pipes.jpg
---

Okay, so you and I both know Monads are great, they allow us to sequence effects
in a structured way and are in many ways a super-power in the functional-programming toolkit.

My argument however, is that monads are actually _too_ powerful for their own good. Or to be more clear, 
monads are more **expressive** than they need to be, at the cost of other benefits.

A defining feature of the Monadic interface is that they allow dynamic selection 
of effects based on **the results of previous effects**.

This is a huge boon, it allows critical workflows like fetching input from a user before 
deciding which command to run, and things like fetching documents from a database, then fetching more documents 
which are referenced by the first documents.

It's truly hard to imagine what any modern Haskell program would look like without using monads!
But too much power can be a bad thing.

Alas, I must convince you, so why would we ever want to restrict ourselves?

Well, let's talk about Applicatives!

## The origin of Applicatives

As far as I can determine, the first widespread introduction of Applicatives to programming was
in [Applicative Programming with Effects](https://www.staff.city.ac.uk/~ross/papers/Applicative.pdf), a 2008 paper by Conor McBride and Ross Paterson.

Take note that this paper was written _after_ Monads were already in widespread use.
Applicatives are, by definition, _less powerful_ than Monads, or to be more precise, Applicatives can
express fewer effectful programs than Monads can. This is easily proven by the fact that every Monad implements the Applicative interface, but not every Applicative is a Monad.

Despite being _weaker_ (a.k.a _less expressive_) by this definition, Applicatives are still very useful! They allow us to express programs with effects that aren't valid monads,
but they also provide us with the ability to analyse values which are sequences of Applicative effects at runtime and glean information about them before running them.

Let's dive a bit deeper to make sure we understand that bit. Feel free to skip ahead if you've got it down.

Looking at the Applicative interface:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

We can see that it affords no way to sequence effects at such that the chosen sequence of effects is dependent on the results of previous effects.

Compare this to the Monad interface:

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

Bind (`>>=`) allows creating new effects in a Haskell function using the result of a previous effect. Apply (`<*>`) on the other hand allows running a Haskell function, but the result of the function is completely independent from the sequence of effects. By the time we're running the effects we already know _exactly_ what the sequence of planned effects is, and, except for a pure exception (which is a whole other can of worms) there's no way to change the planned sequence.

This _limitation_, if you can even call it that, gives us a ton of utility in program analysis.
For example, we could take an applicative effect chain and produce a list
of all the planned effects before running any of them, then could ask the end-user for permission before running potentially harmful effects.
Since the program's structure is set, we can also parallelize effects when we know it's safe to do so.
We can analyse resources accessed by effects and track them in an interactive build system, re-running the effect change when any dependency changes,
and can even do interesting things like reverse or rearrange the ordering of the effects to optimize for some criteria.
E.g. see the [Backwards](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Applicative-Backwards.html) applicative transformer in the `transformers` package.

I hope that's enough ink to convince you that the _expressiveness_ (i.e. the breadth of programs we can express)
is not a simple matter of "more expressible programs is always better", but rather that expressiveness exists on a continuum between
ease of program analysis and expressiveness.

```
Analysis <+------------+------------+> Expressiveness
          |            |            |
          +> Functor   |            +> Monad
                       +> Applicative
```

Power comes at a cost, specifically the cost of ease of analysis.


Revisiting the signature of `bind`

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

It's clear that as soon as we hit a bind in some monadic computation, anything could happen next.

Haskell doesn't provide us with any (sane) mechanism for analyzing arbitrary
chunks of Haskell code we're about to execute at runtime, so if we want to know which
effects might come next we have no choice: Execute the code and see what happens!

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

