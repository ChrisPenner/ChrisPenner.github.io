---
title: "Update Monads: Execution with Auditing"
author: Chris Penner
date: Sep 3, 2018
tags: [programming, haskell]
description: "We explore the applications and implementation of a generalized version of Reader/Writer monads."
image: update-monad/change.jpg
---

Today we're going to take a peek at the Update monad! It's a monad which was
formalized and described in [Update Monads: Cointerpreting Directed
Containers](https://danelahman.github.io/papers/types13postproc.pdf) by Danel
Ahman and Tarmo Uustalu. Most folks probably haven't heard of it before, likely
because most of what you'd use it for is well encompassed by the Reader,
Writer, and State monads. The Update Monad can do everything that Reader,
Writer, and State can do, but as a trade-off tends to be less efficient at each
of those tasks. It's definitely still worth checking out though; not only is it
interesting, there are a few things it handles quite elegantly that might be a
bit awkward to do in other ways.

Heads up; this probably isn't a great post for absolute beginners, you'll want
to have a decent understanding of [monoids](https://wiki.haskell.org/Monoid)
and how [StateT](https://wiki.haskell.org/State_Monad) works before you dive in
here. 

For readers who've spent a bit of time in Javascript land you may notice that
the Update Monad is basically a formalization of the [Flux
architecture](https://www.dotnetcurry.com/reactjs/1356/redux-pattern-tutorial),
most commonly associated with the Redux library; although of course the Update
Monad paper came first 😉. Most of the concepts carry over in some form. The
`Store` in redux corresponds to the state of the Update monad, the `Action`s in
Redux correspond directly to our monoidal Actions in the Update monad, and the
view and dispatcher are left up to the implementor, but could be likened to a
base monad in a monad transformer stack which could render, react, or get user
input (e.g. IO).

The Update monad is very similar to the State monad; and in fact you can
implement either of them in terms of the other! Each has tasks at which it
excels; the Update Monad is good at keeping an audit log of updates and
limiting computations to a fixed set of permissible updates. State on the
other hand has a simpler interface, less boiler-plate, and is MUCH more efficient
at most practical tasks. It's no wonder that `State` won out in the end, but
the Update monad is still fun to look at!

## Structure of the Update Monad

The Update Monad kinda looks like Reader, Writer and State got into a horrific
car accident and are now hopelessly entangled! Each computation receives the
current computation `state` (like `Reader`) and can result in a monoidal action
(like `Writer`). The action is them applied to the state according to a helper
typeclass which I'll call `ApplyAction`: it has a single method
`applyAction :: p -> s -> s`; which applies a given monoidal action `p` to a
state resulting in a new state. This edited state is passed on to the next
computation (like `State`) and away we go! Here's my implementation of this idea for a new
data type called `Update`.


```haskell
class (Monoid p) => ApplyAction p s where
  applyAction :: p -> s -> s

data Update s p a = Update
  { runUpdate :: (s -> (p, a))
  } deriving (Functor)

instance (ApplyAction p s) => Applicative (Update s p) where
  pure a = Update $ \_ -> (mempty, a)
  Update u <*> Update t =
    Update $ \s
      -- Run the first 'Update' with the initial state 
      -- and get the monoidal action and the function out
     ->
      let (p, f) = u s
      -- Run the second 'Update' with a state which has been altered by
      -- the first action to get the 'a' and another action
          (p', a) = t (applyAction p s)
      -- Combine the actions together and run the function
       in (p' <> p, f a)

instance (ApplyAction p s) => Monad (Update s p) where
  Update u >>= f =
    Update $ \s
      -- Run the first 'Update' with the initial state 
      -- and get the monoidal action and the function out
     ->
      let (p, a) = u s
      -- Run the given function over our resulting value to get our next Update
          Update t = f a
      -- Run our new 'Update' over the altered state
          (p', a') = t (applyAction p s)
      -- Combine the actions together and return the result
       in (p <> p', a')
```

We could of course also implement an `UpdateT` monad transformer, but for the
purposes of clarity I find it's easier to understand the concrete `Update`
type. If you like you can take a peek at some other fun implementations
[here](https://github.com/chrispenner/update-monad). Hopefully it's relatively
clear from the implementation how things fit together. Hopefully you can kind
of see the similarities to Reader and Writer; we are always returning and
combining our monoidal actions as we continue along, and each action has access
to the state, but can't *directly* modify it (you may only modify it by
providing **actions**). It's also worth noting that within any individual step
has only the latest `state` and it's not possible to view any previous actions
which may have occurred, just like the Writer monad.

Now that we've implemented our Update Monad we've got our `>>=` and `return`;
but how do we actually accomplish anything with it? There's no `MonadUpdate`
type-class provided in the paper, but here's my personal take on how to get
some utility out of it, I've narrowed it down to these two methods which seem
to encompass the core ideas behind the Update Monad:

```haskell
{-# LANGUAGE FunctionalDependencies #-}
class (ApplyAction s p, Monad m) =>
      -- Because each of our methods only uses p OR m but not both 
      -- we use functional dependencies to assert to the type system that 
      -- both s and p are determined by 'm'; this helps GHC be confident
      -- that we can't end up in spots where types could be ambiguous.
      MonadUpdate m s p | m -> s , m -> p
  where
    putAction :: p -> m ()
    getState :: m s
```

You'll notice some similarities here too! `putAction` matches the signature for
`tell`, and `getState` matches `ask`! This class still provides new value
though, because unlike Reader and Writer the environment and the actions are
related to each other through the `ApplyAction` class; and unlike `get` and
`put` from `State` our `putAction` and `getState` operate over **different**
types; you can only `put` **actions**, and you can only `get` **state**. We can
formalize the expected relationship between these methods with these laws I
made up (take with a deluge of salt):

```haskell
-- Applying the 'empty' action to your state shouldn't change your state
applyAction mempty == id

-- Putting an action and then another action should be the same as 
-- putting the combination of the two actions.
-- This law effectively enforces that `bind` is 
-- employing your monoid as expected
putAction p >> putAction q == putAction (p `mappend` q)

-- We expect that when we 'put' an action that it gets applied to the state
-- and that the change is visible immediately
-- This law enforces that your implementation of bind 
-- is actually applying your monoid to the state using ApplyAction
applyAction p <$> getState == putAction p >> getState
```

Okay! Now of course we have to implement `MonadUpdate` for our `Update` monad;
easy-peasy:

```haskell
instance (ApplyAction p s) => MonadUpdate (Update p s) p s where
  putAction p = Update $ \_ -> (p, ())
  getState = Update $ \s -> (mempty, s)
```

All the plumbing is set up! Let's start looking into some actual use-cases!
I'll start by fully describing one particular use-case so we get an
understanding of how this all works, then we'll experiment by tweaking our
monoid or our `applyAction` function.

## A Concrete Use-Case

Let's pick a use-case which I often see used for demonstrating the State monad so we
can see how our Update monad is similar, but slightly different!

We're going to build a system which allows users to interact with their bank account!
We'll have three actions they can perform: `Deposit`, `Withdraw`, and `CollectInterest`.
These actions will be applied to a simple state `BankAccount Int` which keeps track
of how many dollars we have in the account!

Let's whip up the data types and operations we'll need:

```haskell
-- Simple type to keep track our bank balance
newtype BankBalance =
  BankBalance Int
  deriving (Eq, Ord, Show)

-- The three types of actions we can take on our account
data AccountAction
  = Deposit Int
  | Withdraw Int
  | ApplyInterest
  deriving (Eq, Ord, Show)

-- We can apply any of our actions to our bank balance to get a new balance
processTransaction :: AccountAction -> BankBalance -> BankBalance
processTransaction (Deposit n) (BankBalance b) 
    = BankBalance (b + n)
processTransaction (Withdraw n) (BankBalance b) 
    = BankBalance (b - n)

-- This is a gross oversimplification...
-- I really hope my bank does something smarter than this
-- We (kinda sorta) add 10% interest, truncating any cents.
-- Who likes pocket-change anyways ¯\_(ツ)_/¯
processTransaction ApplyInterest (BankBalance b) 
    = BankBalance (fromIntegral balance * 1.1)
```

Now we've got our Action type and our State type, let's relate them together
using `ApplyAction`.

```haskell
instance ApplyAction AccountAction BankBalance where
  applyAction = processTransaction
```

One problem though! `AccountAction` isn't a monoid! Hrmmm, this is a bit
upsetting; it seems to quite clearly represent the domain we want to work with,
I'd really rather not muck up our data-type just to make it fit here. Maybe
there's something else we can do! In our case, what does it mean to combine two
actions? For a bank balance we probably just want to run the first action, then
the second one! We'll need a value that acts as an 'empty' value for our monoid's `mempty`
too; for that we can just have some notion of performing no actions!

There are a few ways to promote our `AccountAction` type into a monoid with
these properties; but one in particular stands out (I can already hear some of
you shouting it at your screens). That's right! The [Free
Monoid](https://en.wikipedia.org/wiki/Free_monoid) A.K.A. the List Monoid!
Lists are kind of a special monoid in that they can turn ANY type into a monoid
for **free**! We get `mappend == (++)` and `mempty == []`. This means that
instead of *actually* combining things we kinda just collect them all, but fear
not it still satisfies all the monoid laws correctly. This isn't a post on Free
Monoids though, so we'll upgrade our `AccountAction` to `[AccountAction]` and
move on:

```haskell
instance ApplyAction [AccountAction] BankBalance where
  applyAction actions balance =
    let allTransactions :: BankBalance -> BankBalance
        allTransactions = appEndo $ foldMap (Endo . processTransaction) (reverse actions)
     in allTransactions balance
```

We can keep our `processTransaction` function and partially apply it to our
list of Actions giving us a list of `[BankBalance -> BankBalance]`; we can then
use the `Endo` monoid to compose all of the functions together! Unfortunately
Endo does right-to-left composition, so we'll need to reverse the list first
(keeners will note we could use `Dual . Endo` for the same results). Then we
use `appEndo` to unpack the resulting `BankBalance -> BankBalance` which we can
apply to our balance! Now that we have an instance for `ApplyAction` we can
start writing programs using `Update`.

```haskell
useATM :: Update [AccountAction] BankBalance ()
useATM = do
  putAction [Deposit 20] -- BankBalance 20
  putAction [Deposit 30] -- BankBalance 50
  putAction [ApplyInterest] -- BankBalance 55
  putAction [Withdraw 10] -- BankBalance 45
  getState

$> runUpdate useATM (BankBalance 0)
([Deposit 20,Deposit 30,ApplyInterest,Withdraw 10],BankBalance 45)
```

Hrmm, a bit clunky that we have to wrap every action with a list, but we could
pretty easily write a helper `putAction' :: MonadUpdate m [p] s => p -> m ()`
to help with that. By running the program we can see that we've collected the
actions in the right order and have 'combined' them all by running `mappend`.
We also see that our bank balance ends up where we'd expect! This seems to be
pretty similar to the State Monad, we could write helpers that perform each of
those actions over the State pretty easily using `modify`; but the Update Monad
gives us a nice audit log of everything that happened! Not to mention that it
limits the available actions to ones that we support; users can't just multiply
their bank balance by 100, they have use the approved actions. This means we
could verify that actions happened in the correct order, or we could run the
same actions over a different starting state and see how it works out!

The Update Monad also has a few tricks when it comes to testing your programs.
Since the only thing that can affect our state is a sequence of actions, we can
skip all the monad nonsense and test our business logic by just testing that
our `applyAction` function works properly over different lists of actions!
Observe:

```haskell
testBankSystem :: Bool
testBankSystem =
  applyAction [Deposit 20, Deposit 30, ApplyInterest, Withdraw 10] (BankBalance 0) 
    == BankBalance 45

$> testBankSystem
True
```

Cool stuff! We can write the tests for our business logic without worrying
about the impure ways we'll probably be getting those actions (like `IO`). This
separation makes complicated business logic pretty easy to test, and we can
write separate tests for the 'glue' code with confidence that the logic of our
actions is correct and that our program **CAN'T** edit our state in an invalid
way since all updates **must** be performed through the `performTransaction`
function. Note that using an impure base monad like IO could certainly cause
the list of actions which are collected to change, but the list of actions
which is collected **fully describes** the state changes which take place; and
so testing only the application of actions is sufficient for testing state
updates.

There's really only so much we can do with `Update` alone, but it's pretty easy
to write an `UpdateT` transformer! I'll leave you to check out the
implementation
[here](https://github.com/ChrisPenner/update-monad/blob/master/src/UpdateT.hs)
if you like; but this allows us to do things like decide which actions to take
based on user input (via `IO`), use our state to make choices in the middle of
our monad, or use other monads to perform more interesting logic!

## Customizing the Update Monad with Monoids

Okay! We've got one concrete use-case under our belts and have a pretty good
understanding of how all this works! let's see what we can tweak to make things
a bit more interesting!

Something that immediately interested me with the update monad is that there
are several distinct places to tweak its behaviour without even needing to
change which implementation of `MonadUpdate` we use! We can change the action
monoid, or which state we carry, or even our `applyAction` function! This sort
of tweakability leads to all sorts of cool behaviour without too much work, and
people can build all sorts of things we didn't initially expect when we wrote
the type-classes!

I won't get super in depth on each of these and encourage you to implement them
yourself, but here are a few ideas to start with!

Customizations:

- `Update w () a` with `applyAction _ () = ()`
    - A simple implementation of `Writer`!
    - The state doesn't matter; only the monoidal actions are tracked!

- `Update () r a` with `applyAction () r = r`
    - A simple implementation of `Reader`!
    - There're no sensible updates to do; so your state always stays the same.

- `Update (Last s) s a` with `applyAction (Last p) s = fromMaybe s p`
    - This is the state monad implemented in Update!
    - `get == getState` 
    - `put == putAction . Last . Just`
    - `modify f == getState >>= putAction . Last . Just . f`

- `Update (Dual (Endo s)) s a` with `applyAction (Dual (Endo p)) s = p s`
    - Another possible implementation of State inside Update!
    - `get == getState` 
    - `put == putAction . Dual . Endo . const`
    - `modify == putAction . Dual . Endo`

- `Update Any Bool a` with `applyAction (Any b) s = b || s`
    - You could implement a short-circuiting approach where future actions don't bother running if any previous
        action has succeeded! You can flip the logic using `All` and `&&`.

## Bonus: Performance

The definition of the Update monad given here is quite simple because it's the
easiest to explain, but there are a few problems with it; the most notable is
that it ONLY passes along the new monoidal sum; NOT the edited state from step
to step. In mathematic terms it's still correct since we can compute an
up-to-date version of the state; but we have to compute it from scratch every
time we run an action! Clearly not great for performance! Like I said earlier
you can actually implement a more efficient version of `MonadUpdate` using
`State`! We DO still need a dependency on `ApplyAction p s` though, so keep
that in mind. If we have one available we can do something like this:

```haskell
instance ApplyAction p s => MonadUpdate (State (p, s)) p s where
  putAction p' = modify (\(p, s) -> (p <> p', applyAction p' s))
  getState = snd <$> get
```

Technically we don't even need to keep track of the monoidal sum as we go
along; there's no need for it! Unfortunately due to FunctionalDependencies in our
MonadUpdate class GHC gets mad if it doesn't show up inside our State Monad **somewhere**.
This implementation keeps track of the latest state and just applies updates as it goes
along, giving us a more efficient implementation. Note that using `put` or `modify` directly
will probably cause some unexpected behaviour in your Update Monad, so you may want to wrap
your `State` in a newtype first to prevent anyone from messing with the internals.

Thanks for reading! I'm not perfect and really just go through all this stuff
in my spare time, so if I've missed something (or you enjoyed the post 😄)
please let me know! You can find me on
[Twitter](https://twitter.com/chrislpenner) or
[Reddit](https://www.reddit.com/user/ChrisPenner)!




