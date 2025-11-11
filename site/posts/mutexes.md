---
title: "Ditch your (mut)ex, you deserve better"
author: Chris Penner
date: Nov 11, 2025
tags: [programming, haskell]
description: "Mutexes are unreliable tools, let's explore better alternatives."
image: parallel-pipes.jpg
---

Having access to multiple parallel CPU cores isn't a new thing by any means, people have been programming
in parallel for half a century now, but recent years
we've found ourselves at an inflection point. 
Moore's law is dying, beefy single cores are no longer keeping up. Modern computers 
come with multiple CPU cores, so exploiting parallel compute is more important than ever.
Given how long it's been an area of research we can naturally expect that effective 
tools have taken root and that synchronizing threads is trivial now right...?

Unfortunately this has not been my experience, and I'm willing to bet it hasn't been yours either.
Managing shared state across threads is hard, and the most commonly used tools: mutexes and semaphores, simply 
haven't evolved much since their inception.

The words that follow will dig into the problems inherent to mutexes and synchronizing
shared mutable state. Afterwards we'll look into other avenues which should prove more helpful.

## The Problem with Shared State

Let's begin by crafting a simple software system which needs synchronization in the first place. 

I'll present a commonly used example: the task of managing bank account balances correctly in spite of parallel transfer requests.

Of course real banks don't store all their account balances in RAM, so I'll 
hope that the reader can apply the concepts from this pedagogical example to a their own domain as necessary, it serves as a 
stand-in for any sufficiently complex system which requires ad-hoc synchronization of arbitrary data between multiple threads.

Here's some golang'ish pseudo-code (please don't try to actually compile it) for a simple bank account and the operations upon it.
I'm focused on the synchronization problems here, so forgive me for skipping the double-entry accounting, input validation, and other real-world complexities.

```go
struct Account {
  balance int,
}

// Deposit money into an account
func (a *Account) deposit(amount int) {
  a.balance += amount
}

// Withdraw money from an account, or return false if there are insufficient funds
func (a *Account) withdraw(amount int) bool {
  if (a.balance < amount) {
    return false
  } else {
    balance -= amount
    return true
  }
}
```

Great! This defines our Account type and some methods for withdrawing and depositing money into 
such an account. Now let's add a function to transfer money between accounts:

```go
func transfer(from *Account, to *Account, amount int) bool {
  if (from.withdraw(amount)) {
    to.deposit(amount)
    return true
  } else {
    return false
  }
}
```

Looks good, but now what happens when we start handling multiple requests concurrently?

```go
struct TransferRequest {
  from *Account,
  to *Account,
  amount int,
}

func main() {
  // loop forever, accepting transfer requests and processing them in goroutines
  for {
    req := acceptTransferRequest()
    go transfer(req.from, req.to, req.amount)
  }
}
```

Things may work well in your tests if you're (un)lucky, and might even work well in production for a while, 
but sooner or later you're going to lose track of money and have some confused and angry customers.

Do you see why?
This brings us to our first synchronization problem to solve, **Data Races**.

### Data races

__Most__ programming languages are imperative with mutable data structures **\[citation needed\]**, 
so passing pointers to multiple threads leads to _shared mutable data_, and _shared mutable data_ necessarily causes _data races_.

A data race occurs any time two threads access the same memory location concurrently and non-deterministically when at least one of the accesses is a write. 
When a data race is present two runs of the same code with the same state may non-deterministically have a different result.

We're passing accounts by reference here, so multiple threads have access to modify the same account.
With multiple transfer go-routines running on the same account, each could be paused by the scheduler at nearly any point during its execution.
This means that even within this simple example we've already introduced a data race.
Take another look at the `withdraw` function, I'll point it out:

```go
// Withdraw money from an account, or return false if there are insufficient funds
func (a *Account) withdraw(amount int) bool {
  hasFunds := a.balance >= amount 
  // HERE! The scheduler could pause execution here and switch to another thread
  if (hasFunds) {
    balance -= amount
    return true
  } else {
    return false
  }
}
```

If two threads are withdrawing $100 from Alice's account, which only has $150 in it, it's possible that thread 1 checks the balance, sees there's enough money, then gets paused by the scheduler. Thread 2 runs, checks the balance, also sees there's enough money, then withdraws $100. 
When thread 1 later resumes execution _after the check_ it withdraws its $100 too, Alice's account ends up with a negative balance of -$50, which is invalid even though we had validation!

This sort of concurrency error is particularly insidious because the original `withdraw` method is perfectly reasonable, idiomatic, and correct in a single-threaded program;
however when we decide to add concurrency at a __completely different place__ in the system we've introduced a bug deep within existing previously correct code.
The idea that a perfectly normal evolution from a single-threaded to a multi-threaded program can introduce **critical system-breaking bugs** in completely unrelated code without so much as a warning is quite frankly _completely unacceptable_. As a craftsman I expect better from my tools. 

Okay, but now that we've lost thousands if not millions of dollars, how do we fix this?

Traditional knowledge points us towards **Mutexes**.

### Mutexes

Okay, we've encountered a problem with our shared mutable state, the traditional approach to solving these problems is to enforce _exclusive access_ to the shared data in so-called "critical sections".
Mutexes are so-named because they provide **mut**ual **ex**clusion, meaning only a single thread may access a given virtual resource at a time.

Here's how we can edit our program to fix the data race problems using a mutex:

```go
struct Account {
  mutex Mutex,
  balance int,
}

func (a *Account) deposit(amount int) {
  a.mutex.lock()
  defer a.mutex.unlock()
  a.balance += amount
}

func (a *Account) withdraw(amount int) bool {
  a.mutex.lock()
  defer a.mutex.unlock()
  hasFunds := a.balance >= amount 
  if (hasFunds) {
    balance -= amount
    return true
  } else {
    return false
  }
}
```

Now every `Account` has a mutex on it, which acts as an exclusive lock. 

It's much like a bathroom key in a busy restaurant.
When you want to use the bathroom, you take the key, there's only one key available for each bathroom, so while you've got hold of it 
nobody else can use that bathroom.
Now you're free to do your business, then you return the key to the hook on the wall for the next person.

Unlike a bathroom key however, mutexes are only _conceptual_ locks, not _real_ locks, and as such they operate on the honor system. 

If the programmer forgets to lock the mutex the system won't stop them from accessing the data anyways, and even then there's no actual link between
the data being locked and the lock itself, we need to trust the programmers to both _understand_ and _respect_ the agreement. A risky prospect on both counts.

In this case, we've addressed the data-race within `withdraw` and `deposit` by using mutexes, 
but we've still got a problem within the `transfer` function.

What happens if a thread is pre-empted between the calls to `withdraw` and `deposit` while running the `transfer` function? 
It's possible that money will been withdrawn from an account, but won't have yet been deposited in the other. 
This is an inconsistent state of the system, the money has temporarily disappeared, existing only in the operating memory of a thread, but not visible in any externally observable state. 
This can (and will) result in _very_ strange behaviour.

As a concrete way to observe the strangeness let's write a `report` function which prints out all account balances:

```go
func report() {
    for _, account := range accounts {
        account.mutex.lock()
        fmt.Println(account.balance)
        account.mutex.unlock()
    }
}
```

If we run a `report` while transfers are ongoing we'll likely see that the count of the total amount of money that exists within the system is incorrect, and changes from report to report, which should be impossible in a closed system like this! 
This inconsistency occurs even if we obtain the locks for each individual account before checking the balance.

In larger systems this sort of inconsistency problem can cause flaws in even simple logic, since choices may be made against inconsistent system states.
The root of this issue is that the `transfer` function requires holding multiple independent locks, but they're not grouped in any way into an atomic operation.

### Composing Critical Sections

We need some way to make the entire transfer operation atomic, at least from the perspective of other threads who are respecting our mutexes.

Okay, well no problem, we can just lock both accounts, right?

```go
func transfer(from *Account, to *Account, amount int) bool {
  from.mutex.lock()
  to.mutex.lock()
  defer from.mutex.unlock()
  defer to.mutex.unlock()

  if (from.withdraw(amount)) {
    to.deposit(amount)
    return true
  } else {
    return false
  }
}
```

I'm sure some readers have already seen a problem here, but have you seen _two_ problems here?

The first is obvious when you point it out, remember that `withdraw` and `deposit` _also_ lock the mutex on the account, so we're trying to acquire the same lock twice in the _same thread_. 

`transfer` won't even begin to run in this state, it will block forever inside `withdraw` when it tries to lock the `from.mutex` for the second time.

Some systems, like _re-entrant locks_ and Java's `synchronized` keyword do some additional book-keeping which
allow a single thread to lock the same mutex multiple times, so using a re-entrant lock here would solve this particular problem.
However other systems, like golang, avoid providing re-entrant locks [on a matter of principle](https://groups.google.com/g/golang-nuts/c/XqW1qcuZgKg/m/Ui3nQkeLV80J).

So what can we do? I suppose
we'll need to pull the locks _out of_ `withdraw` and `deposit` so we can 
lock them in `transfer` instead.

```go
func (a *Account) deposit(amount int) {
  a.balance += amount
}

func (a *Account) withdraw(amount int) bool {
  hasFunds := a.balance >= amount 
  if (hasFunds) {
    balance -= amount
    return true
  } else {
    return false
  }
}

func transfer(from *Account, to *Account, amount int) bool {
  from.mutex.lock()
  to.mutex.lock()
  defer from.mutex.unlock()
  defer to.mutex.unlock()

  if (from.withdraw(amount)) {
    to.deposit(amount)
    return true
  } else {
    return false
  }
}
```

Ugh, a correct `transfer` function should conceptually just be the _composition_ of our well encapsulated `withdraw` and a `deposit` functions, but defining it _correctly_ has forced us to remove the locking from both `withdraw` and `deposit`, making both of them _less safe_ to use. It has placed the burden of locking on the caller (without _any_ system-maintained guarantees), and even worse, we now need to remember to go and _add_ locking around every existing `withdraw` and `deposit` call in the entire codebase. Even if we try to encapsulate everything within the module and only export "safe" operations we've caused duplication since we now need synchronized and unsynchronized versions of our `withdraw` and `deposit` operations. And we'd still need to expose the mutexes if we want to allow callers to synchronize operations with other non-`Account` data.

What I'm getting at is that mutexes don't _compose_! They don't allow us to chain multiple critical sections into a single atomic unit, they force us to break encapsulation and thrust the implementation details of mutexes and locking onto the caller who shouldn't need to know the details about which invariants must be maintained deep within the implementation. 
Adding or removing access to synchronized variables within an operation will also necessitate adding or removing locking to _every call site_, and those call sites may be in a completely different application or library.
This is an absolute mess.

All that sounds pretty bad, but would you believe those aren't the only problems here? 
It's not just composition that's broken here though, in fixing `transfer` to make it an atomic operation we've managed to introduce a new, extra-well-hidden deadlock bug.

### Deadlocks/Livelocks

Recall that in our main loop we're accepting arbitrary transfer requests and spawning them off in goroutines.
What happens in our system if we have two transfer requests, Alice is trying to Venmo Bob $25 for the beanbag chair she just bought off him, meanwhile Bob remembers he needs to Venmo Alice the $130 he owes her for Weird Al concert tickets.

If by sheer coincidence they both submit their requests at the same time, we have two `transfer` calls:

* `transfer(aliceAccount, bobAccount, 25)`
* `transfer(bobAccount, aliceAccount, 130)`

Each of these calls will attempt to lock their `from` account and _then_ their `to` account.
If Alice and Bob get very unlucky, the system will start the first `transfer` and lock Alice's account, then get paused by the scheduler.
When the second `transfer` call comes in, it first locks Bob's account, then tries to lock Alice's account, but can't because it's already locked by the first `transfer` call.

This is a classic deadlock situation. Both threads will be stuck forever, and worse, both Alice and Bob's accounts will be locked until the system restarts.

This is a pretty disastrous consequence for a problem which is relatively hard to spot even in this trivially simple example. In a real system with dozens or hundreds of methods being parallelized in a combinatorial explosion of ways it's **very difficult** to reason about this, and can be a lot of work to ensure locks are obtained in a safe and consistent order.

Golang gets some credit here in that it does provide _some_ runtime tools for detecting both dead-locks and data-races, which is great, but these detections only help if your tests encounter the problem; they don't prevent the problem from happening in the first place. Most languages aren't so helpful, these issues can be very difficult to track down in production systems.

### Assessing the damage

What a dumpster fire we've gotten ourselves into...

While it may be no accident that the example I've engineered happens to hit all of the worst bugs at once, in my experience, given enough time and complexity these sorts of problems will crop up any system eventually.
Solving them with mutexes is especially dangerous because it will _seem_ to be an effective solution at first. 
Mutexes work fine in small localized use-cases, thus tempting us to use them, but as the system grows organically we stretch them too far and they fail catastrophically at scale, causing all sorts of hacky workarounds. 
I'm of the opinion that crossing your fingers and hoping for the best is not 
an adequate software-engineering strategy.

So, we've seen that architecting a correct software system using mutexes is _possible_, but **very difficult**. 
Every attempt we've made to fix one problem has spawned a couple more. 

Here's a summary of the problems we've encountered:

* Data races causing non-determinism and logic bugs
* Lack of atomicity causing inconsistent system states
* Lack of composition causing
  * Broken encapsulation
  * Code duplication
  * Cognitive overload on callers
* Deadlocks/livelocks causing system-wide freezes
* New features may require changes to every call-site

In my opinion, we've tried to stretch mutexes beyond their limits, both in this blog post and 
in the industry as a whole.
Mutexes work great in small, well-defined scopes where you're locking a _single_ resource which is only ever accessed in a handful of functions in the same module,
but they simply don't scale to large complex systems with many interacting components maintained by dozens or hundreds of developers. We 
need to evolve our tools and come up with more reliable solutions.

## Cleaning up the Chaos

Thankfully, despite an over-reliance on mutexes, we as an industry have still learned
a thing or two since the 1960s.
Particularly I think that enforcing _immutability by default_ goes a _long_ way here. 
For many programmers this is a paradigm shift from what they're used to, which usually causes some uneasiness. 
Seatbelts, too, were often scorned in their early years for their restrictive nature, but over time 
it has become the prevailing opinion that the mild inconvenience is more than worth the provided safety.

More and more languages (Haskell, Clojure, Erlang, Gleam, Elixir, Roc, Elm, Unison, ...) are realizing this 
and are adopting this as core design principle. 
Obviously not every programmer can switch to an immutable-first language over night, but I think it would behoove most
programmers to strongly consider an immutable language if parallelism is a large part of their project's workload.

Using immutable data structures immediately prevents data-races, full-stop. So stick
with immutable data everywhere you can, but in a world of immutability we'll still 
need some way to synchronize parallel processes and for that most of these languages 
do still provide some form of mutable reference. 
It's never the default, and there's typically some additional 
ceremony or tracking in the type system which acts as an immediate sign-post that 
shared-mutable state is involved; here there be dragons.

Even better than mutable references, decades of research and industrial 
research have provided us with a swath battle-tested high-level concurrency patterns
which are built on top of lower-level synchronization primitives like mutexes or mutable references,
typically exposing much safer interfaces to the programmer.

### Concurrency Patterns

Actor systems and Communicating Sequential Processes (CSP) are some of the most common 
concurrency orchestration patterns.
Each of these operate by defining independent sub-programs which have
their own isolated states which only they can access. Each actor or process receives messages from other units
and can respond to them in turn. Each of these deserves a talk or blog post of their own so I won't dive too deeply into them here, but please look into them deeper if this is the first you're hearing of them.

These approaches work great for _task parallelism_, where there are independent processes to run, and where your parallelism needs are bounded
by the number of tasks you'd like to run. 
As an example, I used an actor-based system when building Unison's code-syncing protocol. There was one actor responsible for loading and sending 
requests for code, one for receiving and unpacking code, and one for validating the hashes of received code. This system required **exactly** 3 workers to co-operate regardless
of _how many things_ I was syncing.
Actor and CSP systems are great choices when the number of workers/tasks we need to co-ordinate is statically known, i.e. a fixed number of workers, or a pre-defined map-reduce pipeline. These patterns can scale well to many cores since each actor or process can run independently on its own core without worrying about synchronizing access to shared mutable state, and as a result can often scale to multiple machines as well.

However, there are also problems where the parallelism is _dynamic_ or ad-hoc, meaning there could be any number of runtime-spawned concurrent actors
that must co-ordinate well with each other. In those cases these systems tend to break down.
I've seen consultants describe complex patterns for dynamically introducing actors, one-actor-per-resource systems, tree-based actor resource hierarchies and other complex ideas but in my opinion these systems quickly outgrow the ability of any one developer to understand and debug.

So how then do we model a system like the bank account example? 
Even if we were to limit the system to a fixed number of transfer-workers they'd still be concurrently 
accessing the same data (the bank accounts) and need some way to express **atomic transfers** between them, which isn't easily accomplished with actors or CSP.

What's a guy to do?

## A new (old) synchronization primitive

In the vast majority of cases using a streaming system, actors or CSP is going to be most effective and understandable.
However in cases where we must synchronize individual chunks of data across many workers, and require operations to affect
multiple chunks of data atomically, there's only one name in town that gets the job done right.

Software Transactional Memory (STM) is a criminally under-utilized synchronization tool which solves all of the problems we've encountered so far while providing more safety, better compositionality, and cleaner abstractions. Did I mention they prevent most deadlocks and livelocks too?

To understand how STM works, think of database transactions; in a database transaction isolation provides you with a consistent view of data in spite of concurrent access.
Each transaction sees an isolated view of the data, untampered by other reads and writes. After making all your reads and writes
you _commit_ the transaction. Upon commit, the transaction either succeeds completely and applies _ALL_ the changes you made to the data snapshot, or it may result in a *conflict*. 
In cases of a conflict the transaction *fails* and *rolls back* all your changes as though nothing happened, then it can retry on the new data snapshot.

STM works in much the same way, but instead of the rows and columns in a database, transactions operate on normal in-memory data structures and variables.

To explore this technique let's convert our bank account example into Haskell so we can use STM instead of mutexes.

```haskell
data Account = Account {
  -- Data that needs synchronization is stored in a Transactional Variable, a.k.a. TVar
  balanceVar :: TVar Int
}

-- Deposit money into an account.
deposit :: Account -> Int -> STM ()
deposit Account{balanceVar} amount = do
  -- We interact with the data using TVar operations which
  -- build up an STM transaction.
  modifyTVar balanceVar (\existing -> existing + amount)

-- Withdraw money from an account
-- Everything within the `do` block
-- is part of the same transaction.
-- This guarantees a consistent view of the TVars we 
-- access and mutate.
withdraw :: Account -> Int -> STM Bool
withdraw Account{balanceVar} amount = do
  existing <- readTVar balanceVar
  if existing < amount
    then (return False)
    else do
      writeTVar balanceVar (existing - amount)
      return True

-- Transfer money between two accounts atomically
transfer :: Account -> Account -> Int -> STM Bool
transfer from to amount = do
  -- These two individual transactions seamlessly
  -- compose into one larger transaction, guaranteeing
  -- consistency without any need to change the individual
  -- operations.
  withdrawalSuccessful <- withdraw from amount
  if successful
    then do
      deposit to amount
      return True
    else 
      return False
```

Let's do another lap over all the problems we had with mutexes to see how this new approach fares.

### Data Races

Data races are a problem which I believe are best solved at the language level itself.
As mentioned earlier, using immutable data by default simply prevents data races from existing in the first place.
Since data in Haskell is all immutable by default, pre-emption can occur at any point in normal code and **we know** we won't get a data race.

When we need *mutable data*, it's made explicit by wrapping that data in `TVar`s.
The language further protects us by only allowing us to mutate these variables within 
transactions, which we compose into operations which are guaranteed a consistent uncorrupted view of the data.

Let's convert `withdraw` to use STM and our `balaceVar` TVar.

```haskell
-- Withdraw money from an account
withdraw :: Account -> Int -> STM Bool
withdraw Account{balanceVar} amount = do
  existing <- readTVar balanceVar
  if existing < amount
    then (return False)
    else do
      -- No data races here!
      writeTVar balanceVar (existing - amount)
      return True
```

We can see that the code we wrote looks very much like the original unsynchronized golang version, but while using STM it's perfectly safe from data races!
Even if it the thread is pre-empted in the middle of the operation, the transaction-state is invisible to other threads until the transaction commits.

### Deadlock/Livelock

STM is an **optimistic concurrency system**. This means that threads __never block waiting for locks__. 
Instead, each concurrent operation proceeds, possibly in parallel, on their own independent transaction log. 
Each transaction tracks which pieces of data it has accessed or mutated and
if at commit time it is detected that some other transaction has been committed and altered data which this transaction also accessed, 
then the latter transaction is rolled back and is simply retried. 

This arrangement is fundamentally different from a lock-based exclusive access system. 
In STM, you don't deal with locks at all, you simply read and write data within a transaction as necessary.
Our `transfer` function reads and writes two different `TVar`s, but since we're 
not obtaining exclusive _locks_ to these vars, we don't need to worry about deadlock _at all_. 
If two threads happen to be running a `transfer` on the same `TVars` at the same time, 
whichever commits first will atomically apply its updates to both accounts and the other 
transaction will detect this update at commit-time and will retry against the new balances.

This _can_ cause some contention and possibly even starvation of any single transaction if many threads are trying to update the same data at the same time,
but since a conflict can only occur if some other transaction has been committed, it does still have the guarantee that the system 
will make progress on at least some work. In Haskell, STM transactions must be _pure_ code, and can't do IO, so most transactions are relatively short-running and should proceed eventually. 
This seems like a downside, but in practice it only surfaces as a rare annoyance and can usually be worked around without too much trouble.

### Composition

It may not be immediately obvious from the types if you're not used to Haskell code, but all three of `withdraw`, `deposit`, and `transfer` are all functions which 
return their results wrapped in the `STM` monad, which is essentially a sequence of operations which we can ask to execute in a transaction using the `atomically` function.

We can call out to any arbitrary methods which return something wrapped in `STM` and it will automatically be joined in as part of the current transaction.

Unlike our mutex setup, callers don't need to manually handle locks when calling`withdraw` and `deposit`, nor do we need to expose special __synchronized__ versions of these methods for things to be safe.
We can define them exactly once and use that one definition either on its own or within a more complex operation like `transfer` without any additional work.
The abstraction is leak-proof, the caller doesn't need to know which synchronized data is accessed or lock or unlock any mutexes. 
It simply runs the transaction and the STM system happily handles the rest for you.

Here's what it looks like to actually run our STM transactions, which we do using the `atomically` function:

```haskell
main :: IO ()
main = do
  forever $ do
    req <- acceptTransferRequest
    -- Run each transfer on its own green-thread, in an atomic transaciton.
    forkIO (atomically (transfer req.from req.to req.amount)
```

If we'd like to compile a report of all account balances as we did previously, we can do that too.
This time however we won't get a potentially inconsistent snapshot of the system by accident, 
instead the type-system forces us to make an explicit choice of which behaviour we'd like.  

We can either:

* Access and print each account balance individually as _separate transaction_ which means accounts may be edited in-between transactions, leading to an inconsistent report like we saw earlier.
* Or, we can wrap the entire report into **a single transaction**, reading all account balances in a single transaction. This _will_ provide a consistent snapshot of the system, but due to the optimistic transaction system, the entire transaction will be retried if any individual _transfers_ commit and edit accounts while we're collecting the report. It's possible that if transfers are happening __very__ frequently, the report may be retried many times before it can complete.

This is a legitimate tradeoff that the developer of the system should be forced to consider.

Here's what those two different implementations look like:

```haskell
-- Inconsistent report, may see money disappear/appear
reportInconsistent :: [Account] -> IO ()
reportInconsistent accounts = do
  for_ accounts $ \Account{balanceVar} -> do
    balance <- atomically (readTVar balanceVar)
    print balance

-- Consistent report, may be retried indefinitely if transfers are happening too frequently
reportConsistent :: [Account] -> IO ()
reportConsistent accounts = do
  balances <- atomically do 
    for accounts $ \Account{balanceVar} -> do
      readTVar balanceVar
  -- Now that we've got a snapshot we can print it out
  for_ balances print
```

## Smart Retries

One last benefit of STM which we haven't yet discussed is that it supports *intelligent transaction retries*
based on conditions of the synchronized data itself. 
For instance, if we have a task to withdraw $100 from Alice's account
but it only has $50 in it, the mutex-based system has no choice to but fail the withdrawal entirely and return the failure up the stack.
We can wrap that call with code to try again later, but how will we know when it's reasonable to try again?
This would once again require the caller to understand the _implementation details_, and which locks the method
is accessing.

STM, instead, supports failure and retrying as a first-class concept. At any point in an STM transaction you 
can simply call `retry`, this will record every `TVar` that the
transaction has accessed up until that point, then will abort the current transaction and 
will sleep until any of those `TVar`s 
has been modified by some other successful transaction.
This avoids busy-waiting, and allows writing some very simple and elegant code.

For example, here's a new version of our `withdraw` function which instead of returning a failure will simply 
block the current thread until sufficient funds are available, retrying only when 
the balance of that account is changed by some other transaction's success.

```haskell
-- Withdraw money from an account, blocking until sufficient funds are available
withdraw :: Account -> Int -> STM ()
withdraw Account{balanceVar} amount = do
  existing <- readTVar balanceVar
  if existing < amount
    then retry
    else do
      writeTVar balanceVar (existing - amount)
```

You typically wouldn't use this to wait for an event which may take days or weeks to occur like in this example; but it's a very elegant and efficient solution
for waiting on a channel, waiting for a future to produce a result, or waiting on any other short-term condition to be met.

Here's an example utility for zipping together two STM queues. The transaction will only succeed 
and produce a result when a value is available on both queues, 
and if that's not the case, it will only bother retrying when one of the queues
is modified since `readTQueue` calls `retry` internally if the queue is empty.

```haskell
zipQueues :: TQueue a -> TQueue b -> STM (a, b)
zipQueues q1 q2 = do
  val1 <- readTQueue q1
  val2 <- readTQueue q2
  return (val1, val2)
```

Nifty!

# Conclusion

We've covered a _lot_ in this post, if there's only one thing you can take away from it,
I hope that you've taken the time to consider whether mutexes with shared mutable state are providing you with 
utility which outweighs their inherent costs and complexities.
Unless you need peak performance, you may want to think twice about using such dangerous tools. 
Instead, consider using a concurrency pattern like 
actors, CSP, streaming, or map-reduce if it matches your use-case.

If you need something which provides greater flexibility or lower-level control, 
Software Transactional Memory (STM) is a fantastic choice if it's available in your
language of choice, though note that not all languages support it, or if they do, may
not be able to provide sufficient safety guarantees due to mutable variables and data structures.

If you're starting a new project for which concurrency or parallelism is a first-class concern,
consider trying out a language that supports STM properly, I can recommend Unison
or Haskell as great starting points.
