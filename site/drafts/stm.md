---
title: "Replacing Mutexes"
author: Chris Penner
date: Sep 24, 2025
tags: [programming, haskell]
description: "Mutexes are bad tools, write code so you don't need them."
image: power-small.jpg
---

We find ourselves at an inflection point. Moore's law is dying, beefy single cores are no longer keeping up with our computing needs, so we
need to scale horizontally instead.
Nowadays, most computers come with multiple CPU cores and since individual cores aren't getting much faster year to year, we find ourselves needing to 
utilize multiple cores more effectively.

There are differing schools of thought on how to effectively make use of this new parallelism, 
but I'm here to tell you that sharing mutable data between threads isn't one of them.

For anyone who hasn't run into these problems themselves, let's investigate a simple example to see 
how sharing data causes problems.

## The Problem with Shared State

Let's begin by crafting a simple software system which needs synchronization in the first place. 

I'll present a commonly used example: the task of managing bank account balances correctly in spite of parallel transfer requests.

Obviously real banks don't store all their account balances in RAM, so I'll 
hope that the reader can apply the concepts from this pedagogical example to a their own domain as necessary.

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

Great! Now let's add a method to transfer money between accounts:

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

Things may work well in your tests, and might even work well in production for a while, 
but sooner or later you're going to lose track of money and have some confused and angry customers.

Do you see why?
This brings us to our first synchronization problem to solve, **Data Races**.

### Data races

__Most__ programming languages are imperative with mutable data structures **\[citation needed\]**, 
so passing pointers to multiple threads leads to _shared mutable data_, and _shared mutable data_ necessarily causes _data races_.

A data race occurs any time two threads access the same memory location concurrently and non-deterministically, if at least one of the accesses is a write. In these situations two runs of the same code with the same state may non-deterministically have a different result.

We're passing accounts by reference here, so multiple threads have access to modify the same account.
With multiple transfer go-routines are running on the same account, each could be paused by the scheduler at nearly any point during its execution.
This means that we've got a data race in this seemingly simple withdraw function. Let me point it out:

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

If two threads are withdrawing $100 from Alice's account, which only has $150 in it, it's possible thread 1 checks the balance, sees there's enough money, then gets paused. Thread 2 runs, checks the balance, also sees there's enough money, then withdraws $100. 
When thread 1 resumes execution _after the check_ and withdraws its $100 too, Alice's account ends up with a negative balance of -$50, which is invalid.

This sort of concurrency error is particularly insidious because the original `withdraw` method is perfectly reasonable and idiomatic in a single-threaded program;
however when we decide to add concurrency at a completely different point in the system we've introduced a bug deep within existing previously correct code.

The idea that a perfectly normal evolution from a single-threaded to a multi-threaded program can introduce critical system-breaking bugs in completely unrelated code without so much as a warning is frankly _completely unacceptable_. As a craftsman I expect better from my tools. 

Okay, but now that we've lost thousands if not millions of dollars, how do we fix this?

Enter Mutexes!

### Mutexes

Okay, we've encountered a problem with our shared mutable state, the traditional approach to solving these problems is to enforce _exclusive access_ to the shared data in so-called "critical sections".
Mutexes are so-named because they provide **mut**ual **ex**clusion, meaning only a single thread may access a given virtual resource at a time.

Here's how we can fix the data race problems using a mutex:

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

Now every account has a mutex on it, which acts as an exclusive lock. 

It's like a bathroom key in a busy restaurant.
When you want to use the bathroom, you take the key, there's only one key available for each bathroom, so while you've got hold of it 
nobody else can use that bathroom.
Now you're free to do your business, then you return the key to the hook on the wall for the next person.

Unlike a bathroom key however, mutexes are only _conceptual_ locks, not _real_ locks, and as such they operate on the honor system. 

If you forget to lock the mutex the system won't stop you from accessing the data anyways, there's no actual link between
the data being locked and the lock itself, we need to trust the programmers to respect the agreement (always a risky prospect).

In this case, we've addressed the data-race within `withdraw` and `deposit` using mutexes, 
but we've still got a problem within the `transfer` function.

We're properly using **mutexes** to protect our critical code within each smaller operation, but let's revisit the `transfer` function. What happens if a thread is pre-empted while running the `transfer` function between the calls to `withdraw` and `deposit`? It's possible that money has been withdrawn from an account, but hasn't yet been deposited in the other. 
This is an inconsistent state of the system, as the money has temporarily disappeared. This can result in _very_ strange behaviour.

Let's say we have a `report` function which prints out all account balances. If it runs while transfers are happening, we might see that the total money we've counted within the system is incorrect, even if we obtain the locks for each individual account.

```go
func report() {
    for _, account := range accounts {
        account.mutex.lock()
        fmt.Println(account.balance)
        account.mutex.unlock()
    }
}
```

In larger systems this can even result in flawed logic taking place, since choices may be made against inconsistent system states.
The issue is that a single operation requires multiple independent locks, and they're not grouped in any way into an atomic operation.

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

`transfer` won't even begin to run in this state, it will block forever when it tries to lock the `from.mutex` for the second time.

Some systems, like _re-entrant locks_ and Java's `synchronized` keyword do some additional book-keeping which
allow a single thread to lock the same mutex multiple times, so using a re-entrant lock here would solve this particular problem.
However other systems, like golang, avoid providing re-entrant locks [on a matter of principle](https://groups.google.com/g/golang-nuts/c/XqW1qcuZgKg/m/Ui3nQkeLV80J).

So what can we do? I suppose
we'll need to pull the locks out of `withdraw` and `deposit` so we can 
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

Ugh, defining a correct `transfer` function, which conceptually is just the _composition_ of our well encapsulated `withdraw` and a `deposit` has forced us to remove the locking from both `withdraw` and `deposit`, making both of them _less safe_ to use. It has placed the burden of locking on the caller (without _any_ system-maintained guarantees), and even worse, we now need to remember to go and _add_ locking around every existing `withdraw` and `deposit` call in the entire codebase.

Mutexes don't _compose_! They don't allow us to chain multiple critical sections into a single atomic unit and they force us to break encapsulation and thrust the implementation details of mutexes and locking onto the caller who really shouldn't need to know which invariants must be maintained deep within the implementation. Not to mention that adding or removing access to synchronized variables within an operation now necessitates adding or removing locking to every call site, and those call sites may be in a completely different application or library.
This is an absolute mess.

But would you believe that's actually not the only problem here? 
It's not just composition that's broken here though, in fixing `transfer` to make it an atomic operation we've introduced a new, extra-well-hidden deadlock bug.

### Deadlocks/Livelocks

Remember, in our main loop we're accepting arbitrary transfer requests and spawning them off in goroutines.

What happens in our system if we have two transfer requests, Alice is trying to Venmo Bob $25 for the beanbag chair she just bought off him, meanwhile Bob remembers he needs to Venmo Alice the $130 he owes her for Weird Al concert tickets.

If they both submit their requests at the same time, we have two `transfer` calls:

* `transfer(aliceAccount, bobAccount, 25)`
* `transfer(bobAccount, aliceAccount, 130)`

Each of these calls will attempt to lock their `from` account and _then_ their `to` account.
If Alice and Bob get very unlucky, the system will start the first `transfer` and lock Alice's account, then get paused.
When the second `transfer` call comes in, it first locks Bob's account, then tries to lock Alice's account, but can't because it's already locked by the first `transfer` call.

This is a classic deadlock situation. This leaves both goroutines stuck forever, and worse, both Alice and Bob's accounts will be locked until the system restarts.

This is a pretty disastrous consequence for a problem which is relatively hard to spot even in this trivially simple example. In a real system with dozens or hundreds of methods being parallelized in a combinatorial explosion of ways it's very difficult to reason about this, and can be a lot of work to ensure locks are obtained in a safe and consistent order.

Golang gets some credit here in that it does provide _some_ runtime tools for detecting both dead-locks and data-races, which is great, but these detections only help if your tests encounter the problem; they don't prevent the problem from happening in the first place. Most languages aren't so helpful, these issues can be very difficult to track down in production systems.

### Assessing the damage

What a mess.

In my experience, given enough time and complexity these sorts of problems will crop up eventually.
Solving them with mutexes is especially dangerous because they _seem_ to be an effective solution when they're first introduced. 
They work fine in small units, tempting us to use them, but as the system grows organically we stretch them too far and they fail catastrophically once operating at scale, causing all sorts of hacky workarounds. Crossing your fingers is not 
an adequate software-engineering strategy.

So, architecting a correct software system using mutexes is possible, but _very_ difficult. 
Every attempt we've made to fix one problem has spawned a couple more. 

Here's a summary of the problems we've encountered:

* Data races
* Non-atomicity/inconsistent system states
* Lack of composition
* Leaking abstractions
* Deadlocks/livelocks
* Every change or new addition requires an understanding of the system as a whole

In my opinion, we've tried to stretch mutexes beyond their limits, both in this blog post and 
in the industry as a whole.
Mutexes work great in small, well-defined scopes where you're locking a _single_ resource which is only ever accessed in a handful of functions in the same module,
but they simply don't scale to large complex systems with many interacting components maintained by dozens or hundreds of developers. We 
need to evolve our tools and come up with more reliable solutions.

## Cleaning up the Chaos

Thankfully, despite an over-reliance on mutexes, we as an industry _have_ still learned
a thing or two since the 1960s.
Particularly I think that enforcing _immutability by default_ goes a _long_ way here. 
For many programmers this is a paradigm shift from what they're used to, causing some uneasiness. 
Seatbelts, too, were often scorned in their early years for their restrictive nature, but over time 
it has become the prevailing opinion that the mild inconvenience is more than worth the provided safety.

More and more languages (Haskell, Clojure, Erlang, Gleam, Elixir, Roc, Elm, Unison, ...) are realizing this 
and are adopting this as core design principle. 
Using immutable data structures immediately prevents data-races, full-stop.
While these languages typically do provide some form of mutable references, it's not the default, and there's typically some additional 
ceremony which acts as an immediate sign-post that shared-mutable state is involved, here there be dragons.

Obviously not every programmer can switch to an immutable-first language over night, but I think it would behoove most
programmers to strongly consider an immutable language if parallelism is a large part of their project's workload.

In a world of immutability we'll still need some way to synchronize our parallelism.

Decades of research and industrial research have provided us with a swath battle-tested options. 
Interestingly, because our data is immutable, most of these operate one level higher than mutexes 
as architectural patterns which co-ordinate processes rather than individual atoms of data.

### Concurrency Patterns

Actor systems and Communicating Sequential Processes (CSP) are some of the most common concurrency orchestration patterns.
Each of these operate by defining independent sub-programs which have
their own isolated states that only they can access. Each actor or process receives messages from other units
and can respond to them in turn.

These approaches work great for _task parallelism_, where there are independent processes to run, and where your parallelism needs are bounded
by the number of tasks you'd like to run. 
For instance I used an actor-based system when building Unison's code-syncing protocol. There was one actor responsible for loading and sending 
requests for code, one for receiving and unpacking code, and one for validating the hashes of received code.

I reach for actor and CSP systems when the number of workers/tasks we need to co-ordinate is statically known, i.e. a fixed number of workers, or a pre-defined map-reduce pipeline.
As an added benefit these approaches, which have no shared mutable state, can 
be ported to a truly distributed system involving multiple nodes _much_ easier than
shared-memory approaches.

However, in cases where the parallelism is _dynamic_, meaning there could be any number of runtime-spawned concurrent actors
that must co-ordinate well with each other these systems tend to break down.
I've seen consultants describe complex patterns for dynamically introducing actors, actor-per-resource systems etc, 
but in my opinion these systems quickly outgrow the ability of any one developer to understand and debug.

So how then, do we model a system like the bank account example? 
Even if we were to limit the system to a fixed number of transfer-workers they'd still be concurrently 
accessing the same data (the bank accounts) and need some way to express **atomic transfers** between them.

What's a guy to do?

## A new (old) synchronization primitive

In the vast majority of cases using a streaming system, actors or CSP is going to be most effective and understandable.
However in cases where we must synchronize individual chunks of data across many workers, and require operations to affect
multiple chunks of data atomically, there's only one name in town that gets the job done right.

Software Transactional Memory (STM) is a criminally under-utilized synchronization tool which solves all of the problems we've encountered so far with mutexes while providing more safety, better compositionality, and cleaner abstractions. Did I mention they prevent most deadlocks and livelocks too?

Think of database transactions; in a database transaction isolation provides you with a consistent view of data in spite of concurrent access.
Each transaction sees an isolated view of the data, untampered by other reads and writes. After making all your reads and writes
you _commit_ the transaction. Upon commit, the transaction either succeeds completely, applying changes to the actual data snapshot, or it may result in a conflict. 
In cases of a conflict the transaction fails and rolls back all your changes as though nothing happened, then may retry on the new data snapshot.

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
Pre-emption can occur at any point in normal code and we know we won't get a data race because all the data is immutable.

When we need mutable data, it's made explicit by wrapping that data in `TVar`s, 
and the language further protects us by only allowing us to mutate these variables within 
transactions, which we compose into operations which are guaranteed a consistent uncorrupted view of the data.

Looking at our withdraw function, we see that we can write code that looks very much like the original un-synchronized golang version, but in STM it's perfectly safe from data races since we're using `TVar`s within an `STM` Transaction.

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

### Deadlock/Livelock

STM is an optimistic concurrency system, which means that threads __never block waiting for locks__. 
Instead, each concurrent operation proceeds, possibly in parallel, on their own independent transaction log. 
Each transaction tracks which pieces of data it has accessed or mutated and
if at commit time it is detected that some other transaction has been committed and altered data which this transaction also accessed, 
then the latter transaction is rolled back and is simply retried. 

This arrangement is fundamentally different than a lock-based exclusive access system. 
In STM, you don't deal with locks at all, you simply read and write data within a transaction as necessary.
Our `transfer` function reads and writes two different `TVar`s, but since we're 
not obtaining exclusive _locks_ to these vars, we don't need to worry about deadlock _at all_. 
If two threads happen to be running a `transfer` on the same `TVars` at the same time, 
whichever commits first will atomically apply its updates to both accounts and the other 
transaction will detect this update at commit-time and will retry against the new balances.

This _can_ cause some contention and possibly even starvation of any single transaction if many threads are trying to update the same data at the same time,
but since a conflict can only occur if some other transaction has been committed, it does still have the guarantee that the system 
will make progress on at least some work.

### Composition

It may not be immediately obvious from the types if you're not used to Haskell code, but all three of `withdraw`, `deposit`, and `transfer` are all functions which 
return their results wrapped in the `STM` monad, which is essentially a sequence of operations which we can ask to execute in a transaction using the `atomically` function.

We can call out to any arbitrary methods which return something wrapped in `STM` and it will automatically be joined in as part of the current transaction.

Unlike our mutex setup, callers don't need to manually handle locks when calling`withdraw` and `deposit`, nor do we need to expose special __synchronized__ versions of these methods for things to be safe.
We can define them exactly once and use them on their own or within a more complex operation like `transfer` without any additional work.
This abstraction is leak-proof, the caller doesn't need to know which synchronized data is accessed or lock or unlock any mutexes. 
It simply runs the transaction and the STM system happily handles the rest for you.

Here's what it looks like to actually run our STM transactions, which we do using the `atomically` function:

```haskell
main :: IO ()
main = do
  forever $ do
    req <- acceptTransferRequest
    forkIO (atomically (transfer req.from req.to req.amount)
```

If we'd like to compile a report of all account balances like we did previously, we can do that too.
This time however we won't get a potentially inconsistent snapshot of the system by accident, 
instead the type-system forces us to make an explicit choice of which behaviour we'd like.  

We can either:

* Access and print each account balance individually, which means transfers may be editing account balances in between each individual transaction, leading to a printout that doesn't represent a single snapshot in time
* Or, we can wrap the entire report in a single transaction, reading all account balances in a single transaction. This _will_ provide a consistent snapshot of the system, but due to the optimistic transaction system, the entire transaction will be retried if any _transfers_ commit while we're collecting the report. It's possible that if transfers are happening very frequently, the report may be retried many times before it can complete (if at all).

This is a legitimate tradeoff that the developer of the system should be forced to consider.

Here's what those two different implementations look like:

```haskell
-- Inconsistent report, may see money disappear/reappear
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

In a mutex-based system, if you try to obtain a lock and it's not available, you will block until it is.
The operating system will typically schedule other work, then, when the lock becomes available, any threads blocked on that lock will be woken up and try to obtain the lock again.

This is great, it prevents busy-waiting, but it's also a bit rough-grained.




* STM tracks dependencies between transactions and TVars
* You can manually retry a transaction based on any programmatic condition






Problems with traditional concurrency models:

* Deadlock
* Don't compose
* Did I mention they don't compose?
* Many different abstractions to learn that don't always work well together
* Not always exception safe
* Ecosystem fragmentation
* Data races
* Mutable by default

Benny's of STM:
Eco-system wide adoption,
Allows mixing and matching of different concurrency patterns, and even data structures!

* semaphores
* Mutexes
* Dining Philosopher's problem
* Concurrent sequential processes
* go-routines
* actors
* supervision trees
* errgroup/waitgroup
* channels

Synchronization Primitives:

* Read-write locks (RWMutex)
* Condition variables
* Barriers
* Spinlocks
* Atomic operations

Concurrency Models:

* Software Transactional Memory (STM)
* Communicating Sequential Processes (CSP)
* Structured Concurrency
* Fork-join parallelism
* Futures/Promises
* Async/await
* Dataflow programming

Problems & Patterns:

* Async/Await
* Producer-consumer problem
* Readers-writers problem
* Sleeping barber problem
* Cigarette smokers problem
* Deadlock detection/prevention
* Race conditions
* Thread pools
* Work stealing

Advanced Concepts:

* Lock-free data structures
* Compare-and-swap (CAS)
* Memory models & ordering
* Green threads vs OS threads
* Context switching
* Thread-local storage
* Coroutines

Language-Specific:

* Select statements
* Fan-in/fan-out patterns
* Pipeline patterns
* Worker pools


## Cons

Here-in lies Haskell's greatest strength and greatest tradeoff.

STM works on the principle that all mutation in the concurrent system is contained to data within its `TVar`s. For this reason, STM provides _much_ more safety within languages which use immutable data structures by default, like Haskell.

It _is_ still possible to implement STM systems in languages with mutable data, but it requires much stricter discipline on the part of the programmer.
I suppose it's no more difficult than remembering to contain access to resources within their appropriate mutexes, but if you want the full benefits of STM then you'll need to use a pure functional language like Haskell.

I'll hold my tongue and restrain myself from extolling the many other benefits that immutable data provides, suffice it to say I believe most folks who learn to use it find in practice it's not a restriction but rather provides much greater readability and maintainability to their code.

Data races are solved by simply _never sharing mutable state_. It's really not such a big burden. Haskell provides this guarantee by default, the entire language is based on immutable data, so feel free to share your data between threads however you like.

STM is has the overhead of tracking everything and maintaining transactions. Bearable by most applications, but
may not be suitable for extremely low-level systems.

No guarantee of fairness, STM guarantees that the _system_ will continue to process work, but if you have multiple 
threads trying to update the same data, it's possible that one thread may be starved indefinitely while others keep succeeding.

STM isn't a silver bullet, it uses optimistic locking so may be a bad choice for certain high-contention scenarios where providing proper exclusive access may be a better fit.



## Concurrency patterns scale better

If you're fuzzy on the difference, now would be a good time to refresh on the difference between [parallelism and concurrency](https://wiki.haskell.org/index.php?title=Parallelism_vs._Concurrency). 

Concurrency is the idea of **co-ordinating** multiple independently tasks, whereas
__parallelism__ is the idea of executing multiple tasks simultaneously. So you can use concurrency to wait on mutliple network requests to finish and collect their results, which works even on a single-core machine, but it's parallelism to partition a large array across multiple cores to sum its contents at the same time.

Computer scientists have been thinking about concurrency for a __long__ time, Computing has prioritized simpler single-threaded execution environments for decades, so research in this area is much further ahead. We have a whole collection of tools and patterns for handling concurrency. There's a rich landscape of well-researched patterns like communicating sequential processes (CSP), the actor model, structured concurrency, event loops and supervision trees. There's also a set of common tools which provide strong abstractions to build upon, like select/poll, co-routines (e.g. async/await), futures/promises,  errgroup/waitgroup, and so on.

So what happens when we add parallelism to the mix? Some patterns handle this by having each process own its own independent memory, for example actors and CSP. 
This avoids the need for synchronization, and works great in cases where you have many parallel _processes_ to run, but it breaks down in cases when running some few processes on a large amount of data.

Let's imagine the traditional parallel programming problem of managing many bank accounts, we want to process many concurrent transfer requests between accounts, while ensuring that the total amount of money in the system remains constant, and that no balances go negative.

Actor-based and CSP systems have a tough time with this sort of thing; typically you'd end up with an actor per account which manages requests to read and write to its balance. Each transfer would require sending messages to at least two different actors. 
Not only does this require managing an actor process for every account which can be rough on the scheduler and on memory, it's also _very_ difficult to ensure that transfers are atomic and that the system remains consistent in the face of failures and retries. You need additional patterns like two-phase commit to add guarantees that money won't be lost or created out of thin air.
At this point, you're no longer writing code in your programming language, you're now writing actor-code to build a distributed system.
TODO: fixup the above

With parallel, shared-memory systems, we now need ways to synchronize access to shared resources, and this is where things get tricky.
Way back in the 1960s Edsger Dijkstra first proposed the concept of semaphores as a locking mechanism for providing exclusive access to critical sections of code in concurrent systems.

60 years later, semaphores and mutexes (which is a binary semaphore) are _still_ the core synchronization primitives used in most programming languages.

Did Dijkstra really perfect his approach on the first try? Have systems really not evolved in a half century?

More likely, I think, is that inventing new approaches is difficult, and risks alienating users who are used to the status quo.
As a result, each new language simply copy-pastes existing concurrency ideas, perhaps providing some incremental improvements.

Managing share-memory parallelism in a large software system is extremely difficult and introduces copious bugs which are both expensive and difficult to find.

As an industry we should strive for good, reliable tools which are _easy to use_ and which, by design, make it _more difficult to make mistakes_.
This post serves to document _why_ mutexes simply aren't cutting it anymore as the one-size-fits-all tool for synchronization, and to shine some additional light on the best tool I've found so far. Perhaps it will also serve to inspire others to keep looking for better solutions rather than accept mutexes as the state-of-the-art solution.

So, what's the deal with mutexes anyways? Lock it, do the thing, unlock it again, simple right? What could possibly go wrong...



## Conclusion


Summary:

* In almost all modern languages with mutable data, switching a program from single-threaded to concurrent requires you to reconsider the _entirety_ of the newly concurrent codepaths without providing any help. All of your carefully crafted code could now be subject to a data-race or worse.
* Mutexes and locks break encapsulation. They force you to break encapsulation and leak implementation details, forcing the caller to have an intimate knowledge of the system's concurrency invariants.
* Mutexes don't compose. You can't safely build larger atomic operations out of smaller ones without re-architecting existing code. Obtaining multiple locks when accessing multiple resources is error-prone with disastrous consequences. The author of a new operation may not even be aware that they've obtained locks to multiple resources because resource accesses may be scattered anywhere within their call graph.
* Deadlocks and livelocks are difficult to resolve and even more difficult to detect and hunt down. 

More on the topic:

https://danluu.com/concurrency-bugs/

It mentions many tools, but AFAIK none of them claim to detect, never mind _prevent_ 
entire classes of these bugs.




----

# Graveyard


Moore's law is dead, modern computing is multi-threaded, distributed, and there's probably a network call happening on your machine after every few key-presses,
gone are the days where a single core executes a single contiguous task all the way to completion.
We live in the age of parallelism, where it's far easier to scale horizontally than vertically, whether that means adding CPU cores or entire servers.
In the timeline of programming languages however, this paradigm shift away from fast single cores is still relatively new and languages are still adapting to it. 
Tech moves quickly, but fundamental abstractions usually don't. 



This isn't a new concept, it ori 

> Don't communicate by sharing memory; share memory by communicating.
> -- Rob Pike


