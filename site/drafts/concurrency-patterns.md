---
title: "The best concurrent programming language"
author: Chris Penner
date: Sep 24, 2025
tags: [programming, haskell]
description: ""
image: power-small.jpg
---

Moore's law is dead, modern computing is multi-threaded, distributed, and there's probably a network call happening on your machine after every few key-presses.

Gone are the days where a single core executes a single contiguous task all the way to completion.

In the timeline of programming languages however, this revolution is still a relatively new development. 
Tech moves quickly, but fundamental abstractions usually don't. 

Notably, at the time of writing Javascript is _still_ single-threaded in the browser, and Python's global interpreter lock (GIL) has hampered true concurrency for decades.

Way back in the 1960s Edsger Dijkstra first proposed the concept of semaphores as a locking mechanism for providing exclusive access to critical sections of code in concurrent systems.
Now, 60 years later, semaphores and mutexes (which is a binary semaphore) are _still_ the core concurrency primitives in most programming languages.

Did Dijkstra really perfect his approach on the first try? Have systems really not evolved in 60 years?

More likely, I think, is that inventing new approaches is difficult, and it's both risky and alienating to introduce new ideas.
As a result, each new language simply copy-pastes existing concurrency ideas, perhaps providing some incremental improvements.

Managing concurrency is a large software system is extremely difficult and introduces copious bugs which are both expensive and difficult to find.

As an industry we should strive for good, reliable tools which are _easy to use_ and which, by design, make it _more difficult to make mistakes_.
This post serves to document _why_ mutexes simply aren't cutting it anymore, and to shine some additional light on the best approach I've found. Perhaps it will also serve to inspire others to keep looking for better solutions rather than accept the current state-of-the-art.

So, what's the deal with mutexes anyways? Lock it, do the thing, unlock it again, simple right? What could possibly go wrong...

## The Concurrency Problem(s)

Let's begin by crafting a simple software system which induces the need for synchronization in the first place, then we'll try to fix it with mutexes and see how that goes.

I'll present a common concurrency example: the task of managing bank account balances correctly in spite of concurrent transfer requests.

Obviously real banks don't store all their account balances in RAM, but I'll trust in the reader's intelligence enough to 
translate from this pedagogical example into their more realistic domain of choice.

Here's some golang pseudo-code for a simple bank account and the operations upon it.
I'm focused on the concurrency problems here, so forgive me for skipping the double-entry accounting, input validation, and other real-world complexities.

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

Things may work well in your tests, and might even work well in production for a while, but sooner or later you're going to lose track of money and have some confused and angry customers.

This brings us to our first synchronization problem to solve, Data Races!

### Data races

__Most__ programming languages are imperative with mutable data structures **\[citation needed\]**. 
Mutable data necessarily brings along data-races.

A data race is any time two threads access the same memory location concurrently and non-deterministically, when at least one of the accesses is a write.

We're passing accounts by reference here, so multiple threads have access to modify the same account.
With multiple transfer go-routines are running on the same account, each could be paused by the scheduler at nearly any point during its execution.
This means that in this seemingly simple withdraw function, we've actually got a data race. Let me point it out:

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
When thread 1 resumes execution and withdraws its $100 too, Alice's account ends up with a negative balance of -$50, which is invalid.

This sort of concurrency error is particularly insidious because the original `withdraw` method is perfectly reasonable and idiomatic in a single-threaded program;
however when we decide to add concurrency at a completely different point in the system we've introduced a bug deep within existing previously correct code.

The idea that a perfectly normal evolution from a single-threaded to a multi-threaded program can introduce critical system-breaking bugs in completely unrelated code without so much as a warning is frankly _completely unacceptable_. As a craftsman I expect better from my tools.

Okay, but now that we've lost thousands if not millions of dollars, how do we fix this?

Mutexes are the solution espoused to fix this issue, if you remember to use them, and indeed if you use them _correctly_.

Here's how we can fix the above code using a mutex:

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

Unlike a bathroom key however, mutexes are only _conceptual_ locks, an they only operate on the honor system. 
If you forget to lock the mutex the system won't stop you from accessing the data anyways. Only the program author knows _what_ they actually intend that mutex to conceptually control (and hopefully that programmer documented it somewhere).

In this case, we've addressed the data-race within `withdraw` and `deposit` using mutexes, but we've still got a problem with the `transfer` function.

We're properly using mutexes to protect our critical code within each smaller operation, but it's still possible for our `transfer` function to be pre-empted in-between the calls to `withdraw` and `deposit`, in this case it's possible that money has been withdrawn from an account, but hasn't yet been deposited in the other. 
This is an inconsistent state of the system, as the money has temporarily disappeared.

If we print out all account balances at any time while transfers are happening, we might see that money is missing from the system, even if we obtain the locks for each individual account.

```go
func report() {
    for _, account := range accounts {
        account.mutex.lock()
        fmt.Println(account.balance)
        account.mutex.unlock()
    }
}
```

In order to fix this, we need to chain together multiple critical sections into a single atomic operation.

### Composition

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
allow a single thread to lock the same mutex multiple times, so using a re-entrant lock here would solve this problem.

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

Ugh, defining a correct `transfer` function, which conceptually is just the _composition_ of our well encapsulated `withdraw` and a `deposit` has forced us to remove the locking from both `withdraw` and `deposit`, making both of them _less safe_ to use. It has placed the burden of locking on the caller (without any system-maintained guarantees), and even worse, we now need to remember to go and _add_ locking around every existing `withdraw` and `deposit` call in the entire codebase.

Mutexes don't _compose_. They don't allow us to chain multiple critical sections into a single atomic unit and they force us to break encapsulation and thrust the implementation details of mutexes and locking onto the caller, who really shouldn't need to know which invariants must be maintained deep within the implementation.

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

This is a pretty disastrous consequence for a problem which is relatively hard to spot even in this trivially simple example. In a real system with dozens or hundreds of methods it's very difficult to reason about this, and can be a lot of work to ensure locks are obtained in a safe and consistent order.

Golang gets some credit here in that it does provide some runtime tools for detecting both dead-locks and data-races, which is great, but these detections only help if your tests encounter the problem; they don't prevent the problem from happening in the first place.

What a mess.

### How do we fix it?

As we've seen, architecting a correct concurrent software system using mutexes is possible, but _very_ difficult. 
Every attempt to fix one problem has spawned a couple more. 

In my opinion, we've tried to stretch mutexes beyond their limits.
Mutexes work great in small, well-defined scopes where you're locking a _single_ resource which is only ever accessed in a handful of functions in the same module,
but they simply don't scale to large complex systems with many interacting components maintained by dozens or hundreds of developers.

Time to rebuild things from the foundations.

## Software Transactional Memory

For _most_ concurrent applications, Software Transactional Memory (STM) is __*the best*__ currently available solution to **all** of these problems and _it's not even close_.

Think of database transactions; transaction isolation allows you a consistent view of data in spite of concurrent access.
Each transaction sees an isolated view of the data, untampered by other reads and writes. After making all your reads and writes
you _commit_ the transaction. Upon commit, the transaction either succeeds completely, applying changes to the data snapshot, or it may detect a conflict. Upon a conflict the transaction fails and rolls back all your changes as though nothing happened, then may retry on the new data snapshot.

STM works in much the same way, but instead of rows and columns in a database, you use transactions on your normal in-memory data structures and variables.

Unlike our mutex workflows, these transactions are composable, atomic, and help in preventing and detecting deadlock.

Let's convert our bank account example into Haskell using STM instead of mutexes.

```haskell
data Account = Account {
  -- All mutable data is stored in a Transactional Variable, a.k.a. TVar
  balanceVar :: TVar Int
}

-- Deposit money into an account
deposit :: Account -> Int -> STM ()
deposit Account{balanceVar} amount = do
  modifyTVar balanceVar (\existing -> existing + amount)

-- Withdraw money from an account
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
  -- These two operations are contained within the same
  -- transaction (see the STM type in the signature).
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

Data races area  problem which I believe are best solved at the language level itself.

In a language like Haskell, data races are solved by simply _never sharing mutable state_. Haskell provides guarantee by default, the entire language is based on immutable data, so you're free to share your data between threads however you like.

In other languages it does require some discipline to ensure that all mutable state is contained within `TVar`s, but I suppose this is no more difficult than remembering to contain access to resources within their appropriate mutexes.

In practice this means that you just literally can't _mutate state_ to cause a problem for some other thread. The language simply doesn't provide a mechanism to do it without explicitly wrapping your state in an `IORef`s, which you'd have no reason to do unless you explicitly _want_ to accept the tradeoffs and dangers that come with traditional mutation.

Looking at our withdraw function, we see that we can write code very much like the original un-synchronized golang version, but in STM it's perfectly safe from data races since we're using TVars within a Transaction.

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

STM is an optimistic concurrency system, which means that threads never block waiting for locks. 
Instead, each concurrent operation proceeds on their own independent transaction log. If, at commit time it is detected that some other transaction has been committed and altered data which this transaction read, then the transaction is rolled back and retried. This can be accomplished efficiently 
by tracking access to all `TVar`s within a transaction.

This arrangement is fundamentally different than a lock-based exclusive access system. In STM, you don't deal with locks at all, you simply read and write data within a transaction as necessary.
Our `transfer` function reads and writes two different `TVar`s, but since we're not obtaining exclusive _locks_ to these vars, we don't need to worry about deadlock _at all_ here. If two threads are running a `transfer` on the same vars at the same time, whichever commits first will atomically apply its updates to both accounts, and the other transaction will detect this update at commit-time and will retry against the new balances.

A conflict can only occur if some other transaction has succeeded, so while it _is_ possible that one particular transfer may be stalled for a while (which is true of a mutex based system as well), the system is guaranteed to always be making progress on at least some thread.

### Composition

It may not be immediately obvious from the types if you're not used to Haskell code, but all three of `withdraw`, `deposit`, and `transfer` are all functions which 
return their results wrapped in the `STM` monad, which is essentially a sequence of operations which we can ask to execute in a transaction using the `atomically` function.

We can call out to any arbitrary methods which return something wrapped in `STM` and it will automatically be part of the same transaction.

So, unlike our mutex setup, callers don't need to manually handle locks when calling`withdraw` and `deposit`, nor do we need to expose special __synchronized__ versions of these methods for things to be safe.

We can define them exactly once and use them on their own or within a more complex operation like `transfer` without any additional work.

Here's what it looks like to actually run our STM transactions.

```haskell
main :: IO ()
main = do
  forever $ do
    req <- acceptTransferRequest
    forkIO (atomically (transfer req.from req.to req.amount)
```

If we'd like to compile a report of all account balances like we did previously, we can do that too.
This time however we won't default in to getting a potentially inconsistent snapshot of the system, we 
are faced with an explicit choice to make: what are the bounds of our transaction going to be?

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
