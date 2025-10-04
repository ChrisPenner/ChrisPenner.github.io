---
title: "The best concurrent programming language"
author: Chris Penner
date: Sep 24, 2025
tags: [programming, haskell]
description: ""
image: power-small.jpg
---

Moore's law is dead, modern computing is distributed, and there's probably a network call happening on your machine after every few key-presses.

Gone are the days where a single core executing a single contiguous task to completion.

However, in the timeline of programming languages, this is still a relatively new development. Tech moves quickly, but fundamental abstractions usually don't. Old languages are built on old paradigms, but need to graft on new concepts to their aging design documents in order to stay relevant.

As a result, most programming languages have a questionable concurrency stories. 

Javascript is _still_ single-threaded in the browser, and Python's global interpreter lock (GIL) has hampered true concurrency for decades.

For those languages that do provide options, either you need to be an expert on arcane techniques, knowing how to avoid the copious pitfalls and gotchas, or the model is overly simplistic and can't operate at scale or provide enough control.

While I'm sure I probably don't need to convince any modern programmer that concurrency is hard and error prone, let me be a bit more specific about the problems we can expect to encounter.

## The Concurrency Problem(s)

Let's look at a common concurrency example: the task of managing bank account balances correctly.
Obviously real banks don't store all their account balances in RAM, but I'll trust in the reader's intelligence enough to 
translate from this simple example into their real domain of choice.

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
func transfer(from *Account, to *Account, amount int) {
  from.withdraw(amount)
  to.deposit(amount)
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

This brings us to our first offender, Data Races!

### Data races

__Most__ programming languages are imperative with mutable data structures **\[citation needed\]**. 
Mutable data necessarily brings along data-races.

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

If two threads are withdrawing $100 from account *A*, which only has $150 in it, it's possible thread 1 checks the balance, sees there's enough money, then gets paused. Thread 2 runs, checks the balance, also sees there's enough money, then withdraws $100. 
When thread 1 resumes execution and withdraws its $100 too, the account with an invalid negative balance of -$50.

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
If you forget to lock the mutex the system won't stop you from accessing the data anyways because only the program author knows _what_ they actually intend that mutex to conceptually control (and hopefully that programmer documented it somewhere).

So in this case, we've addressed the data-race within `withdraw` and `deposit`, but we've still got a problem with the `transfer` function.

We're properly using mutexes to protect our critical code within each smaller operation, but it's still possible for our `transfer` function to be pre-empted in-between the calls to `withdraw` and `deposit`, in this case it's possible that money has been withdrawn from an account, but hasn't yet been deposited in the other. 
This is an inconsistent state of the system, as the money has temporarily disappeared.

If we print out all account balances at any time while transfers are happening, even if we obtain the proper mutex locks we might see that money is missing from the system.

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
func transfer(from *Account, to *Account, amount int) {
  from.mutex.lock()
  to.mutex.lock()
  defer from.mutex.unlock()
  defer to.mutex.unlock()

  from.withdraw(amount)
  to.deposit(amount)
}
```

I'm sure some readers have already seen a problem here, but have you seen _two_ problems here?

The first is obvious when you point it out, remember that `withdraw` and `deposit` _also_ lock the mutex on the account, so we're trying to acquire the same lock twice in the _same thread_. 

`transfer` won't even begin to run in this state; so what can we do? I suppose
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

func transfer(from *Account, to *Account, amount int) {
  from.mutex.lock()
  to.mutex.lock()
  defer from.mutex.unlock()
  defer to.mutex.unlock()

  from.withdraw(amount)
  to.deposit(amount)
}
```

Ugh, this is also unacceptable. Defining a correct `transfer` function, which conceptually is just the _composition_ of our well encapsulated `withdraw` and a `deposit` has forced us to change the implementation of both `withdraw` and `deposit`, has made both of them _less safe_ to use, has placed the burden of locking on the caller (without any system-maintained guarantees), and even worse, we now need to remember to go and _add_ locking around every existing `withdraw` and `deposit` call in the entire codebase.

This is what I mean when I say that mutexes don't _compose_. Not only do they not allow us to chain multiple critical sections into a single atomic unit, but they also force us to break encapsulation and force the implementation details of mutexes and locking onto the caller, who really shouldn't need to know which invariants must be maintained deep within the implementation.

It's not just composition that's broken here though, in fixing `transfer` to make it an atomic operation we've introduced a new, extra-well-hidden deadlock bug!

### Deadlocks/Livelocks

In our main loop we're accepting arbitrary transfer requests and spawning them off in goroutines.

What happens in our system if we have two transfer requests, Alice is trying to Venmo Bob $25 for the beanbag chair she just bought off him, meanwhile Bob remembers he needs to Venmo Alice the $130 he owes her for Weird Al concert tickets.

If they both submit their requests at the same time, we have two `transfer` calls:

* `transfer(aliceAccount, bobAccount, 25)`
* `transfer(bobAccount, aliceAccount, 130)`

Each of these calls will attempt to lock their `from` account and _then_ their `to` account.
If Alice and Bob get very unlucky, the system will start the first `transfer` and lock Alice's account, then get paused.
When the second `transfer` call comes in, it first locks Bob's account, then tries to lock Alice's account, but can't because it's already locked by the first `transfer` call.

This is a classic deadlock situation. This leaves both goroutines stuck forever, and worse, both Alice and Bob's accounts will be locked until the system restarts.

This is a pretty disastrous consequence for a problem which is relatively hard to spot even in this trivially simple example. In a real system with dozens or hundreds of methods it's very difficult to reason about this, and can be a lot of work to ensure locks are obtained in a safe and consistent order.

Golang does get credit here in that it does provide some runtime tools for detecting both dead-locks and data-races, which is great, but these detections only help if your tests encounter the problem; they don't prevent the problem from happening in the first place.

What a mess.

## How do we fix it?

As we've seen, every attempt to fix one problem has spawned a couple more. 
In decades of 

## Software Transactional Memory

Software Transactional memory is the best currently available solution to ALL of these problems and _it's not even close_.

STM provides the database-like ability to perform operations on your application's data and data structures in transactions which either commit as a consistent atomic unit or roll back all their changes as though nothing happened.

Here's how our bank account example looks using STM:

```haskell
data Account = Account {
  -- All mutable data is stored in a Transactional Variable, a.k.a. TVar
  balance :: TVar Int
}

-- Deposit money into an account
deposit :: Account -> Int -> STM ()
deposit account amount = do
    modifyTVar (balance account) (\existing -> existing + amount)

-- Withdraw money from an account
withdraw :: Account -> Int -> STM ()
withdraw account amount = do
    modifyTVar (balance account) (\existing -> existing - amount)

-- Get the current balance of an account
getBalance :: Account -> STM Int
getBalance account = readTVar (balance account)

-- Transfer money between two accounts atomically
transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
    withdraw from amount
    deposit to amount
```

Let's see how this handles each of our problems.

### Data Races

Data races are solved by simply _never sharing mutable state_. It's really not such a big burden. Haskell provides this guarantee by default, the entire language is based on immutable data, so feel free to share your data between threads however you like.


### Deadlock/Livelock

STM works by optimistic concurrency, meaning that threads never block waiting for locks. Instead, they proceed with their operations, and if a conflict is detected at commit time, one of the transactions is rolled back and retried, avoiding deadlocks entirely.

It is still possible to end up in a deadlock if you erroneously split up your operations into multiple transactions that depend on each other, but this is a much easier problem to reason about and avoid.


Traditional deadlocks are impossible with STM as long as you compose your operations into atomic transactions.


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





## Conclusion

More on the topic:

https://danluu.com/concurrency-bugs/

It mentions many tools, but AFAIK none of them claim to detect, never mind _prevent_ 
entire classes of these bugs.
