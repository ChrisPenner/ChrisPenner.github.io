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

## Problems

### Data races

_Most_ programming languages are imperative with mutable data structures [citation needed]. 
Mutable data brings with it data-races.

Mutexes are the solution espoused to fix this issue, but you need to remember to use them, and you need to use them _correctly_.

What will the following pseudo-code print?

```go
x = 42
println(x)
```

In any sane system you'd say it prints `42`, but in _most_ languages you couldn't say for certain! If the address of `x` has been shared then it could change at any time, and it's impossible to know that you need a lock before accessing it, nothing about its type will indicate it, and the compiler typically won't stop you.

Sure, if you're perfect, disciplined, or _always_ use the correct patterns you might be safe, skill can help, but quite frankly I'd just rather be focused on solving the problem at hand.

### Deadlock/Livelock

Deadlock is quite possibly the most frustrating problem to debug. 
At least when you get a segfault you know your program crashed. In a deadlock your program just... stops, or maybe it's a livelock and things appear to be happening, but never progressing.

It can be orders of magnitude more difficult to debug because by definition: concurrency is involved, and concurrency is non-deterministic. It's also just harder to use a debugger across multiple threads.

Once again, most languages don't attempt to help out here, though tides are turning here and Golang will at least detect when all goroutines are blocked.

Avoiding deadlocks can be extremely difficult, espescially when dealing with locks that are created dynamically at runtime.

### Lack of composability

In my opinion this is the single biggest problem.

Let's say we implement a simple bank account abstraction, here's some pseudo-code.

```go
struct Account {
  mutex Mutex,
  balance int,
}

// Deposit money into an account
func (a *Account) Deposit(amount int) {
  a.mutex.lock()
  a.balance += amount
  a.mutex.unlock()
}

// Withdraw money from an account
func (a *Account) Withdraw(amount int) {
  a.mutex.lock()
  balance -= amount
  a.mutex.unlock()
}

// Get the current balance of an account
func (a *Account) Balance() int {
  a.mutex.lock()
  b := a.balance
  a.mutex.unlock()
  return b
}
```

Great! We've used abstraction and have written small units of code that do their job.


Okay now let's transfer money between accounts:


```go
func transfer(from *Account, to *Account, amount int) {
  from.Withdraw(amount)
  to.Deposit(amount)
}
```

Oh, but wait, this transfer isn't atomic, anything could happen between the withdraw and deposit.

Okay no problem, we know how to solve this, we use mutexes!

```go
func transfer(from *Account, to *Account, amount int) {
  from.mutex.lock()
  to.mutex.lock()

  from.Withdraw(amount)
  to.Deposit(amount)

  from.mutex.unlock()
  to.mutex.unlock()
}
```

Great, now it's atomic right?

Wrong! It's a deadlock, we acquire the account lock in `transfer`, but also in `Withdraw` and `Deposit`, the second time we try to acquire the lock we'll block forever. 

Again, nothing in the language or type-system prevents us from writing this code, and though in this case the problem is small in scope, the second lock acquisition could just as easily be nested many many layers deep in some other complex operation.

Okay, so I guess we need to remove the locks from `Withdraw` and `Deposit`;

```go
func (a *Account) Deposit(amount int) {
  a.balance += amount
}
func (a *Account) Withdraw(amount int) {
  balance -= amount
}

func (a *Account) transfer(from *Account, to *Account, amount int) {
  from.mutex.lock()
  to.mutex.lock()

  from.Withdraw(amount)
  to.Deposit(amount)

  from.mutex.unlock()
  to.mutex.unlock()
}
```

Okay, this works, but now it's trivially easy to accidentally call `Withdraw` or `Deposit` without holding the lock.

But wait... while I was explaining the composability problem we've also had a hidden deadlock bug this whole time.

```go
func threadA() {
  transfer(account1, account2, 100)
}

func threadB() {
  transfer(account2, account1, 50)
}
```

If these two threads run at the same time, we can end up in the classic deadlock situation where they both acquire one lock and then block forever trying to acquire the other.

What a mess.

### Transactional operations

As we discovered in the above example, we not only want the ability to slice and dice code into composable units however we want, but it's also important that we can craft **larger atomic operations out of smaller ones**.

Golang has a ton of great first-class concurrency support, for example this great [thread-safe map](https://pkg.go.dev/sync#Map).

But what happens when you want to atomically move things from one map to the other? The interface doesn't allow for that.

Suddenly this concurrent map doesn't cut it, you need to work with your own locks again, and will need to either wrap the type (updating all your code to use the new one), or manually update everywhere that accesses it to use your new locks. 

I'm picking on `sync.Map`, but this is all too common. Even if a library auther were to think of this and provide methods for doing so, what happens when you want to atomically transfer between two _different_ concurrent data structures?

This leads to the final problem...

### Ecosystem fragmentation

When our concurrency models don't compose it's no surprise that there's no easy way to patch together safe operations from multiple libraries in the ecosystem. It's left to the application author to wrap all of these useful libraries with even more concurrency patterns.


---


## Software Transactional Memory

Software Transactional memory is the best solution to these problems and _it's not even close_.

For those who've not heard of it, STM provides the database-like ability to perform operations on your application's data and data structures in transactions which either commit as a consistent atomic unit or roll back all their changes as though nothing happened.

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
