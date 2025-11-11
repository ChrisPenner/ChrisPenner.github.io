

---

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


