---
title: "High Performance Haskell is Hard."
author: Chris Penner
date: Dec 12, 2024
tags: [programming, haskell]
description: "Some things I learned writing high performance Haskell these past few months"
image: flux-monoid/flux.jpg
---

Gotta go fast!

Make it work, make it right, make it fast; so the saying goes. Whether you buy into that or not, the implementation of the Unison programming language's interpreter has officially entered its "make it fast" era.

Others who've been tasked with ensuring Haskell code is running at its fastest are well aware that it's a bit of an arcane art. 
My co-worker Dan and I have been crunching on Unison's performance for several months now and I figured it's time that I collect and chronicle some of the most impactful and strange performance improvements we've found, if only so I'll remember them the next time I need to do this sort of thing.

While I certainly won't claim that all these things are applicable to the reader's situation, there's sure to be some interesting tidbits for all. Let's get to it!

Before we even start, make sure you've enabled optimization on your builds using `-O2` or all bets are off!

## First things first, mind your algorithmic complexity

I won't spend much time here, but all the performance tricks in the world won't help as much as
reducing the time complexity of your algorithm. Your first step should be to think
about what your hotspots are doing at a high level and make sure your core algorithm makes sense.

But how do you know where your hotspots are? Profiling! 👇

## Profile, profile, profile.

Okay I lied earlier, your VERY first step should be to write some realistic benchmarks or 
workflows which exercise your system in a way that's similar to how the end user will use it.

After that, you can profile it to determine which parts of your code are _actually_ slow. Spoiler alert, it's not always where you think it may be! 
I can't count the number of times I thought one area of my code would be a problem only to learn that my code was doing
something exponentially dumber (literally) in a completely different place.

Consult the latest version of the excellent [GHC Manual](https://downloads.haskell.org/ghc/9.6.5/docs/users_guide/profiling.html) to
see how to compile with profiling enabled and how to inspect cost centers and such.

This isn't a profiling guide, but I'll highlight a few additional things which I've personally found useful.

If you use the `stack` build tool, profiling your program is usually as easy as compiling with 
`stack build --profile` then using `stack exec --profile my-exe +RTS -P -RTS`. This should output a
`my-exe.prof` which has some useful information at the top. Personally I find it much more helpful to
post-process the prof file with tools like [profiteur](https://hackage.haskell.org/package/profiteur) and [profiterole](https://hackage.haskell.org/package/profiterole). 
Each of these is just a `stack install` away.

These tools present the data contained in a `.prof` file in a more digestible way.
`profiteur` is my favourite; it presents your program as a tree-map showing the proportion of your
execution's time (or allocations, see the option in the top-left) keyed by method name.

The following image shows that 17% of execution time and 14.5% of our allocations were from within `compare` from the `Unison.Symbol` module.
Might be worth looking deeper into!

![](perf/profiteur.png)

Profiterole produces a hyperlinked view of the `.prof` file which summarizes a few highlights
that may be worth investigating. Each entry includes the total time (`TOT`) spent in that location
including calls made from within it,
the _inherited_ time (`INH`) which is the `TOT` time minus time spent in locations which have their own summary entry, and the `IND` time which is the time spent within exactly that location and not within a sub-call.

I'll often have both the profiterole and profiteur pages open at the same time.

![](perf/profiterole.png)

## Computer, zoom in and enhance!

I feel safe saying that for the _majority_ of cases profiling will be enough to identify your worst bottlenecks, however
in the case of Unison's interpreter the default profiling setup was unfortunately not all that
useful. We had a plethora of `INLINE` pragmas within the interpreter, resulting in a profile output
which just said we spent most of our time within our interpreter's `eval` function... thanks but I
could've guessed that!

Sure we could remove the inlining to get a better profile, but now we're not actually profiling the
real optimized code that we'll be shipping, which is a cardinal sin in profiling. 
So what can we do if we want a more granual profiling readout but without
making sweeping changes to the code? Enter the `SCC` pragma!

`SCC` means "Set Cost Center", cost centers are how the profiler keeps track of how to label
the execution time it tracks. GHC will automatically assign cost centers to function calls which aren't 
inlined, but in our case most things ARE being inlined so we need to get more fine-grained. SCC 
allows us to manually tell GHC to label costs within arbitrary blocks of code.

There are a few flags available in the [profiling manual](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#ghc-flag--fprof-auto) which tell GHC where to add more cost centers, but there's also the [SCC pragma](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#inserting-cost-centres-by-hand) which allows us to easily tell GHC we want a label in a specific block of code.

[Click through](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#inserting-cost-centres-by-hand) to learn more, but the gist of it is that we can do something like:

```haskell
myFunction = do
  foo 1 2 3
  {-# SCC "busywork" #-} do
    bar x y z
    baz 99
```

And now if we profile our program we'll have a `busywork` label which tells us how much time we
spent in that `do` block! Nifty!

I used this a lot to determine which branches of our giant `eval` and `exec` case statements
we were spending the most time in, or to see which were performing a lot of allocations.

## When profiling isn't enough

Profiling helped a little and can tell us _where_ things are slow, but it doesn't help us actually
speed it up!

Once you've found your problem areas, here are some things you can think about to hopefully find an
avenue towards more speed.

## Module boundaries and inlining

In Haskell, code is organized into modules. Typically we think about modules being organized for the
programmer's benefit, grouping code by purpose and making the codebase easier to navigate, but it
has an effect on compilation and optimization too!

Most notably, it affects how GHC inlines your functions.

Inlining is when the code of a function is copied from its definition directly into the call-site.
In many languages this is beneficial for small functions because it avoids the cost of a function
call, but it can have much larger ripple effects with a smart optimizing compiler like GHC, since
it brings more  code into the same shared scope for GHC to reason about.

GHC uses many heuristics to decide when to inline a function call, but in order to inline it needs to
have the source code of a function available when examining the call site. GHC doesn't
always expose the source code of functions across module boundaries since doing this for _everything_ would be slow and add bloat. 
All  in all, this means that a function imported from some other module may not be inlined even when it makes sense to!

If there's a particular function you think could benefit from being inlined elsewhere, either because you want it to be _specialized_ at all its call-sites, or because it's only used in one or two places, then you can choose
to tell GHC to definitely expose the source code to other modules using the `INLINEABLE` pragma!

Just slap an `{-# INLINEABLE myFunc #-}` next to your function and GHC 
will ensure its source code will be available for call sites to inline it.
Note that unlike `INLINE` this doesn't force GHC to inline the function, it just makes it possible
if the heuristics decide it's a good idea.

As an anecdote, at one point I discovered a few utility functions had been moved into their own
module without an inline pragma on them, adding an inline pragma to them resulted in a 1.5X speedup
to a hotspot _that didn't even use those utils_, simply because GHC was able to trigger additional
optimizations in other places.

## Specializing typeclass methods

For better or worse, Typeclasses are a huge part of Haskell.
Ad-hoc polymorphism is a powerful tool, but polymorphism can sometimes be at odds with performance.

Functions like typeclass methods are difficult for GHC to optimize effectively since they almost always
rely on arguments with parameterized types. This means that GHC doesn't necessarily know which 
implementation of given typeclass methods it may be calling by examining the definition of the function alone.

GHC can statically resolve some of these unknowns when it can _specialize_ a method. If a typeclass method is
called with concrete type at a given call-site, then GHC knows exactly which implementations
are being called and can resolve them statically, avoiding any sort of dynamic dispatch and possibly even inlining them for further gains!

Typically the biggest blocker here is, as mentioned above, that typeclass instances are often defined in a
separate module from their call-sites.
It's a bit of an annoyance, but adding an `INLINEABLE` or `INLINE` pragma to typeclass method
implementations can sometimes help as it will allow GHC to inline and specialize many more of the call sites.

Doing this correctly allows you to use the convenience of typeclasses without incurring
additional runtime cost, if a given implementation is inlined it should behave just as though you wrote a specialized version of the
typeclass methods for each usage site.

## When to NOINLINE

I've talked a lot about inlining so far, but there's also its counterpart: `NOINLINE`!

Most Haskellers have probably only seen this used in cases pertaining to `unsafePerformIO`; e.g. 

```haskell
myGlobalVar :: TVar Int
myGlobalVar = unsafePerformIO $ newTVarIO 0
{-# NOINLINE myGlobalVar #-}
```

This is a necessity to ensure correctness in spite of the weirdness of `unsafePerformIO`, but
there's another case where `NOINLINE` can be helpful that I want to talk about.

In the Unison interpreter we use runtime tags to indicate the type of certain values we're holding on the
stack. For unrelated reasons, these tags are part of a much larger `Closure` data-type and we can't use a
simple enum type for each tag instance.

If we defined the tags as naive constants like so:

```haskell
intTag = Closure IntTag
```

Then GHC is likely to choose to inline the definition wherever it's used. In most cases this would be fine,
maybe even preferrable! However in our case, we prefer to keep this tag as a single top-level expression so that we can
allocate the memory for it exactly once, and simply _refer_ to it at each usage site. 
If it were inlined it may be allocated once per call site, or even
within a tight loop, which we certainly want to avoid.

In most cases such small allocation is nothing to be concerned about, but inside the core loop of an interpreter we want to avoid 
as many allocations as possible.

Adding a simple `NOINLINE` by the definition will prevent that from happening and give us the assurance that it
will only be allocated exactly once.

```haskell
intTag = Closure IntTag
{-# NOINLINE intTag #-}
```


### Avoiding code-size bloat

There's another case where NOINLINE can be useful, it has to do with code-size!

In Haskell we operate at high level and typically don't think about the low-level machines that actually execute our
code. At the end of the day our Haskell code runs on processors just like everything else, and we need to be aware of that.

Real-world processors have to load the code they're about to execute, and they have a number of code
caches which they use to speed up this process as much as possible. These code caches are very limited in size.
If we manage to keep the actual size of the code we're executing relatively small, it's more
likely that the processor will be able to keep commonly accessed bits of code in its cache, which
can provide significant speed ups, especially in tight loops like our interpreter.

What does this look like in practice? Let me explain.

I  was recently rewriting part of our code for executing builtins in the Unison interpreter.
As part of that work I replaced a dynamic dispatch mechanism with a giant case statement,
this was a performance win, but now that GHC could see that the call was a single static case-statement it decided
to inline the entire thing, which makes sense since it was only being called from one place, why pay the cost of a function call?

However, this particular place happened to be in the core loop of our interpreter, and the case statement was _huge_ (several hundred cases).
Beforehand we may have had enough processor cache to keep at least a bit of our core loop hot, but now we had hundreds of lines of code
smack-dab in the middle of our core logic that we would only  use once in a blue moon.

By separating off and `NOINLINE`ing that big chunk it cut down on the code size of the main loop and we got a small but noticeable speedup on
the more common instructions.

## The wonders (and pains) of worker-wrapper optimizations

## Manual unboxing

## Finding allocations

There was one point where the interpreter was running a bit slower than expected and I wasn't sure why, it seemed like a regression.

On a hunch, I decided to load up a hot-loop  which just counts from 1 to 1,000,000 then I ran
it with the useful and whimsical `+RTS -B` flag, which rings your terminal's bell whenever
the runtime performs a garbage collection, and wouldn't you know it, it started dinging like crazy.
Though perhaps a bit silly, it can definitely be a useful tool.

In my case, it was helpful because the UCM executable loads some code on startup and so I'd expect a couple GCs, 
but I wouldn't expect any GCs to be happening once the loop starts. Hearing a simple audio cue makes it easy to tell _when_ the GCs are happening in 
your program's life-cycle.

GHC is actually _extremely_ fast when it comes to allocating and collecting short-lived memory,
but for tight loops in an interpreter we really don't have any excuse to be allocating at all.

Now that we know we're allocating more than we should, we can combine this info with the useful `+RTS -sstderr` flag which will print out runtime stats 
and you get a pretty good picture of your allocations and garbage collections.

Here's an example output from `-sstderr` from an arbitrary run so you get an idea.

```
   2,953,202,040 bytes allocated in the heap
     118,339,168 bytes copied during GC
      18,415,336 bytes maximum residency (7 sample(s))
         254,232 bytes maximum slop
              83 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       697 colls,     0 par    0.001s   0.128s     0.0002s    0.0058s
  Gen  1         7 colls,     6 par    0.126s   0.139s     0.0199s    0.0939s

  Parallel GC work balance: 92.45% (serial 0%, perfect 100%)

  TASKS: 25 (1 bound, 24 peak workers (24 total), using -N8)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.009s  (  0.025s elapsed)
  MUT     time    1.208s  (  1.536s elapsed)
  GC      time    0.127s  (  0.267s elapsed)
  EXIT    time    0.006s  (  0.010s elapsed)
  Total   time    1.350s  (  1.839s elapsed)

  Alloc rate    2,445,681,543 bytes per MUT second

  Productivity  89.4% of total user, 83.6% of total elapsed
```

We can see the total amount of memory we allocated, peak memory residency, time spent on GC, average pause times, all that good stuff!

##  Trusting GHC

If there's one final tip I've learned, it's that in the absence of knowing 
exactly what you're doing, it's often best to just trust GHC and its defaults.

There were many times where I attempted to optimize something by rearranging code or adding a pragma only 
to realize that either GHC was _already_ performing that optimization for me, or that my change actually made things worse.
This, incidentally is also why having benchmarks is so important!

##  Conclusion

2. Manual data inlining for instructions
2. Manual inlining with knot-tying
3. Unlifted/unboxed types wherever possible
6. Worker-wrapper and friends
8. Trust GHC
9. Manual worker-wrapper wrappers.
10. Reading Core
