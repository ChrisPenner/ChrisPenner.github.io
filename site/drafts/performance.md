---
title: "High Performance Haskell is Hard."
author: Chris Penner
date: Dec 12, 2024
tags: [programming, haskell]
description: "Some things I learned writing high performance Haskell these past few months"
image: flux-monoid/flux.jpg
---


Make it work, make it right, make it fast; so the saying goes. 
Over at Unison we've been in the "make it fast" chapter of our journey for the past few months,
Dan Doel (my co-worker) and I have managed to boost the speed of Unison's core interpreter loop by ~24 times!

Others tasked with speeding up code written in Haskell may well be aware that it's certainly a bit of an arcane art.
Throughout the process I've really benefited from the writing of others, so it's time to give back.

Here's my collection of some of the most impactful (and some of the most strange) performance improvements we've found... if only so I'll remember them the next time I need to do this sort of thing.

While I certainly won't claim that all of these approaches are applicable to the reader's situation, there's sure to be some interesting tidbits for all. Let's get to it!

## Prequel to the Prequel

Before we even start, we should make sure we understand our goal and our problem.

Presumably if you want to speed something up it's because you've noticed some issue with your app.
Your first job is to distill this issue down into a repeatable automated benchmark. This allows us to 
measure our progress, and by implementing many different benchmarks can also help protect us against regressions.

## Profile, profile, profile.

Okay, we've got some benchmarks, now we need to find out definitively _exactly where_ we're spending our time
in our slow workflow.

Spoiler alert, it's very often something you didn't expect.
I can't count the number of times I thought one area of my code would be a problem only to learn that my code was doing
something exponentially dumber (literally) in a completely different place.

We can avoid exponential idiocy by consulting the latest version of the excellent [GHC Manual](https://downloads.haskell.org/ghc/9.6.5/docs/users_guide/profiling.html) to
see how to compile with profiling enabled and how to inspect cost centers and such.

This isn't a profiling guide, so I'll just highlight a few additional things which I've personally found useful.

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

With these tools we've got objective data on where we're spending our execution time, which tells us places we may
be able to get the biggest possible performance gains.

## Computer, zoom in and enhance!

I feel safe saying that for the _majority_ of cases profiling will be enough to identify your worst bottlenecks, however
in the case of Unison's interpreter the default profiling setup was unfortunately not all that
useful. 

In the interpreter we have a plethora of `INLINE` pragmas which result in a profile output
that just says we spend most of our time within our interpreter's `eval` function... Thanks, but I'd
guessed that!

We could remove the inlining to get a better profile, but now we're not actually profiling the
real optimized code that we'll be shipping, which is a cardinal sin. 

So what can we do if we want a more granular profiling readout but without
making sweeping changes to the code? Let's learn about the `SCC` pragma!

`SCC` means "Set Cost Center", cost centers are how the profiler keeps track of execution time.
GHC will automatically create cost centers for function calls which aren't 
inlined, but in our case most functions _ARE_ being inlined. So what can we do about that? 

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
spent in that `do` block. How nifty!

I used this a lot to determine which individual branches of our giant `eval` and `exec` case statements
we were spending the most time in, or to see which were performing a lot of allocations.

Now we have fine-grained observability into where our time is being spent and can start trying to speed things up.

## The simplest thing that could possibly work

First, ensure you've enabled optimization on your builds by passing `-O2` to GHC.

This is the simplest thing you can do, but is also the most important, (almost) everything that follows depends on running an optimized build.

Moving on!

## Mind your algorithmic complexity

To be brief, all the performance tricks in the world won't help as much as
reducing the time complexity of your algorithm. Your first step should be to think
about what your hotspots are doing at a high level and make sure your core algorithm makes sense.

This is responsible for some of my biggest wins on the Unison CLI, I've gotten as much as a 20X speedup by replacing an exponential-time
algorithm with a linear-time one.

If your profile is showing a lot of time spent within some domain-specific algorithm, spend some time thinking about how you might be able to speed it up, use caching for it, or even skip that part entirely.

## Module boundaries and inlining

In Haskell, code is organized into modules. Typically we think about modules being organized for the
programmer's benefit, grouping code by purpose and making the codebase easier to navigate, but it
has an effect on compilation and optimization too.

Notably, module boundaries can affect how GHC inlines your functions.

Inlining is when the code of a function is copied from its definition directly into the call-site.
In many languages this is beneficial for small functions because it avoids the cost of a function
call, but it can have much larger ripple effects with a smart optimizing compiler like GHC

GHC uses many heuristics to decide when to inline a function call, but in order to inline a function GHC needs to
have the source code available. 

GHC doesn't
always expose the source code of functions across module boundaries since doing so for every function would be slow and add bloat. 
All this to say, **a function which is imported from some other module may not be inlined even when it makes sense to!**

If there's a particular function which you think could benefit from being inlined elsewhere in your program, either because you want it to be _specialized_ at all its call-sites, or because it's only used in one or two places, then you can choose
to tell GHC to definitely expose the source code to other modules using the `INLINEABLE` pragma.

Just slap an `{-# INLINEABLE myFunc #-}` next to your function and GHC 
will ensure its source code will be available for call sites to inline it.
Note that unlike `INLINE` this doesn't force GHC to inline the function, it just makes it possible
if the existing inlining heuristics indicate that it would be a good idea.

As an anecdote, at one point I discovered a few utility functions had been moved into their own
module without an inline pragma on them, adding an inline pragma to them resulted in a 1.5X speedup
to a hotspot _that didn't even use those utils_, simply because GHC was able to trigger additional
optimizations in other places which cascaded throughout the app.

## Specializing typeclass methods

Ad-hoc polymorphism is a powerful tool, but polymorphism can sometimes be at odds with performance.

Functions like typeclass method implementations are difficult for GHC to optimize effectively since they almost always
rely on arguments containing type parameters, limiting the amount of specialization it can perform looking without more context.

GHC can often do better when it has the context of the call-site of a typeclass method. 
If the typeclass method is called with a more concrete type at a given call-site then GHC can insert a specific typeclass implementation and specialize it to the types at that specific call site. This avoids any sort of dynamic dispatch can allow other optimizations to trigger at this call site.

Unfortunately, typeclass instances are very often defined in a separate module from their call-sites.

Adding the `INLINEABLE` or `INLINE` pragmas I mentioned earlier to typeclass method
implementations can sometimes help, it will allow GHC to inline and specialize many more of the call sites even across module boundaries.

Doing this correctly allows you to use the convenience of typeclasses without incurring
additional runtime cost, if a given implementation is inlined it should behave just as though you wrote a specialized version of the
typeclass methods for each usage site.

## When to NOINLINE

I've talked a lot about inlining so far, but there's also its counterpart: `NOINLINE`.

Most Haskellers have probably only seen this pragma used in situations pertaining to `unsafePerformIO`; e.g. 

```haskell
myGlobalVar :: TVar Int
myGlobalVar = unsafePerformIO $ newTVarIO 0
{-# NOINLINE myGlobalVar #-}
```

This is a necessity to ensure correct semantics in spite of the weirdness of `unsafePerformIO`, but
there's another case where `NOINLINE` can be helpful for performance rather than semantics.

In the Unison interpreter we use runtime tags to indicate the type of certain values we're holding on the
stack. For unrelated reasons, these tags are part of a much larger `Closure` data-type and we can't use a
simple enum type for each tag instance.

If we defined the tags as naive constants like so:

```haskell
intTag = Closure IntTag
```

Then GHC is likely to choose to inline the definition wherever it's used. In most cases this would be fine,
maybe even preferable! However in the interpreter we prefer to keep this tag as a single top-level expression so that we can
allocate the memory for it exactly once and simply _refer_ to it via a pointer at each usage site. 
If it were inlined it may be allocated once per call site, or even
churn allocations within a tight loop, which we certainly want to avoid.

In most cases such small allocation isn't something you should be worried about, but inside the core loop of an interpreter we want to avoid 
as many allocations as possible.

Adding a simple `NOINLINE` by the definition will prevent that from happening and give us the assurance that it
will only be allocated exactly once.

```haskell
intTag = Closure IntTag
{-# NOINLINE intTag #-}
```


### Avoiding code-size bloat

There's another case where NOINLINE can be useful, this one has to do with code-size.

Generally when writing Haskell we operate at a high level and don't think much about the low-level machines that actually execute our
code. In high-performance Haskell that's a mistake, at the end of the day our Haskell code runs on processors just like everything else, and we need to be aware of that.

Real-world processors have to load the code they're about to execute, and they have a number of code
caches which they use to speed this process up as much as possible. These code caches are very limited in size,
so if we can keep the size of the code we're executing relatively small it's more
likely that the processor will be able to keep commonly accessed bits of code in its cache. 
This, in turn, can sometimes provide significant speed ups, especially in tight loops like our interpreter.

What does this look like in practice? Let me explain with an example.

I was recently rewriting part of our code for executing builtins in the Unison interpreter.
As part of that work I replaced a dynamic dispatch mechanism with a giant case statement,
this was largely a performance win, but now that GHC could see that the call was a single static case-statement it decided
to inline the entire thing, which makes sense since it was only being called from one place, why pay the cost of a function call?

In this case however, it was inlined directly into the core loop of our interpreter, and the case statement was _huge_ (several hundred cases).
Beforehand we may have had enough processor cache to keep at least a bit of our core interpreter loop instructions hot, but now we had hundreds of lines of code
smack-dab in the middle of our core logic that we would only use once in a blue moon, fragmenting our cache between useful and useless code.

We separated this big case statement off into its own function and added a `NOINLINE` tag on it, keeping it out of the main interpreter loop and despite adding a function call it got us a small but noticeable speedup.

## Finding allocations

There was one point where the interpreter was running a bit slower than expected and I wasn't sure why, it seemed like a regression.

On a hunch, I decided to load up a hot-loop  which just counts from 1 to 1,000,000 then I ran
it with the useful and whimsical `+RTS -B` flag, which rings your terminal's bell whenever
the runtime performs a garbage collection, and wouldn't you know it, it started dinging like crazy.
Though perhaps a bit silly, it can definitely be a useful tool.

In my case, it was helpful because the UCM executable loads some code on startup and so I'd expect a couple GCs, 
but I wouldn't expect any GCs to be happening once the loop starts. Hearing a simple audio cue makes it easy to tell _when_ the GCs are happening in 
your program's life-cycle.

GHC is _extremely_ fast when it comes to allocating and collecting short-lived memory,
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

This helped me to confidently say that there was way too much allocation happening, which enabled me to dig into the core and find the offending allocations.

## The wonders (and pains) of worker-wrapper optimizations

There's one GHC optimization in particular that I hadn't heard of prior to this project, the [worker-wrapper transformation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag-fworker-wrapper).

The goal of this optimization is to swap out the usage of boxed Haskell types with unboxed versions where possible.
Unboxed types can avoid ceremony, allocations, and pointer-chasing, so they're typically a big boost to performance when you can use them,
however using unboxed types manually is a massive pain. It's not something you'd typically want to instrument by hand.

Here's where the worker-wrapper transformation comes in. 
GHC can use strictness analysis to notice when a certain function of your program 
forces the evaluation of its arguments, in this case it can then unpack those arguments into unboxed 
types within that scope without otherwise affecting the behaviour of the program, and will happily do so.
This can be particularly useful in self-recursive or looping bits of your program.

In our case, the `eval` function of the interpreter is strict in its use of our custom stack data-type and it's just a self-recursive loop, so the worker-wrapper optimization can apply.

GHC will split this eval function up into `eval` and `weval` during compilation, where
`eval` is simply a wrapper which accepts the boxed arguments, unwraps them, then calls down into the worker `weval` which uses the unboxed versions of these arguments. GHC will also rewrite all the self-recursive calls to `weval` so as to avoid packing the arguments back up until we eventually return up to the wrapper.

Getting worker-wrapper to trigger reliably is an art of its own, and can require the use of strictness annotations and a lot of examining GHC core to see what's going on. I have neither the time nor space to go deeply into reading core here, but if you've got a tight loop in your program it's worth digging into the core code to ensure GHC is performing this optimization, search for calls to the `w<func>` within the core for your module, if you can find them you know you're getting worker-wrapper, if not you may have some tweaking to do.

Some other quick tips here:

* If your function has many arguments, or the arguments are records with several fields, check your [`-fmax-worker-args`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag-fmax-worker-args-n) setting, and consider increasing it. If unpacking the data in your arguments will cause a worker function to have more than this many arguments, GHC will not perform the worker-wrapper transformation.
* In certain very rare cases you may wish to manually write your own unboxed worker function. We had to do this as part of splitting off the large case-statement I mentioned eariler. Perhaps because of the `NOINLINE` pragma GHC had decided not to unbox the stack argument here, and thus the whole `eval` function was using a boxed stack. I was able to fix this by explicitly accepting unboxed arguments, then for ergonomics I immediately re-boxed them within the method. GHC of course can see that it's silly to rebox them and will rewrite the function to be completely unboxed without causing me undue suffering.
* Some records are best left boxed, GHC is usually pretty good at this, in our case we had one large record which was rarely touched. After some experimentation we discovered that unboxing it actually slowed things down because it required passing many more arguments, and since it was so rarely used there wasn't a significant performance gain from unboxing it.

## Learn to read a bit of Core

Core is one of the intermediate languages which GHC targets as part of compilation.
You can see much of the optimization work that GHC has performed by looking at the Core
after the optimization passes.

It can help you know where you're allocating, whether worker-wrapper has been triggered, what is getting inlined, etc.

For me, I've found that reading Core is largely a lot of skimming and learning to recognize certain patterns, 
so I'd encourage you to start by reading through the core for some small chunks of code which you already know well
for practice.

If you'd like to give it a go, you can read up on the myriad of core flags [here](https://downloads.haskell.org/ghc/9.10-latest/docs/users_guide/debugging.html#core-representation-and-simplification), but here's a starter-pack which you can add to your `package.yaml` or pass to ghc directly.

```
ghc-options: -ddump-simpl -ddump-to-file -dsuppress-coercions -dsuppress-idinfo -dsuppress-module-prefixes -ddump-str-signatures -ddump-simpl-stats
```

If you're using `stack` you can find the core files inside `.stack-work/dist/<arch>/<ghc-version>/build/` in your package directory after running a build.

##  Trusting GHC

If there's one final tip I've learned, it's that in the absence of knowing 
exactly what you're doing, it's often best to just trust GHC and its defaults.

There were many times where I attempted to optimize something by rearranging code or adding a pragma only 
to realize that either GHC was _already_ performing that optimization for me, or that my change actually made things worse.
This, incidentally is also why having benchmarks is so important!

##  Conclusion

That was a lot, I know it won't all be useful to you right now, but hopefully can serve as a list of areas to look into the next time you've exhausted your own arsenal of profiling and optimization techniques.

Optimizing code can be significantly more complex in Haskell than other languages, 
but just remember that GHC has got your back and you can achieve truly impressive speed in such a high-level language.

Happy Haskelling!
