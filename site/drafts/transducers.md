# Transducers

I hope it's not controversial to say that I find Haskell to be a very _interesting_ language. It does things differently than many other languages out there. Haskell imposes a lot of **structure** on its programs: they must be well-typed and must be explicit about side-effects. Although the compiler doesn't enforce it, an idiomatic Haskell program will also express its ideas in terms of high level abstractions when possible (sometimes to a fault 🙃).

For these reasons I tend to have a lot of fun trying to "port" abstractions from other programming paradigms into Haskell to see how they turn out, and to determine whether the design of those abstractions is a perhaps a specialization of some more general mathematic abstraction. It can often lead to some fascinating insights, and can on occasion discover new useful tricks to be employed in the source language.

One such example is how the ["Build Systems á la carte" paper](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/build-systems-a-la-carte-theory-and-practice/097CE52C750E69BD16B78C318754C7A4) (Andrey Mokhov, Neil Mitchell, Simon Peyton Jones) explores how typeclasses of differing amounts of power results in different build system capabilities. This culminates in a matrix of possible build systems which discovers several combinations that have not yet been explored in practice!

Or perhaps how Phil Freeman's research into [Comonadic UIs](https://speakerdeck.com/paf31/the-future-is-comonadic?slide=28) determined that most modern UI frameworks could be modelled by an Adjunctive Monad/Comonad pairs, a unifying abstraction for most modern libraries. Who knew?

In this post I'm going to port the Clojure concept of **Transducers** into Haskell. We'll split the work into two parts:

* Port Transducers into Haskell as **faithfully** as possible to the Clojure implementation
* Experiment with "Haskellizing" our implementation and see how Transducers might have been designed differently in Haskell


**Caveats:** By no means am I a practiced Clojure developer, nor have I ever used transducers _in anger_, but I have studied them in depth and believe I understand the point in the design space they aim to fill.

Before we endeavour to provide a faithful implementation, let's get a bit of background on transducers.

## Transducers: What are they good for?

If you're not familiar with transducers, Rich Hickey's [introduction talk](https://www.youtube.com/watch?v=6mTbuzafcII) is a great place to start and I'd recommend watching that first. For a quicker summary, check out the [transducers homepage](https://clojure.org/reference/transducers).

In summary, transducers are composable transformations which are _process agnostic_; i.e. they don't care where their inputs are coming from, or where their outputs will be consumed. Here's an example of a transducer from the homepage:

```clojure
(comp
  (filter odd?)
  (map inc)
  (take 5))
```

The idea of a "pipeline" is ubiquitous these days so hopefully this doesn't throw you for a loop. This builds a transducer which accepts integers, filters to select only _odd_ values, increments them, then stops after yielding 5.

A similar pipeline in Haskell might look something like this:

```haskell
filter odd >>> map (+1) >>> take 5
```

Which we can run on a list to get the expected results:

```haskell
> filter odd >>> map (+1) >>> take 5 $ [1..10]
[2,4,6,8,10]
```

This Haskell pipeline is different from the transducer in many ways:

* The input and output types are fixed to a List
* Each step in the pipeline accepts the _entire_ list of input at once, it doesn't stream values one at a time. GHC will _likely_ optimize this using stream fusion, but it's not 100% reliable.
* This pure pipeline has no ability to perform side effects.

Although Haskell regularly uses linked lists in combination with laziness to provide efficient streaming, in order to be on par with transducers we'll need to support effectful and stateful transformations as well as a few other features.

Transducers implement all of the following aspects which we'll do our best to replicate:

* Stateful transformations: Transducers for deduplication, partitioning, etc. all require persistent state between pipeline items.
* Completion handlers for flushing final values and clean up
* Cancelling the entire transduction pipeline (e.g. on an unrecoverable error)
* Initialization: Transducers can optionally provide a "default" value on a run without input.

## Writing our first Transducer

Let's look at some clojure code! Again, watching the [introduction talk](https://www.youtube.com/watch?v=6mTbuzafcII) is the best way to understand this, but I'll do my best to provide a basic understanding. 

Here's what one of the simplest transducers looks like; the `map` transducer:


```clojure
(defn map
  ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
          (rf result (f input)))))))
```

A transducer is actually a function which accepts the rest of the pipeline as an argument. In code Rich calls this `rf` for **reducing function**. This is analogous to capturing the _continuation_ of your program, but it's scoped to just the rest of the pipeline. This allows each transducer to decide on its own terms when to call the next stage in a pipeline with its input, it may decide not to call the rest of the pipeline at all, or it may call the rest of the pipeline multiple times!

After passing in the **reducing function** continuation, the meat of any transducer is always a closure with **three arities**. Arities allow one of several function implementations to be chosen based on the number of arguments provided when the function is applied. The arities break down like this:

* Zero Arguments: Initialization Function

```clojure
([] (rf))
```

Rich mentions that the vast majority of transducers won't have a reasonable value to provide for the "init" function and should defer to the continuation in almost all cases. In fact I'd go as far as to say that transucers should never provide their own init value, since they're meant to be agnostic with regard to the reducing function that's taking place, but the Init arity provides a mechanism for the initial reduction value to be injected by the process running the transducer when when it first boots up.

In line with the above considerations we can see that the `map` transducer simply delegates its **init** arity onto `rf`.

* One Argument: Completion Function

```clojure
([result] (rf result))
```

The **completion** arity is called by the process running the transducer when it decides to shut down the transduction, perhaps because the input is exhausted or maybe an external actor has cancelled the process. This is where transducers have a chance to perform any needed clean up or flush intermediate results. A `result` is provided as an argument which should be passed along as it may be required by any transducers who have extra output to flush.

Since `map` doesn't have any state to clean up or any intermediate results to flush, it calls directly through to the continuation's completion handler.

* Two Arguments: Step Function

```clojure
([result input]
    (rf result (f input)))
```

This arity is where the work happens! The function receives the current `result`, i.e. the current accumulator in whichever fold the transducing process is running. If the transducing process is summing up numbers it would be the current total; if we were accumulating results into a list this would be the list of outputs we've encountered so far. The type of value here is determined by the final step in the transduction and we will have no knowledge of it here, so the only correct choice is to blindly pass it thread it through our computation.

The `input` on the other hand is indeed germane to our `map` transducer. `map`'s job is to run a function on all inputs and provide the transformed values as inputs to the rest of the pipeline. We can see that we apply `f` to `input` and then provide the outcome to the reducing function as its next input.

---

Okay! So we've broken down the different arities used in Clojure and seen an admittedly trivial impementation of a transducer which uses them. Now we can start porting this transducer into Haskell! We'll be using an iterative approach to write our Haskell implementation, we'll add features one-by-one as we find that we need them.

Let's start with some data modelling. The first issue at hand: Haskell doesn't have **function arities**! Don't fret though; it turns out that function arities are subsumed by **Sum Types**. By accepting a Sum Type as an argument we can use pattern-matching to choose the correct function implementation to run, and we have a type-safe guarantee that the correct arguments are provided.

Here's a Sum Type which represents each of the three arities we need:

```haskell
data Input r i =
      Init
    | Completion r
    | Step r i
  deriving Show
```

Our `Input` type has a constructor for each way we might want to run our reducing function, and it allows parameterizing the result and input types since those will change depending on where we use it. Now we can define the type of our reducing function:

```haskell
-- Reducing Function
type RF r i = Input r i -> r
```

Our reducing function is very simple, it takes an input and returns a result!


Next we'll need to define the type of a **transducer** itself. Remember, a transducer accepts a reducing function which represents the rest of the pipeline and returns a new reducing function. It shakes out somethingn like this:

```haskell
type Transducer r i o = RF r o -> RF r i

-- or, fully expanded:
type Transducer' r i o = (Input r o -> r) -> (Input r i -> r)
```

I'm using `i`, `o`, and `r` for the input, output, and result of the transducer respectively. Can you see why the reducing function we accept is of type `RF r o`? It's because that's what we'll be calling AFTER we've transformed our input, it may seem backwards, but we'll see in a second why it's not!

Great! We've got our types modelled, let's see if we can implement the `map` transducer. I'll paste the Clojure and Haskell versions here side by side:

Clojure:

```clojure
(defn map
  ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
          (rf result (f input)))))))
```

Haskell:
```haskell
mapT :: (i -> o) -> Transducer r i o
mapT _f rf Init = rf Init
mapT _f rf (Completion r) = rf (Completion r)
mapT f rf (Step r i) = rf (Step r (f i))
```

Wait a minute! What about the nested closures? Haskell defaults to **currying**, which means that we create the equivalent of a closure **every time we accept a function argument**! It's functionally equivalent to spelling out the closures with explicit lambdas but thankfully allows us to avoid the extra syntax here.

See how nicely our `Input` Sum Type compares to function arities when we pattern match on it? It adds a slight syntactic burden since we need to wrap our arguments when we construct our call to the next `rf`, but I think it's actually really nice to be able to concretely specify which arity we want to call through to. This makes for nicely self-documenting code. Using a Sum Type provides a tag on the input which allows for providing multiple implementations which take the **same number of arguments**, we won't see any uses of that here though since this feature isn't provided by function arities in Clojure.

That's it! We've built our first transducer in Haskell! However it's not much good if we can't run it 🤔... Let's write our first consumer.
o

### Running a Transducer

Let's implement Clojure's `transduce` function. `transduce` accepts and runs a transducer using a provided stepping function. When we translate the type into Haskell it looks something like this:

```haskell
transduce :: Foldable f => Transducer r i o -> RF r o -> Maybe r -> f i -> r
```

Reading left to right, we have the name of the function `transduce`, which is followed by a `Foldable f` constraint: we'll come back to that soon.

The first argument is the `Transducer` we want to run. 

Next, `RF r o` is the reducing function we want to run on the output of the pipeline. This reducing function is how we tell `transduce` the way we want to accumulate all of our results. We might use it to build a data structure, emit on a channel, log the results, etc.

The `Maybe r` is an optional starting value for the accumulation. If `Nothing` is provided we can run the transducer (including the provided `RF`) to generate an initial value instead.

Lastly is `f i`, which is the Haskell way to represent an arbitrary collection of inputs. The `Foldable` constraint we saw earlier provides an interface for us to consume elements of `f i` one by one. We could pass a list, key-value map, or a Set of inputs here and transduce will pull inputs from the collection for the pipeline.

Here's the implementation:

```haskell
import Data.Foldable ( Foldable(foldl') )
import Data.Maybe ( fromMaybe )

transduce :: forall r f i o. Foldable f => Transducer r i o -> RF r o -> Maybe r -> f i -> r
transduce xf rf maybeInitial inputs =
    let result = foldl' step initialResult inputs
     in xform (Completion result)
  where
    xform :: RF r i
    xform = xf rf
    initialResult :: r
    initialResult = (fromMaybe (xform Init) maybeInitial)
    step :: r -> i -> r
    step r i = xform (Step r i)
```

Let's start by reading the bindings we define in the `where` clause at the bottom:

```haskell
xform :: RF r i
xform = xf rf
```

This applies the 'final' reducing function to our transducer pipeline which results in a new reducing function which runs on one input at a time generating a new result.

```haskell
initialResult :: r
initialResult = fromMaybe (xform Init) maybeInitial
```

Here we pick our initial value. If an explicit initial value was provided we'll use that; otherwise we'll run our reducing function with the `Init` value to have it generate one for us.

```haskell
step :: r -> i -> r
step r i = xform (Step r i)
```

This simply restructures our reducing function into a normal ol' folding function, we'll need it to fold all of our inputs together.


Next we can look at the expression we use to compute result:

```haskell
foldl' step initialResult inputs
```

We're using a strict left-fold to apply our `step` function over our inputs one-by-one starting with the initial result. After all is said and done we'll have consumed all our input and have the final result! But wait, there's more! We have to run the transducer one last time using the `Completion` tag so that transducers can clean things up if they like. This final run will return the result which will be returned by transduce.

```haskell
    let result = foldl' step initialResult inputs
     in xform (Completion result)
```

Let's try this on-for-size. We only have a single transducer implemented so far, so it'll be a very simple transducer. Just like in Clojure we can sequence many transducers by simply composing each transducer together using regular function composition. If you've ever used lenses in Haskell this will look pretty familiar:

```haskell
myFirstTransducer :: Transducer r Int String
myFirstTransducer = mapT (*10) . mapT show
```

Functions compose from right to left, and indeed we're passing the reducing functions from right-to-left, but because of how they're used, we end up reading the transformation from left-to-right; just like we do with lenses (insert Checkhov's lens here).

This simple transducer accepts integers as input, multiplies them by ten, then outputs the `String` representation using show.

The last piece we'll need to run the transducer is a Reducing Function to process the outputs. This is one area where Clojure's arities work well syntactically; because arities are untyped, they can use any function which matches the correct shape as a reducing function. In Haskell we try to be more explicit about what we allow to be used where, so we'll probably want a utility function to take **normal** Haskell functions and turn them into reducing functions.

```haskell
toRF :: r -> (r -> o -> r) -> RF r o
toRF initialR _step Init = initialR
toRF _initialR _step (Completion r) = r
toRF _initialR step (Step r o) = step r o
```

Not much to comment on there, we can very easily convert a default value and step function into the format that `transduce` requires for us.

Let's write a few reducing functions:

```haskell
accumList :: RF [a] a
accumList = toRF [] (\r o -> r <> [o])

sumAll :: Num n => RF n n
sumAll = toRF 0 (+)
```

BTW, if you're screaming "It's a MONOID!" at your screen right now, just bite your tongue for a few more paragraphs 😉, if not, you're about to learn something pretty cool!

Okay, so we've written two very simple reducing functions, `accumList` is a reducing function which appends all the outputs into a list. `sumAll` adds all the outputs together using `0` as a starting point.

```haskell
>>> transduce myFirstTransducer accumList Nothing [1, 2, 3, 4, 5]
["10","20","30","40","50"]
```

Nifty, it actually works as expected!

Let's try out our `sumAll` reducing function and also try passing a different initial value:

```haskell
>>> transduce (mapT (*10) . mapT negate) sumAll Nothing [1, 2, 3, 4, 5]
-150

>>> transduce (mapT (*10) . mapT negate) sumAll (Just 1000) [1, 2, 3, 4, 5]
850
```

We're doing just swell so far but we've still got a lot of features to add. I'll be moving a bit faster now.

### One to none, one to many

Let's dive right into our next transducer. Let's write `filter`. This transducer should ignore any inputs which don't match a predicate.

```haskell
filterT :: (i -> Bool) -> Transducer r i i
filterT _keep rf Init = rf Init
filterT _keep rf (Completion r) = rf (Completion r)
filterT keep rf (Step r i) = 
    if keep i then rf (Step r i)
              else r
```

Here's the interesting part:

```haskell
    if keep i then rf (Step r i)
              else r
```

We only run the continuation if we want to process a given element, otherwise we perpetuate the previous result which will cause the pipeline to proceed as though nothing happened.

`filterT` removes elements from a pipeline, next let's write a transducer which **adds** elements to the pipeline.

```haskell
catT :: forall r f i. Foldable f => Transducer r (f i) i
catT rf Init = rf Init
catT rf (Completion r) = rf (Completion r)
catT rf (Step r inputs) = foldl' step r inputs
  where
    step :: r -> i -> r
    step result input = rf (Step result input)
```

`catT` accepts any foldable container as input and will output each element as input to the rest of the pipeline.

Let's experiment with our new toys:

```haskell
-- Filter for only even numbers then multiply by 10
>>> transduce (filterT even . mapT (*10)) accumList Nothing [1..10]
[20,40,60,80,100]

-- Break down the input lists then show the results
>>> transduce (catT . mapT show) accumList Nothing [[1, 2, 3], [4, 5, 6]]
["1","2","3","4","5","6"]

-- Filter out any long words, then flatten Strings into Chars
>>> words = ["cat", "hippopotomonstrosesquipedaliophobia", "dog"]
>>> transduce (filterT ((<10) . length) . catT) accumList Nothing  words
"catdog"
```

## Adding Effects

Our transducers are coming along, but so far they're completely pure! There are two cases in transducers where we need to run effects:

1. When the transformation itself requires an effect, e.g. printing a message to screen, making a web request, or reading the filesystem.
2. When our transformation needs to keep track of mutable state between runs.

Let's start with the first case by building a transducer which logs every value it encounters before passing it along.

In order to log things to the console we'll need access to the `IO` effect, and we'll need this effect **after** we've received the input. That means we'll need to edit the type of our reducing function:

```haskell
-- Here's the old version
type OldRF r i = Input r i -> r
-- We just need to add an IO wrapping the result
type RF r i = Input r i -> IO r
```

With this slight tweak we can now implement our logger:

```haskell
tapT :: Show i => Transducer r i i
tapT rf Init = rf Init
tapT rf (Completion r) = rf (Completion r)
tapT rf (Step r i) = print i >> rf (Step r i)
```

Since we've tweaked our `RF` type we also need to update the pure transducers:

```haskell
import Control.Monad

-- mapT can actually stay exactly the same since it always just calls through.
mapT :: (i -> o) -> Transducer r i o
mapT _f rf Init = rf Init
mapT _f rf (Completion r) = rf (Completion r)
mapT f rf (Step r i) = rf (Step r (f i))

-- I just needed to wrap the else clause with `pure`
filterT :: (i -> Bool) -> Transducer r i i
filterT _keep rf Init = rf Init
filterT _keep rf (Completion r) = rf (Completion r)
filterT keep rf (Step r i) = 
    if keep i then rf (Step r i)
              else pure r

-- Swap foldl' with foldM, 
-- Add IO to the type of the 'step'
catT :: forall r f i. Foldable f => Transducer r (f i) i
catT rf Init = rf Init
catT rf (Completion r) = rf (Completion r)
catT rf (Step r inputs) = foldM step r inputs
  where
    step :: r -> i -> IO r
    step result input = rf (Step result input)
```

We'll need to rewrite a few of our combinators too:

```haskell
-- Added IO to the step function 
-- Lifted the Init and Completion results using pure
toRF :: r -> (r -> o -> IO r) -> RF r o
toRF initialR _step Init = pure initialR
toRF _initialR _step (Completion r) = pure r
toRF _initialR step (Step r o) = step r o

accumList :: RF [a] a
accumList = toRF [] (\r o -> pure $ r <> [o])

sumAll :: Num n => RF n n
sumAll = toRF 0 (\a b -> pure $ a + b)

-- transduce took a bit more re-arranging, but it's nothing to complicated.
transduce :: forall r f i o. Foldable f => Transducer r i o -> RF r o -> Maybe r -> f i -> IO r
transduce xf rf maybeInitial inputs = do
    let xform = xf rf
        step r i = xform (Step r i)
    initialResult <- case maybeInitial of
                       Just initialR -> pure initialR
                       Nothing -> xform Init
    result <- foldM step initialResult inputs
    xform (Completion result)
```

Let's test out our new effectful transducer:

```haskell
>>> total <- transduce (mapT (*10) . tapT . mapT negate) sumAll Nothing [1..5]
10
20
30
40
50
>>> total
-150
```

We're printing intermediate results to the console as we run items through the `tapT` step!

This is handy, but we're still missing the ability to use any state within our transducers. Our `RF` function has access to `IO` in its output: `Step i -> IO r`, but since that IO block is going to be run once for every single input we can't initialize our stateful variables there. We'll need to follow Clojure's pattern and initialize them outside of the reducing function and include the resulting references to mutable state in a closure. Unlike Clojure however, we must also use the IO monad for the setup block, which means we need another small tweak to our types:


```haskell
-- Old version:
type OldTransducer r i o = RF r o -> (RF r i)
-- New version:
type Transducer r i o = RF r o -> IO (RF r i)
```

This change takes place in the type of the transducer itself, but otherwise looks to be the same idea! We're wrapping the result of our transformation function in IO so that we can use effects, its just that this time we're modifying our transformation OVER a reducing function, rather than modifying a reducing function itself. This provides us with a "setup" phase which is run when producing the pipeline. We can use this setup to initialize any state we might need within the reducing function. We'll usually use it to allocate stateful variables like IORefs.

Just like last time we'll need to edit all of our existing definitions to support this tweak. We'll only do this one more time I promise.

Introducing an effect in the middle of our function arguments means we'll need to re-arrange our pattern matching. It's a bit annoying, but we can use extensions like `LambdaCase` to reduce the pain of the boilerplate. If you're just reading along, don't bother reading through all the tweaks, you'll see how it works when we implement our first stateful transducer.

```haskell
mapT :: (i -> o) -> Transducer r i o
mapT f rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i -> rf (Step r (f i))

filterT :: (i -> Bool) -> Transducer r i i
filterT keep rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i ->
      if keep i then rf (Step r i)
                else pure r
catT :: forall r f i. Foldable f => Transducer r (f i) i
catT rf = pure $ \case 
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r inputs -> foldM step r inputs
  where
    step :: r -> i -> IO r
    step result input = rf (Step result input)

tapT :: Show i => Transducer r i i
tapT rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i -> print i >> rf (Step r i)

transduce :: forall r f i o. Foldable f => Transducer r i o -> RF r o -> Maybe r -> f i -> IO r
transduce xf rf maybeInitial inputs = do
    xform <- xf rf
    let step r i = xform (Step r i)
    initialResult <- case maybeInitial of
                       Just initialR -> pure initialR
                       Nothing -> xform Init
    result <- foldM step initialResult inputs
    xform (Completion result)
```

Overall pretty minor tweaks all around. Now we're ready to share state across transducer runs!

## Building a Stateful Transducer

There are many types of transducers which require persistent state from one run to the next:

* `take n`: Needs to keep track of how many items we've taken so far
* `uniq`: Needs to keep track of the input we've already seen
* `partition n`: Needs to keep a buffer of inputs until we have enough to group up and emit.

There are many more; see if you can think of some on your own.

We'll be implementing `partition` since it will also demonstrate our use of the `Completion`, which we haven't made use of yet.

I'll show you the whole implementation first, then we'll break it down. This is definitely the most complex transducer we'll be implementing:

```haskell
import Data.IORef ( atomicModifyIORef', newIORef, readIORef )

partitionT :: Int -> Transducer r i [i]
partitionT size rf = do
  -- initialize a mutable buffer to store items as we accumulate them
  buffer <- newIORef []
  -- return our next reducing function
  pure $ \case
    Init -> rf Init
    -- Our completion handler needs to flush any remaining values
    Completion r -> do
      leftovers <- readIORef buffer
      if not . null $ leftovers
        then rf (Step r leftovers)
        else pure r
    Step r i -> do
      emission <- atomicModifyIORef' buffer (updateBuffer i)
      case emission of
        Just is -> rf (Step r is)
        Nothing -> pure r
  where
    -- Look at the number of values in our buffer.
    -- If we've matched our output size we'll clear the buffer and emit them.
    updateBuffer :: a -> [a] -> ([a], Maybe [a])
    updateBuffer a xs =
        if length (a:xs) == size
           then ([], Just (a:xs))
           else ((a:xs), Nothing)
```

Okay! That's a lot at once. Here's the first interesting part:

```haskell
  buffer <- newIORef []
```

This initializes a mutable variable in the "setup phase" of our transducer, which is before we've accepted any input and occurs when we're just threading all of our transducers together. Crucially, this setup phase is run inside `transduce` immediately before using the resulting transformation function, ensuring that we get fresh state each time we run `transduce`. We'll use this buffer to accumulate values across 'runs' until we reach our threshold, then we'll emit them all at once as a list.

Looking at our step function next:

```haskell
    Step r i -> do
      emission <- atomicModifyIORef' buffer (updateBuffer i)
      case emission of
        Just is -> rf (Step r is)
        Nothing -> pure r
  where
    -- Look at the number of values in our buffer.
    -- If we've matched our output size we'll clear the buffer and emit them.
    updateBuffer :: a -> [a] -> ([a], Maybe [a])
    updateBuffer a xs =
        if length (xs <> [a]) == size
           then ([], Just (xs <> [a]))
           else ((xs <> [a]), Nothing)
```

We use `atomicModifyIORef'` to take a look at our buffer, and use our `updateBuffer` function to determine whether we're ready to emit or not. `updateBuffer` will clear the buffer and return a list of values to emit if we've matched the requested partition size, otherwise it will add the new value to the buffer and return `Nothing` to indicate we're not ready to emit yet. Then, inside our step function we can pattern match on the result and pass the list of values downstream or wait by returning the intermediate result.

The last thing to look at is in our `Completion` branch:

```haskell
    Completion r -> do
      leftovers <- readIORef buffer
      if not . null $ leftovers
        then rf (Step r leftovers)
        else pure r
```

Remember that the Completion handler is run exactly once when the transducing process has decided to shut everything down. For our partition transducer it would be nice if we could "flush" our buffer of accumulated state if we've got any left. The completion handler gives us that chance!

We check our buffer, and if we have any values chilling out in there we can flush them all downstream. We know that this transducer will never be called again after `Completion` is called, so there's no need to do any cleanup on our `buffer` IORef.

Let's try it out before we do one last iteration on our design.

Now that we've added yet another effect to our type signature we'll also have to adjust how we compose our transducers. Each Transducer is a function between reducing functions; but the reducing function it outputs is wrapped in `IO`. When we compose functions of the shape `a -> IO b` in Haskell we use `<=<` instead of `.`. Don't let the direction of the arrows confuse you, values still flow through the pipeline from left to right.

For those who are wondering; yes, you could also wrap each transducer in something like `Kleisli` or `Star` and use `Control.Cateogory ((.))`, but we won't cover that here.

```haskell
>>> transduce (filterT even <=< partitionT 3) accumList Nothing [1..20]
[[2,4,6],[8,10,12],[14,16,18],[20]]
```

This example takes the integers from 1 through 20, selects only the even numbers, then batches them up into lists of 3. We can see that our completion handler has successfully flushed the 'leftovers' because we have a list containing the single element `20` in our output.

## Early Termination

We've almost done it! We have one last feature left to achieve feature parity with Clojure's transducers. We need to implement the idea of "Early Termination".

The concept is quite simple; any link in our transducer chain, including the final sink, can decide at any point to abandon the computation and stop the whole process. This can occur in response to an unrecoverable error, or in more mundane circumstances such as a transducer like `take 3` where there's no need to continue working after it has received three elements.

The Clojure implementation accomplishes this by using a [Sentinel Value](https://en.wikipedia.org/wiki/Sentinel_value#:~:text=In%20computer%20programming%2C%20a%20sentinel,a%20loop%20or%20recursive%20algorithm.), specifically they use a value-wrapper called `Reduced` to wrap the result value. Transducers can use the predicate `reduced?` to check whether a result has been wrapped in `Reduced`, and should subsequently avoid calling that reducing function with any more input.

We can easily port this idea into Haskell by adding a new wrapper type that behaves the same way:


```haskell
data Result r = 
      Continue r
    | Stop r
```

By returning this new `Result` wrapper from our reducing functions we allow each transducer to handle a request to abort the computation. Here's what the new `RF` looks like:

```haskell
type OldRF r i = Input r i -> IO r

type    RF r i = Input r i -> IO (Result r)
```

As before, we'll need to update a few of our implementations to thread the `Result` through, but I promise this will be the last time!

At this point we're pros at this, so here's the everything we've built with the final tweaks needed to handle the `Result` types. Don't worry, I'll highlight a few interesting parts if you don't feel like reading everything through.

```haskell
import Data.IORef ( atomicModifyIORef', newIORef, readIORef )
import Data.Foldable ( Foldable(toList) )

data Input r i =
      Init
    | Completion r
    | Step r i
  deriving Show

data Result r =
      Continue r
    | Stop r
  deriving Show

type RF r i = Input r i -> IO (Result r)
type Transducer r i o = RF r o -> IO (RF r i)

getResult :: Result r -> r
getResult (Continue r) = r
getResult (Stop r) = r

mapT :: (i -> o) -> Transducer r i o
mapT f rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i -> rf (Step r (f i))

toRF :: r -> (r -> o -> IO r) -> RF r o
toRF initialR _step Init = pure (Continue initialR)
toRF _initialR _step (Completion r) = pure (Continue r)
toRF _initialR step (Step r o) = fmap Continue (step r o)

accumList :: RF [a] a
accumList = toRF [] (\r o -> pure $ r <> [o])

sumAll :: Num n => RF n n
sumAll = toRF 0 (\a b -> pure $ a + b)

foldWithResult :: RF r i -> [i] -> r -> IO (Result r)
foldWithResult _ [] r = pure $ Continue r
foldWithResult rf (i:rest) result  = do
    rf (Step result i) >>= \case
        Stop nextResult -> pure (Stop nextResult)
        Continue nextResult -> foldWithResult rf rest nextResult

transduce :: forall r f i o. Foldable f => Transducer r i o -> RF r o -> Maybe r -> f i -> IO r
transduce xf rf maybeInitial inputs = do
    xform <- xf rf
    initialResult <- case maybeInitial of
                       Just initialR -> pure initialR
                       Nothing -> getResult <$> xform Init
    foldWithResult xform (toList inputs) initialResult >>= \case
      Stop r -> pure r
      Continue r -> getResult <$> xform (Completion r)

filterT :: (i -> Bool) -> Transducer r i i
filterT keep rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i ->
      if keep i then rf (Step r i)
                else pure (Continue r)

catT :: forall r f i. Foldable f => Transducer r (f i) i
catT rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r inputs -> foldWithResult rf (toList inputs) r

tapT :: Show i => Transducer r i i
tapT rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i -> print i >> rf (Step r i)

partitionT :: Int -> Transducer r i [i]
partitionT size rf = do
  buffer <- newIORef []
  pure $ \case
    Init -> rf Init
    Completion r -> do
      leftovers <- readIORef buffer
      if not . null $ leftovers
        then rf (Step r leftovers)
        else pure (Continue r)
    Step r i -> do
      emission <- atomicModifyIORef' buffer (updateBuffer i)
      case emission of
        Just is -> rf (Step r is)
        Nothing -> pure (Continue r)
  where
    updateBuffer :: a -> [a] -> ([a], Maybe [a])
    updateBuffer a xs =
        if length (xs <> [a]) == size
           then ([], Just (xs <> [a]))
           else ((xs <> [a]), Nothing)
```

I've added a new folding combinator to help us out:

```haskell
foldWithResult :: RF r i -> [i] -> r -> IO (Result r)
foldWithResult _ [] r = pure $ Continue r
foldWithResult rf (i:rest) result  = do
    rf (Step result i) >>= \case
        Stop nextResult -> pure (Stop nextResult)
        Continue nextResult -> foldWithResult rf rest nextResult
```

This function folds a reducing function over a list of inputs, but will short circuit if it is ever told to `Stop`. We use this fold within `catT` and `transduce` since those are the only places where we need to decide whether to continue partway through an iteration.

Last of all we'll need to write a transducer which uses this feature so that we're able to test it out. Clojure has an example in the way of `halt-when`, which will halt the entire pipeline if a predicate is true on its input.

```haskell
haltWhenT :: (i -> Bool) -> Transducer r i i
haltWhenT predicate rf = pure $ \case
    Init -> rf Init
    Completion r -> rf (Completion r)
    Step r i ->
        if predicate i then pure $ Stop r
                       else rf (Step r i)
```

This bears a lot of similarity to `filterT`, it checks each input to see if the predicate passes, and if it does, it returns the `Stop` result instead of calling through. Let's try it:

```haskell
>>> let pipeline = filterT even <=< tapT <=< haltWhenT (>10)
>>> result <- transduce pipeline accumList Nothing [1..20]
2
4
6
8
10
12
>>> result
[2,4,6,8,10]
```

Here we can see using `tapT` that we process inputs up to and including `12`, but as soon as `haltWhen` receives `12`, which is greater than `10`, it halts execution and doesn't pass the `12` to the final step. We can also see from this that we've successfully implemented the **laziness**  that is expected from transducers, we didn't process any more elements than we needed, with the exception of the 12 which was needed to trigger our termination condition.

## In Summary


