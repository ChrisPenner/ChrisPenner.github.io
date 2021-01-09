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

## Faithful Implementation

### Writing our first Transducer

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

Here's the implmentation:

```haskell
import Data.Foldable ( Foldable(foldl') )
import Data.Maybe ( fromMaybe )

transduce :: forall r f i o. Foldable f => Transducer r i o -> RF r o -> Maybe r -> f i -> r
transduce xf rf maybeInitial inputs =
    foldl' step initalResult (toList inputs)
  where
    xform :: RF r i
    xform = xf rf
    initalResult :: r
    initalResult = (fromMaybe (xform Init) maybeInitial)
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
initalResult :: r
initalResult = fromMaybe (xform Init) maybeInitial
```

Here we pick our initial value. If an explicit initial value was provided we'll use that; otherwise we'll run our reducing function with the `Init` value to have it generate one for us.

```haskell
step :: r -> i -> r
step r i = xform (Step r i)
```

This simply restructures our reducing function into a normal ol' folding function, we'll need it to fold all of our inputs together.


Now we can look at the expression we use to compute the final result:

```haskell
foldl' step initalResult inputs
```

We're using a strict left-fold to apply our `step` function over our inputs one-by-one starting with the initial result. After all is said and done we'll have consumed all our input and have the final result!

Let's try this on-for-size. We only have a single transducer implemented so far, so it'll be a very simple transducer. Just like in Clojure we can sequence many transducers by simply composing each transducer together using regular function composition. If you've ever used lenses in Haskell this will look pretty familiar:

```haskell
myFirstTransducer :: Transducer r Int String
myFirstTransducer = mapT (*10) . mapT show
```

Functions compose from right to left, and indeed we're passing the reducing functions from right-to-left, but because of how they're used, we end up reading the transformation from left-to-right; just like we do with lenses (insert Checkhov's lens here).

This simple transducer accepts integers as input, multiplies them by ten, then outputs the `String` representation using show.

The last piece we'll need to run the transducer is a Reducing Function to process the outputs. This is one area where Clojure's arities work well syntactically; becaue arities are untyped, they can use any function which matches the correct shape as a reducing function. In Haskell we try to be more explicit about what we allow to be used where, so we'll probably want a utility function to take **normal** Haskell functions and turn them into reducing functions.

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

`catT` accepts any foldable container as input and will output each element as input to the rest of the pipeline. Think of it as breaking down its inputs.

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
