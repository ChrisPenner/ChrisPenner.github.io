The so-called free contravariant functor is similar in structure to a cofree comonad but instead of tagging each
functor with an 'a'; it requires an 'a' at each slot in the functor.

i.e.

```haskell
data FreeContravariant f a = FreeContravariant (f (a -> FreeContravariant f a))

-- Accept bools indefinitely, short circuit computation when "False" is encountered
truthy :: Bool -> FreeContravariant (Either [Char]) Bool
truthy b = FreeContravariant (if b then Left "done" else Right truthy)
```

Can represent many computations:

- `FreeContra Maybe`: Computations which may end eventually
- `FreeContra Identity`: Infinite computation
- `FreeContra []`: Non-deterministic computation (computation with branching)
- `FreeContra Either`: computation which may terminate producing a value
- `FreeContra (b,)`: Computation which produces values (Moore/Mealy machine?)
- `FreeContra (State s)`: Computation which produces a computation!
- `(a, a)`: Pausable computation


You can contramap your computations!

- `evenish = cmap even truthy` will compute whether any input integers have been even!
- `cmap unpack truthy` Turns a computation over Strings into one over Text

You can use `divide` to run different computations over parts of the same input at the same time!

`divide :: (a -> (b, c)) -> f b -> f c -> f a`

`divide id evenish oddish`

`choose :: (a -> Either b c) -> f b -> f c -> f a`

Given a function from arbitrary input to input of two different computers,
choose one and run that computation.



Two key types;
Comonadic and Monadic:

```haskell
extractStep :: Comonad w => FreeContra w a -> a -> FreeContra m a
extractStep (FreeContra wa) a = extract wa a

joinStep :: Monad m => FreeContra m a -> a -> FreeContra m a
joinStep (FreeContra ma) a = ma >>= \f -> runFreeContra (f a)
```

Examples:

- Comonad Env (i.e. (b, a)): Computations producing values along the way
- Comonad (Store s): (Covariant Comonad) Choose between multiple possible next computation steps
    - also, can be transformed easily: `Store a (FreeContra a) -> FreeContra (Store a) a`
- Comonad (Trace m): Computations with a 'default' behaviour that can be altered. Or could potentially 'collect'
    program input/alterations?
- Comonad Zipper: Choose your next step!
- Monad [a]: Nondeterministic computations
- Monad Maybe a: Haltable computations
- Monad Either b a: Haltable computations (with result)


Compositions:
- Compose [] Either : many computations producing many results
- Compose Either [] : Single computation which may end in many results
- Compose [] (Env b) : Multiple computations each producing results
- Compose (Env b) [] : ??


Can potentially pair `FreeContra f` with `CoFree f` by pairing tags with
functions and continuting on! Maybe the Applicative instance of `f` determines
how structures are combined?

Can use Monad Transformers and Comonad Transformers to combine `FreeContra []` with `FreeContra Maybe` for example;
e.g. lift to `MaybeT []` or something and compute that, joining after each step? or in the case of comonad
transformers, extracting after each step?
