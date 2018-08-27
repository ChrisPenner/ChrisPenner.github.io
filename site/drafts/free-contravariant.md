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


You can contramap your computations!

- `evenish = cmap even truthy` will compute whether any input integers have been even!
- `cmap unpack truthy` Turns a computation over Strings into one over Text

You can use `divide` to run different computations over parts of the same input at the same time!

`divide :: (a -> (b, c)) -> f b -> f c -> f a`

`divide id evenish oddish`

`choose :: (a -> Either b c) -> f b -> f c -> f a`

Given a function from arbitrary input to input of two different computers,
choose one and run that computation.


