

* Principle of least power
* Power is a trade-off, you sacrifice observability for power.

# Monads vs Categorical Effects

Monads depend heavily on the use of *closures* to build the next part of the program from the results of running effects.
This is bad, because closures are completely opaque, they can ONLY be applied.

Building with ONLY applicatives avoids the closure problem, but doesn't allow us to use results of effects when executing new effects.

If we instead treat input as part of the effect, we can analyze the structure of our programs while still allowing results to affect your programs.


* Monads allow you to build never-before-seen programs on the fly.
* Category-based effects allow y
