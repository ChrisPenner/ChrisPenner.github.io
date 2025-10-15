
## Why not just use Arrows?

The Arrow hierarchy already exists, and we already have Arrow notation, so why not just use that?

In short, the Arrow hierarchy is too large-grained for my liking.
While it works great for defining programs that use the full power of the Haskell language,
sometimes we want to _intentially_ limit ourselves to fewer constraints so that can do things like
implementing a program in our DSL that can be compiled down to a different target language.

Within this post we required a Profunctor constraint simply to avoid a lot of boilerplate and tedium, since we were
compiling down to simple targets, however, there are plenty of valid and interesting 
categories which don't have a reasonable Profunctor instance.

For example, if we're compiling down to SQL queries, we can't allow users of the 
DSL to embed arbitrary Haskell functions into that structure, we have no way of interpreting them down into valid SQL.

Unfortunately, when using Arrow-notation, the system _requires_ that you implement the `arr` method of the class,
and uses is within arrow-notation desugaring, so if your Category can't reasonably implement it, then you're out of luck.

For example we may wish to implement a DSL category which can be compiled down to SQL queries, GPU shaders, even Javascript.
By using the fine-grained Category hierarchy we can implement each language's methods of routing data
using the appropriate Semi-(Co)Cartesian constraints, and avoid the use of Profunctor.

## Want to help?

