---
title: "Optics \& Regex: Greater than the sum of their parts"
author: Chris Penner
date: Sep 8, 2019
tags: [haskell]
description: Optics \& regex: greater than the sum of their parts.
---

Regardless which language you're coming from, regular expressions are a core tool in the programmer's toolbox. Though some have a distate for their difficult to maintain nature, they're an adaptable quick'n'dirty way to get things done. I tend to use small programming puzzle to learn new programming languages, and most of these involve some simple parsing of problem input from a file or stdin. In most languages I reach for regex as a natural way to do this, so naturally when I started solving problems with Haskell I google for Regular Expressions in Haskell.

I was faced with a deluge of options, and none really stood out as being "the standard".
A common response when beginners ask how to use regex in Haskell is to "learn parser combinators"; or they post a link to one of the myriad of disjointed front-end back-end combinations which must be stitched together in order to work. Even if you find a library which you can figure out how to install and properly import you're then faced with the magical "do everything" operator, which uses type inference to fundamentally change its behaviour:

```haskell
(=~) :: ( RegexMaker Regex CompOption ExecOption source2
        , RegexContext Regex source1 target
        ) => source1 -> source2 -> target
```

While this is handy for a Haskeller who knows what they're doing, I find it makes code harder to reason about, and makes it nigh impossible for a beginner to decipher and use.


Long story short, I DID eventually end up learning to use parser combinators (and they're awesome), but it still seems silly and even a bit rude to require that of all Haskell developers. Why is this problem so hard? I revisited it recently now that I'm a lot more comfortable with the language and realized that the situation is still pretty rough. After a quick look around I realized that the jury is still very much still deliberating on what the best interface to use for regex is. And as I was looking at it, I started to realize some similarities to something I've been obsessing about lately, and that drove me to try something new!

Note that I definitely don't mean to throw shade at any of these other libraries; implementing a (fast) regular expression library is really hard, and so is interface design!
I just think there's still room for improvement, and I found something that fit the bill so perfectly I couldn't stop myself from trying! Hopefully this helps set the stage for why I decided to add yet another regex library (YARL) to all this chaos.

# Something Different

Introducing `lens-regex-pcre`; a Haskell regular expression library which uses optics as its primary interface. Why would we be so crazy as to do something like that? Consider the things people like to do with regular expressions:

* Determine whether a **match** exists in this **text**
* Collect all the **matches** of this expression from this **text**
* Replace or modify every **match** in this **text**


Re-read those points replacing every instance of **match** with **focus** and replace **text** with **structure** and we've basically just described what optics was created to do!
If your problem matches a shape like this, then you can build an optics interface for it! Interop with optics means you **instantly** benefit from the plethora of existing optics combinators! In fact, optics fit this problem **so nicely** that the lensy wrapper I built supports **more features**, with **less code**, and runs **faster** and than the regex library it wraps! Stay tuned for more on how that's even possible near the end!

Why would we try to invent a whole new vocabulary of combinators and functions to perform these tasks when we already have a **vast**, **composable** language which most folks already have included in their applications? Granted, it's still not a great experience for newbies, but at least this way any gained knowledge is transferable; and combinators for performing different tasks are different from one another.

Skeptical about how it works out? Check it out; note how many of these solutions read almost like a normal sentence!

`lens-regex-pcre` provides `regex`, `rx`, `match`, `group` and `groups` in the following examples, everything else is regular ol' optics from the `lens` library!

We'll search through this text in the following examples:

```haskell
txt :: Text
txt = "raindrops on roses and whiskers on kittens"
```

First off, let's check if a pattern exists in the text:

```haskell
>>> has (regex [rx|wh.skers|]) txt
True
```

Looks like we found it!

The `regex` combinator constructs a traversal over all the text that matches the pattern you pass to it; `rx` is an alias of a QuasiQuoter from `pcre-light` which will compile and check your regex for you at compile time! Look; if we give it a bad pattern we find out right away!

```haskell
-- Search
>>> has (regex [rx|?|]) txt

<interactive>:1:12: error:
    • Exception when trying to run compile-time code:
        Text.Regex.PCRE.Light: Error in regex: nothing to repeat
```

Handy!

Okay! Moving on, what if we just want to find the first match? With lenses we can `firstOf` to get `Just` the first focus or `Nothing`.

Here we use a fun regex to return the first word with doubles of a letter inside; it turns out `kittens` has a double `t`!

We use `match` to say we want to extract the text that was matched.

```haskell
>>> firstOf (regex [rx|\w*(\w)\1\w*|] . match) txt
Just "kittens"

-- Alias: ^?
>>> txt ^? regex [rx|\w*(\w)\1\w*|] . match
Just "kittens"
```

Next we want to get ALL the matches for a pattern, this one is probably the most common task we want to perform, luckily it's common when working with optics too!

Let's find all the words starting with 'r' using `toListOf`

```haskell
>>> toListOf (regex [rx|\br\w*|] . match) txt
["raindrops","roses"]

-- ALIAS: ^..
>>> txt ^.. regex [rx|\br\w*|] . match
["raindrops","roses"]
```

What if we want to count the number of matches instead?

```haskell
>>> lengthOf (regex [rx|\br\w*|]) txt
2
```

Basically anything you can think to ask is already provided by `lens`

```haskell
-- Are any matches contain "drop"?
>>> anyOf (regex [rx|\br\w*|] . match) (T.isInfixOf "drop") txt
True

-- Are all of our matches greater than 3 chars?
>>> allOf (regex [rx|\br\w*|] . match) ((>3) . T.length) txt
True

-- "Is 'roses' one of our matches"
>>> elemOf (regex [rx|\br\w*|] . match) "roses" txt
True
```

## Substitutions and replacements

But that's not all! We can edit and mutate our matches in-place! This is something that the lensy interface does much better than any regex library I've ever seen. Hold my beer.

We can do the boring basic regex replace without even breaking a sweat:

```haskell
>>> set (regex [rx|\br\w*|] . match) "brillig" txt
"brillig on brillig and whiskers on kittens"

-- Alias .~
>>> txt & regex [rx|\br\w*|] . match .~ "brillig"
"brillig on brillig and whiskers on kittens"
```

Now for the fun stuff; we can **mutate** a match in-place!

Let's reverse all of our matches:

```haskell
>>> over (regex [rx|\br\w*|] . match) T.reverse txt
"spordniar on sesor and whiskers on kittens"

-- Alias %~
>>> txt & regex [rx|\br\w*|] . match %~ T.reverse
"spordniar on sesor and whiskers on kittens"
```

Want to replace matches using a list of substitutions? No problem! We can use `partsOf`

```haskell
>>> txt & partsOf (regex [rx|\br\w*|] . match) .~ ["one", "two"]
"one on two and whiskers on kittens"

-- Providing too few simply leaves extras alone
>>> txt & partsOf (regex [rx|\br\w*|] . match) .~ ["one"]
"one on two and whiskers on kittens"

-- Providing too many performs as many substitutions as it can
>>> txt & partsOf (regex [rx|\br\w*|] . match) .~ ["one", "two", "three"]
"one on two and whiskers on kittens"
```

We can even do mutations which require effects!

Let's replace every Haskell filename in a block of text with its contents from our hard-drive:

```haskell
manuscript :: T.Text
manuscript = [r|
# My Awesome Book

Check out this cool code snippet:

HelloWorld.hs

And this one too!

Fibonacci.hs

|]

--                    Match hs files          Read file contents as text
>>> traverseOf (regex [rx|\w+\.hs|] . match) (T.readFile . T.unpack) manuscript 
-- Prints (assuming you have these files sitting around):
# My Awesome Book

Check out this cool code snippet:

print "Hello, world!"

And this one too!

fix $ \fibs -> 0 : 1 : Prelude.zipWith (+) fibs (Prelude.tail fibs)
```

That may be a smidge confusing at first, but when you think about what we've managed to do in a single line of code I think it's pretty impressive.

It works with any traversal, so you can do dictionary lookups, database requests, even perform operations concurrently if we like.

And we haven't even looked at groups yet!

## Using Groups

Any sufficiently tricky regex problem will need groups eventually! `lens-regex-pcre` supports that!

Instead of using `match` after `regex` we just use `groups` instead! It's that easy:

Let's say we want to collect only the names of every variable in a template string:

```haskell
template :: T.Text
template = "Hello $NAME, glad you came to $PLACE"

>>> toListOf (regex [rx|\$(\w+)|] . group 0) template
["NAME","PLACE"]
```

You can substitute/edit groups too!

What if we got all our our area codes and local numbers messed up in our phone numbers? We can fix that in one fell swoop:

```haskell
phoneNumbers :: T.Text
phoneNumbers = "555-123-4567, 999-876-54321"

>>> phoneNumbers & regex [rx|(\d{3})-(\d{3})|] . groups %~ Prelude.reverse
"123-555-4567, 876-999-54321"
```

Anyways, at this point I'm rambling, but I hope you see that this is too useful of an abstraction for us to give up!

Huge thanks to everyone who has done work on `pcre-light` and `pcre-heavy`; and of course everyone who helped to build `lens` too! This wouldn't be possible without both of them!

The library also has a Text interface wor supporting regex over unicode!

# Performance

Typically one would expect that the more expressive an interface, the worse it would perform, in this case the opposite is true! `lens-regex-pcre` utilizes `pcre-heavy` ONLY for regex compilation and finding match positions with `scanRanges`, that's it! In fact, I don't use `pcre-heavy`'s built-in support for replacements **at all**! After finding the match positions it lazily walks over the full bytestring splitting it into chunks. Chunks are tagged with whether they're a match or not, then the "match" chunks are split further to represent whether the text is in a group or not. This allows us to implement all of our regex operations as a simple traversal over a nested list of Either's. These traversals are the ONLY things we actually need to implement, all other functionality including listing matches, filtering matches, and even setting or updating matches already exists in `lens` as generic optics combinators!

This means I didn't need to optimize for replacements or for viewing separately, because I didn't optimize for specific actions **at all**! I just built a single Traversal, and everything else follows from that. There was a little bit of fiddly logic involved with splitting the text up into chunks, but after that it all gets pretty easy to reason about. To optimize the Traversal itself I was easily able to refactor things to use ByteString 'Builder's rather than full ByteStrings, which have much better concatenation performance. This single change took `lens-regex-pcre` from being about **half the speed** of `pcre-heavy` to being on the fast side of **equal** for search, and **~10% faster** for replacements. It's just as fast for arbitrary **modifications**, which is something other regex libraries simply don't support. If there's a need for it, it can also support things like inverting the match to operate over all **unmatched** text, or things like splitting up a text on matches, etc.

I suspect that these performance improvements could also be back-ported to pcre-heavy if anyone has the desire to do so, I'd be curious if it works just as well for `pcre-heavy` as it did for `lens-regex-pcre`.
