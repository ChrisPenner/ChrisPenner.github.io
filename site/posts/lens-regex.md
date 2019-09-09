---
title: "A cleaner Haskell regular expression interface"
author: Chris Penner
date: Sep 8, 2019
tags: [haskell]
description: Fusing lenses and regular expressions is greater than the sum of their parts.
---





Regular expressions are a core tool in the programmer's toolbox; they're veritable swiss-army knives. The programmer becomes a master of the arcane arts, battling an endless army of text armed only with they're Magyver-esque ability to string together archaic symbols in exactly the right order to return exactly the text they need.

When I first came to Haskell from mostly Python and Golang I practiced my budding Haskell skills by trying a programming problems from things like [Advent of Code](https://adventofcode.com). Almost every one of these puzzles includes 

Naturally I assumed that EVERY language would want to provide its patrons with such power and immediately searched for how to use regular expressions in Hasekll. Except... this is weird... Haskell's "base" library doesn't have any regular expression tools in it?? Wut!? I was aghast! How was I expected to extract the precise bits & bobs I needed from the flood of text that is stdin so I could solve my problem and win my stars?

After a bit of google practice I discovered that Haskell is much more partial to parsers; but as a baby Haskeller who just wanted to parse a handfull of numbers this seemed to be an entirely excessive and unnecessary diversion.  Parsers are great, I did eventually follow that rabbit hole and reap the benefits, but there's such a thing as "overkill"; and learning parser combinators to extract the numbers from `"42 x 27"` is the definition.

I was wise enough to realize this at the time, so I kept looking; and came across the plethora of regex libraries, all trying to solve a different problem, nearly none of them supporting the wonderful `PCRE` compliant goodness I'd come to know and love. Almost all of them required installing a separate regex implementation as a C-dependency. Even after I managed to pick one and get the C-libraries installed I now needed to traverse the dangerous obfuscated terrain surrounding the dangerously overloaded `=~` operator.

If you haven't seen it before, basically everything you want to do with the regex lib is done using the same operator; here's the signature:

```haskell
(=~) :: (RegexMaker Regex CompOption ExecOption source, RegexContext Regex source1 target) => source1 -> source -> target
```

Eazy-peazy-lemon-squeazy right? After regaining consciousness from my brain shutting down on the first read I sought out some examples to figure out how to use this thing.
I'll make the long story short and just say that it was pretty confusing and complicated, not a great experience.

# Something Different

Introducing `lens-regex-pcre`; a Haskell regular expression library which uses optics as its primary interface. Why would we be so bold as to do something like that? Consider the things people like to do with regular expressions:

* Determine whether a **match** exists in this **text**
* Collect all the **matches** of this expression from this **text**
* Replace or modify every **match** with this **text**

Replace every instance of **match** with **focus** and replace **text** with **structure** and we've just describe the value proposition of optics!

Why would we try to invent a whole new vocabular of combinators and functions to perform these tasks when we already have a **vast**, **composable** language which most folks already have included in their applications? Granted, it's still not a great experience for newbies, but at least this way any gained knowledge is transferable; and combinators for performing different tasks are different from one another.

Skeptical about how it works out? Check it out.

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

The `regex` combinator constructs a traversal over everything that matches the pattern you pass to it; `rx` is a QuasiQuoter from `pcre-light` which will compile and check your regex for you at compile time! Look; if we give it a bad pattern we find out right away!

```haskell
-- Search
>>> has (regex [rx|(kitten|]) txt

<interactive>:1:12: error:
    • Exception when trying to run compile-time code:
        Text.Regex.PCRE.Light: Error in regex: missing ")"
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

Let's find all the words starting with 'r' using a regex; `^..`

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
-- Any matches contain "drop"?
>>> anyOf (regex [rx|\br\w*|] . match) (T.isInfixOf "drop") txt
True

-- All of our matches greater than 3 chars?
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

Let's reverse all ov our matches:

```haskell
>>> over (regex [rx|\br\w*|] . match) T.reverse txt
"spordniar on sesor and whiskers on kittens"

-- Alias %~
>>> txt & regex [rx|\br\w*|] . match %~ T.reverse
"spordniar on sesor and whiskers on kittens"
```

Want to replace matches using a list of substitutions? No problem! We can use `partsOf`

```haskell
>>> set (partsOf (regex [rx|\br\w*|] . match)) ["one", "two"] txt
"one on two and whiskers on kittens"

-- Providing too few simply leaves extras alone
>>> set (partsOf (regex [rx|\br\w*|] . match)) ["one"] txt
"one on two and whiskers on kittens"

-- Providing too many performs as many substitutions as it can
>>> set (partsOf (regex [rx|\br\w*|] . match)) ["one", "two", "three"] txt
"one on two and whiskers on kittens"
```

We can even do mutations which require effects!

Let's replace every Haskell filename with it's contents from our hard-drive:

```haskell
manuscript :: T.Text
manuscript = [r|
# My Awesome Book

Check out this cool code snippet:

HelloWorld.hs

And this one too!

Fibonacci.hs

|]

>>> traverseOf (regex [rx|\w+\.hs|] . match) (T.readFile . T.unpack) manuscript >>= T.putStr
-- Prints:
# My Awesome Book

Check out this cool code snippet:

print "Hello, world!"


And this one too!

fix $ \fibs -> 0 : 1 : Prelude.zipWith (+) fibs (Prelude.tail fibs)
```

That may be a smidge confusing at first, but when you think about what we've managed to do in a single line of code I think it's pretty impressive.

It works with any traversal, so you can do dictionary lookups, database requests, even perform operations concurrently!

And we haven't even looked at groups yet!

## Using Groups

Any sufficiently tricky regex problem will need groups eventually! `lens-regex-pcre` supports that!

Instead of using `match` after `regex` we just use `groups` instead! It's that easy:

Let's say we want to collect only the names of every variable in a template string:

```haskell
template :: T.Text
template = "Hello $NAME, glad you came to $PLACE"

>>> toListOf (regex [rx|\$(\w+)|] . groups . d) template
["NAME","PLACE"]
```

We need to use `_head` to select the first group because there may be many groups!

You can substitute/edit groups too!

What if we got all our our area codes and local numbers messed up in our phone numbers? We can fix that in one fell swoop!

```haskell
phoneNumbers :: T.Text
phoneNumbers = "555-123-4567, 999-876-54321"

>>> phoneNumbers & regex [rx|(\d{3})-(\d{3})|] . groups %~ Prelude.reverse
"123-555-4567, 876-999-54321"
```

Anyways, at this point I'm rambling, but I hope you see that this is too useful of an abstractin for us to give up!
