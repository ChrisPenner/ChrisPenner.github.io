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

```haskell
>>> firstOf (regex [rx|\w*(\w)\1\w*|] . match) txt
Just "kittens"

-- Preview and ^? are aliases of the same operation
>>> preview (regex [rx|\w*(\w)\1\w*|] . match) txt
Just "kittens"

>>> txt ^? regex [rx|\w*(\w)\1\w*|] . match
Just "kittens"
```

Next we want to get ALL the matches for a pattern, this one is probably the most common task we want to perform, luckily it's common when working with optics too!

Let's find all the words starting with 'r' using a regex; `^..`


```haskell
>>> txt ^.. regex [rx|\br\w*|] . match
["raindrops","roses"]
```


