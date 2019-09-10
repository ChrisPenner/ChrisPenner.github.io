---
title: "A cleaner Haskell regular expression interface"
author: Chris Penner
date: Sep 8, 2019
tags: [haskell]
description: Fusing lenses and regular expressions is greater than the sum of their parts.
---



Regular expressions are a core tool in the programmer's toolbox; they're veritable swiss-army knives. Armed with regular expressions the programmer becomes a master of the arcane arts, battling an endless slew of text armed only with their Magyver-esque ability to string together archaic symbols in exactly the right order to return exactly the text they need.

This post isn't here to dunk on the other regular expression libraries out there, but in my opinion some of them are a bit tricky to use and don't give me the flexibility to do what I want with the clean interface I want. For instance, most of the libs in use these days use the following combinator for pretty much everything:

```haskell
(=~) :: (RegexMaker Regex CompOption ExecOption source, RegexContext Regex source1 target) => source1 -> source -> target
```

Eazy-peazy-lemon-squeazy right? 

Anyways; I'm going to show you a new interface which I hope I can convince you is sensible, adaptable, and practical. Even better; you might already have used it without even knowing it!

# Something Different

Introducing `lens-regex-pcre`; a Haskell regular expression library which uses optics as its primary interface. Why would we be so bold as to do something like that? Consider the things people like to do with regular expressions:

* Determine whether a **match** exists in this **text**
* Collect all the **match** of this expression from this **text**
* Replace or modify every **match** with this **text**

Re-read those points replacing every instance of **match** with **focus** and replace **text** with **structure** and we've basically just described what optics was created to do!

Why would we try to invent a whole new vocabulary of combinators and functions to perform these tasks when we already have a **vast**, **composable** language which most folks already have included in their applications? Granted, it's still not a great experience for newbies, but at least this way any gained knowledge is transferable; and combinators for performing different tasks are different from one another.

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

>>> toListOf (regex [rx|\$(\w+)|] . groups . _head) template
["NAME","PLACE"]
```

We need to use `_head` to select the first group because there may be many groups.

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
