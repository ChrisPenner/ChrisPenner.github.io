---
title: "A cleaner Haskell regular expression interface"
author: Chris Penner
date: Sep 8, 2019
tags: [haskell]
description: Fusing lenses and regular expressions is greater than the sum of their parts.
---

Regardless which language you're coming from, regular expressions are a core tool in the programmer's toolbox. Regex is a veritable swiss-army knife. They're succinct and well-defined; a power tool of choice whenever you need to get something done quickly. Though there may be more readable options out there, it's indisputably an important part of the programming landscape.

This is all a lead-up to how surprised I was at **just how hard** it is to use regular expressions in Haskell! A common response when beginners ask how to use regex in Haskell is to "learn parser combinators"; or they post a link to one of the myriad of disjointed front-end back-end combinations which must be stitched together in order to work. Or perhaps they link to the magical "do everything" operator, which uses type annotations to change its behaviour:

```haskell
(=~) :: ( RegexMaker Regex CompOption ExecOption source2
        , RegexContext Regex source1 target
        ) => source1 -> source2 -> target
```

Eazy-peazy-lemon-squeazy right? 

Nah, not so much!

Now let's say you not only want a regex library that's fast, you're greedy enough to want it to support [PCRE](https://www.pcre.org/)! For those who don't know what PCRE is, that's likely because it's been around since 1997, so you probably just know it as "Regular Expressions". As it turns out, many of the cornucopia of regex libs don't support it! 

That means:

* No extended character classes like `\d`, `\w`, etc.
* You need to escape all your modifiers like `+`, `()`, etc.
* No greedy or possesive matches like `.*?` or `.*+`
* No Lookahead/Lookbehind
* No Named capture groups (These also aren't supported by all PCRE versions)


Read more [here](https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended)

Long story short, you want PCRE. It came out over 20 years ago after all, it'd be starting University by now.

---

Okay, so we need PCRE, anything else?

Believe it or not, most Haskell regex libs have zero support for replacement/substitution!

This has been a long standing problem; here's a [blog post from 2010](http://0xfe.blogspot.com/2010/09/regex-substitution-in-haskell.html) which starts off with the familiar sentiment:

> I'm shocked and appalled at the fact that there is no generic regex substitution function in the GHC libraries. All I'm looking for is a simple function equivalent to perl's s/.../.../ expression.

Continuing on...

> While this [regex-compat] works well, it does not use PCRE, and as far as I can tell, there's no support for ByteStrings.

Also check out [this stack overflow post](https://stackoverflow.com/questions/3847475/haskell-regex-substitution) where effectively the only solution was to implement a method of substitution yourself.

Even if you could find a way to do substution, I have yet to find a **single** regex library in **any** language which allows you to do anything other than a **static** **global** replacement. That is, you must replace EVERY instance of a given string with the **same** replacement string. You can't 'edit' or 'update' the match, and you can't replace different matches with different strings. This seems like something we would have figured out by now.

---

I don't link to these to throw shade at any of these libraries; implementing a (fast) regular expression library is really hard, and so is interface design!

I just think there's still room for improvement, hopefully this helps set the stage for why I decided to add yet another regex library (YARL) to all this chaos.

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


---

Adaptability; if there's a strong desire for it `lens-regex-pcre` can easily and efficiently support other strange operations like inverting a match; (e.g. focusing all text that ISN'T in a match), splitting a file on matches, annotating text with whether it's in a capture group, etc.
