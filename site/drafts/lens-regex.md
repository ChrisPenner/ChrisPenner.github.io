---
title: "Optics \& Regex: Greater than the sum of their parts"
author: Chris Penner
date: Sep 8, 2019
tags: [haskell]
description: Optics \& regex: greater than the sum of their parts.
---

Regardless of the programming language, regular expressions have always been a core tool in the programmer's toolbox. Though some have a distaste for their difficult to maintain nature, they're an adaptable quick'n'dirty way to get things done.

As much love as I have for Regex, they've become an incredibly hacky thing; they support a lot of options and a lot of different behaviours so the way they end up being used is a little less than "principled" these days.

# The Status Quo

I don't know about you, but I've found almost every regular expression interface I've ever used in any language to be a bit clunky and inelegant; here are a few reasons why I think that's true:

* Regular expressions can be used to either get **or** set
* Sometimes you want only **one** match, sometimes a few, sometimes you want **all** of them!
* Sometimes you want **just** the match groups; sometimes you want the whole match, sometimes you want **BOTH**!
* Regular Expression searching is **expensive**; we want to be **lazy** a to avoid work!
* Regular expressions are simply text; what if it's not valid?

Think about designing a single interface which can support ALL of the following operations performantly:

* Get me the second match group from the first three matches
* Replace only the first match with this text
* Get me all groups AND match text from ALL matches
* Replace the first match with this value, the next with this one, and so on...
* Lazily get me the full match text of the first 2 matches where match-group 1 has a certain property.

Yikes... That's going to take either a lot of methods or a lot of options!

Luckily Haskell has a few tricks that help make some of these inherently difficult things a bit easier. Inherently lazy data structures and computations allows us to punt off laziness to the language rather than worrying about how to do the minimal amount of work possible. TemplateHaskell allows us to statically check Regular Expressions to ensure they're valid at compile time, and could even possibly allow us to statically analyze the existence of match groups. But that still leaves a lot of surface area to cover! It's easy to see how come these interfaces are complicated!

That leaves us with simply specifying what we want to happen. In a language like Haskell which doesn't have keyword or optional arguments it means we have to either overload operators with a lot of different meanings based on context; or provide a LOT of functions that the user has to learn, increasing our API's surface area. You may be familiar with the laughably overloaded "do everything" regex operator in many Haskell regex libs:

```haskell
(=~) :: ( RegexMaker Regex CompOption ExecOption source2
        , RegexContext Regex source1 target
        ) => source1 -> source2 -> target
```

And even that doesn't handle replacement!

Overloading is one approach, but as it turns out, it requires a lot of spelunking through types and documentation to even find out what the valid possible uses are! I'm going to rule out this approach as unwieldy. That leaves us with the other option; add a whole bunch of methods or options, which doesn't sound great either, mainly because I don't want someone to need to learn a dozen **specialized** functions just to use **my** library. If only there was some existing vocabulary of operations which could be composed in different permutations to express complex ideas!

# Something Different

Introducing `lens-regex-pcre`; a Haskell regular expression library which uses optics as its primary interface.

Think about what regular expressions are meant to do; they're an interface which allows you to **get** or **set** zero or more small pieces of text in a larger whole. This is practically the **dictionary definition** of a Traversal in optics!  Interop with optics means you **instantly** benefit from the plethora of existing optics combinators! In fact, optics fit this problem **so nicely** that the lensy wrapper I built supports **more features**, with **less code**, and runs _**faster**_ and than the regex library it wraps! Stay tuned for more on how that's even possible near the end!

Using optics as an interface has the benefit that the user is either **already familiar** with most of the combinators and tools they'll need from using optics previously, or that everything they learn here is transferable into work with optics in the future! As more optics are discovered, added, and optimized, the regex library passively benefits without any extra work from anyone!

I don't want to discount the fact that optics can be tough to work with; I'm aware that they have a strong reputation of being too hard to learn and having poor type-inference and tricky error messages. I'm [doing my best to address those problems](https://opticsbyexample.com/), and there are new optics libraries coming out every year that improve error messages and usability! Despite current inconveniences, optics are fundamental constructions which model problems well; I believe **optics are inevitable**! So rather than shying away from an incredibly elegant solution because of a few temporary issues with the domain I'd rather push through them, use all the power the domain provides me, and continue to do all I can to chip away at the usability problems over time. If optics have ever seemed inaccessible to you in the past; please be willing to give them another try with a [bit of helpful direction](https://opticsbyexample.com/)!

> Optics are inevitable.

Okay! I'll put my soapbox away, now it's time to see how it actually works. Notice how most of the following examples actually read roughly like a sentence!

# Examples

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
>>> toListOf (regex [rx|\<r\w*|] . match) txt
["raindrops","roses"]

-- ALIAS: ^..
>>> txt ^.. regex [rx|\<r\w*|] . match
["raindrops","roses"]
```

What if we want to count the number of matches instead?

```haskell
>>> lengthOf (regex [rx|\<r\w*|]) txt
2
```

Basically anything you can think to ask is already provided by `lens`

```haskell
-- Do any matches contain "drop"?
>>> anyOf (regex [rx|\<r\w*|] . match) (T.isInfixOf "drop") txt
True

-- Are all of our matches greater than 3 chars?
>>> allOf (regex [rx|\<r\w*|] . match) ((>3) . T.length) txt
True

-- "Is 'roses' one of our matches"
>>> elemOf (regex [rx|\<r\w*|] . match) "roses" txt
True
```

## Substitutions and replacements

But that's not all! We can edit and mutate our matches in-place! This is something that the lensy interface does much better than any regex library I've ever seen. Hold my beer.

We can do the boring basic regex replace without even breaking a sweat:

```haskell
>>> set (regex [rx|\<r\w*|] . match) "brillig" txt
"brillig on brillig and whiskers on kittens"

-- Alias .~
>>> txt & regex [rx|\<r\w*|] . match .~ "brillig"
"brillig on brillig and whiskers on kittens"
```

Now for the fun stuff; we can **mutate** a match in-place!

Let's reverse all of our matches:

```haskell
>>> over (regex [rx|\<r\w*|] . match) T.reverse txt
"spordniar on sesor and whiskers on kittens"

-- Alias %~
>>> txt & regex [rx|\<r\w*|] . match %~ T.reverse
"spordniar on sesor and whiskers on kittens"
```

Want to replace matches using a list of substitutions? No problem! We can use `partsOf`

```haskell
>>> txt & partsOf (regex [rx|\<r\w*|] . match) .~ ["one", "two"]
"one on two and whiskers on kittens"

-- Providing too few simply leaves extras alone
>>> txt & partsOf (regex [rx|\<r\w*|] . match) .~ ["one"]
"one on two and whiskers on kittens"

-- Providing too many performs as many substitutions as it can
>>> txt & partsOf (regex [rx|\<r\w*|] . match) .~ ["one", "two", "three"]
"one on two and whiskers on kittens"
```

We can even do mutations which require effects!

Let's find and replace variables in a block of text with strings from environment variables!

```haskell
import qualified Data.Text as T
import Control.Lens
import Control.Lens.Regex
import System.Environment
import Data.Text.Lens

src :: T.Text
src = "Hello $NAME, how's your $THING?"

replace :: T.Text -> IO T.Text
replace = regex [rx|\$\w+|] . match . unpacked %%~ getEnv . tail
```

Let's run it:

```haskell
>>> setEnv "NAME" "Joey"
>>> setEnv "THING" "dog"
>>> replace src
"Hello Joey, how's your dog?"
```

When you think about what we've managed to do in a single line of code I think it's pretty impressive.

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

## Bringing it in

So with this new vocabulary how do we solve the problems we posed earlier?

* Get me the second match group from the first three matches

```haskell
>>> "a:b, c:d, e:f, g:h" ^.. taking 3 (regex [rx|(\w):(\w)|] . group 1)
["b","d","f"]
```

* Replace only the first match with this text

```haskell
>>> "one two three" & regex [rx|\w+|] . index 0 . match .~ "new"
"new two three"
```

* Get me all groups AND match text from ALL matches

```haskell
>>> "a:b, c:d, e:f" ^.. regex [rx|(\w):(\w)|] . matchAndGroups
[("a:b",["a","b"]),("c:d",["c","d"]),("e:f",["e","f"])]
```

* Replace the first match with this value, the next with this one, and so on...

```haskell
-- If we get more matches than replacements it just leaves the extras alone
>>> "one two three four" & partsOf (regex [rx|\w+|] . match) .~ ["1", "2", "3"]
"1 2 3 four"
```

* Lazily get me the full match text of the first 2 matches where match-group 1 has a certain property.

```haskell
-- The resulting list will be lazily evaluated!
>>> "a:b, c:d, e:f, g:h" 
      ^.. regex [rx|(\w):(\w)|] 
      . filtered (has (group 0 . filtered (> "c"))) 
      . match
["e:f","g:h"]
```

Anyways, at this point I'm rambling, but I hope you see that this is too useful of an abstraction for us to give up!

Huge thanks to everyone who has done work on `pcre-light` and `pcre-heavy`; and of course everyone who helped to build `lens` too! This wouldn't be possible without both of them!

The library has a Text interface for supporting regex over unicode, and a `ByteString` interface for when you've gotta go **fast**!

# Performance

Typically one would expect that the more expressive an interface, the worse it would perform, in this case the opposite is true! `lens-regex-pcre` utilizes `pcre-heavy` ONLY for regex compilation and finding match positions with `scanRanges`, that's it! In fact, I don't use `pcre-heavy`'s built-in support for replacements **at all**! After finding the match positions it lazily walks over the full ByteString splitting it into chunks. Chunks are tagged with whether they're a match or not, then the "match" chunks are split further to represent whether the text is in a group or not. This allows us to implement all of our regex operations as a simple traversal over a nested list of Eithers. These traversals are the ONLY things we actually need to implement, all other functionality including listing matches, filtering matches, and even setting or updating matches already exists in `lens` as generic optics combinators!

This means I didn't need to optimize for replacements or for viewing separately, because I didn't optimize for specific actions **at all**! I just built a single Traversal, and everything else follows from that.

You heard that right! I didn't write ANY special logic for viewing, updating, setting, or anything else! I just provided the appropriate traversals, optics combinators do the rest, and it's still performant!

There was a little bit of fiddly logic involved with splitting the text up into chunks, but after that it all gets pretty easy to reason about. To optimize the Traversal itself I was easily able to refactor things to use ByteString 'Builder's rather than full ByteStrings, which have much better concatenation performance. 

With the caveat that I don't claim to be an expert at benchmarks; (please [take a look](https://github.com/ChrisPenner/lens-regex-pcre/blob/master/bench/Bench.hs) and tell me if I'm making any critical mistakes!) this single change took `lens-regex-pcre` from being about **half the speed** of `pcre-heavy` to being on the **faster side** of **equal** for search, and **~10% faster** for replacements. It's just as fast for arbitrary **modifications**, which is something other regex libraries simply don't support. If there's a need for it, it can also trivially support things like inverting the match to operate over all **unmatched** text, or things like splitting up a text on matches, etc.

I suspect that these performance improvements could also be back-ported to `pcre-heavy` if anyone has the desire to do so, I'd be curious if it works just as well for `pcre-heavy` as it did for `lens-regex-pcre`.
