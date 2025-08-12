---
title: "Silly job interview questions in Haskell"
author: "Chris Penner"
date: "Oct 14, 2020"
tags: [haskell]
description: "Implementations of various silly job interview questions in Haskell"
---

Today I thought it'd be fun to take a look at a few common & simple "interview questions" in Haskell. These sorts of questions are often used to establish whether someone has programming and problem solving skills, and I thought it might be useful for folks to see how they play out in Haskell since our beloved language's solutions tend to follow a different paradigm than most other languages do. I'll withhold any judgement on whether these questions are in any way helpful in determining programming skill whatsoever 😅; please don't @ me about it.

## Palindromes

Let's start off nice and easy with the standard "is it a palindrome" question! The task is to write a function which determines whether a given string is a palindrome (i.e. whether it reads the same in both reverse and forwards)

```haskell
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

>>> isPalindrome "racecar"
True

>>> isPalindrome "hello world!"
False
```

That'll do it! Not much to say about this one, it's nice that our definition roughly matches an English sentence describing the problem "does a given string equal itself in reverse". I'll leave it as an exercise for the reader to expand it to handle differences in capitalization however you like.

## Fizz Buzz

Next up is the infamous Fizz Buzz! For the 3 of you who are unfamiliar, for each number from 1 to 100 we need to print out "Fizz" if it's divisible by 3, "Buzz" if it's divisible by 5, and "Fizz Buzz" if it's divisible by both 3 AND 5! Otherwise we print the number itself.


Let's see it!

```haskell
import Data.Foldable

fizzle :: Int -> String
fizzle n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "Fizz Buzz!"
  | n `mod` 3 == 0 = "Fizz!"
  | n `mod` 5 == 0 = "Buzz!"
  | otherwise = show n

main :: IO ()
main = do
    for_ [1..100] (putStrLn . fizzle)


>>> main
1
2
Fizz!
4
Buzz!
Fizz!
7
8
Fizz!
Buzz!
11
Fizz!
13
14
Fizz Buzz!
16
-- ...you get the idea
```

I write a helper function "fizzle" here which converts a number into its appropriate string so I can keep the "printing" logic separate, which is good programming style in Haskell as it makes things easier to both test and reason about.

We can see that "case analysis" is very helpful for these sorts of problems, I'm using "pattern guards" to do a sort of multi-way if statement. Since "divisible by both 3 & 5" overlaps with the other conditions and also is the most restrictive, we check for that one first, then check the other two cases falling back on returning the string version of the number itself. It all works beautifully!

I really enjoy looking at this problem as an example of how Haskell is different from other languages. Most things in Haskell are *functions*, even our loops are just higher-order functions! The nice thing about that is that functions are *composable* and have very clean boundaries, which means we don't need to intermingle the **syntax** of a **for-loop** with our logic. It's these same principles which allow us to easily separate our _effectful_ printing logic from our function which computes the output string.

The next difference we can see is that we use pattern-matching, specifically "pattern guards", which allow us to select which definition of a function we want to use. It looks a bit like a glorified if-statement, but I find it's less syntactic noise once you get used to it, and there are many more things pattern guards can do!

All that's left is to loop over all the numbers and print them out one by one, which is a snap thanks to the `for_` function!

Next!

### Sum up to N problem

Here's a less-common problem that nonetheless I've still heard a few times! I think it was in one of my algorithms assignments back in the day...

The task is to take a **list of numbers** and find any **combinations of _3_ numbers** which add up to a specified total. For instance, if we want to determine all combinations of **3** numbers which add up to **15**, we'd expect our result to look something like this:

```haskell
>>> sumToN 15 [2, 5, 3, 10, 4, 1, 0]
[[2,3,10],[5,10,0],[10,4,1]]
```

Notice how each inner list sums to 15? We only care about *combinations* here, not *permutations*, so we have `[2, 3, 10]`, but don't bother with `[3, 2, 10]`!

So how will we set about implementing an algorithm for this? Well, the first thing to come to mind here is that we're finding *combinations*, then we're filtering them down to match a predicate!

In Haskell we like to split problems into smaller composable pieces, the filter part should be pretty easy, so let's tackle the combinations problem first.

After a quick look through hackage it looks like there *is* a [`permutations`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:permutations) function, but strangely there's no `combinations` function! I suppose we could somehow try to de-duplicate the output of `permutations`, but it'll be fun to write our own version!  `combinations` are quite nice to compute recursively, so let's try it that way!

```haskell
combinations :: Int -> [a] -> [[a]]
-- Only one way to get zero things
combinations 0 _ = [[]]
combinations n (x:xs) =
    -- Get all combinations containing x by appending x to all (n-1)
    -- combinations of the rest of the list
    fmap (x:) (combinations (n-1) xs)
    -- Combine it with all combinations from the rest of the list
      <> combinations n xs
-- No elements means no combinations!
combinations _ [] = []
```

Here we're using pattern matching and recursion to do our dirty work. First we can confidently say that there's only ONE way to get 0 elements from **any** list of elements, so we can fill that in. Next we'll handle a single step, if we have at least one element left in the list, we can compute all the combinations which contain that element by prepending it to all the combinations of size `n-1` from the remainder of the list; and we'll concatenate that with all the combinations of the **rest** of the list.

Lastly we add one more pattern match which handles all invalid inputs (either negative numbers or empty lists) and simply assert that they have no valid combinations.

Let's try out our implementation before we move on to the next part.

```haskell
>>> combinations 3 [1..5]
[[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
>>> combinations 2 [1..4]
[[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
```

Feel free to take the time to convince yourself that these are correct 😀

To finish it off we need to find any of these combinations which add up to our target number.

```haskell
sumNToTotal :: Int -> Int -> [Int] -> [[Int]]
sumNToTotal n totalNeeded xs =
    filter matchesSum (combinations n xs)
  where
    matchesSum ys = sum ys == totalNeeded


>>> sumNToTotal 3 15 [2, 5, 3, 10, 4, 1, 0]
[[2,3,10],[5,10,0],[10,4,1]]
```

Great! We can simply get all possible combinations and filter out the results which don't properly sum to the expected number. One other nifty thing here is that, because Haskell is **lazy**, if we only need to find the **first** valid combination, we could just grab the first result of the list and Haskell won't do any more work than absolutely necessary.

But wait! There's a surprise **part two** of this problem:

We now have to find all combinations of ANY length which sum to a target number, lucky for us, that's pretty easy for us to adapt for!

```haskell
sumAnyToTarget :: Int -> [Int] -> [[Int]]
sumAnyToTarget totalNeeded xs
  = foldMap (\n -> sumNToTotal n totalNeeded xs) [0..length xs]

>>> sumAnyToTarget 15 [2, 5, 3, 10, 4, 1, 0]
[ [5,10]
, [2,3,10]
, [5,10,0]
, [10,4,1]
, [2,3,10,0]
, [10,4,1,0]
, [2,5,3,4,1]
, [2,5,3,4,1,0]
]
```

This new version re-uses the `sumNToTotal` function we wrote in the previous step! It iterates over each possible length of combination and finds all the winning combinations using `sumNToTotal`, then concatenates them using `foldMap`! Works out pretty cleanly if I do say so myself!


## Check if two strings are anagrams

For whatever reason, interviewers LOVE string manipulation questions; so let's try another one!

Here our task is to determine whether two strings are anagrams of each other. I'd say the difficulty for this one comes from thinking up your *strategy* rather than the implementation itself. Here's how I'd give this a go in Haskell!

```haskell
import Data.Function (on)
isAnagram :: String -> String -> Bool
isAnagram = (==) `on` sort

>>> isAnagram "elbow" "below"
True
>>> isAnagram "bored" "road"
False
>>> isAnagram "stressed" "desserts"
True
```

Here we're using a funky higher-order function called `on`; `on` takes two functions, AND THEN takes two arguments!
In this case it calls "sort" on both arguments, then checks if the sorted results are equal!
It turns out this is sufficient to know if two strings are anagrams!

But wait! What's that? What if they're in differing cases! Okay fine!

```haskell
import Data.Char (toLower)

isAnagram :: String -> String -> Bool
isAnagram a b = (==) `on` (sort . map toLower)
```

Happy now? No? What's that? It seems non-performant? Well yes, but actually no!

While it's true that sort has an `O(nlogn)` performance profile, one interesting thing here is that sorting is **lazy** in Haskell! This means that if our two strings are unequal, they will only be sorted far enough to determine inequality! In fact, if the first elements of each sorted string aren't equal to each other, then we won't bother sorting any more.

Sure, our function isn't perfect, but it's not bad, especially since this is the first approach that came to mind. Compare our 2 line solution with the [Java Solution](https://javarevisited.blogspot.com/2013/03/Anagram-how-to-check-if-two-string-are-anagrams-example-tutorial.html) provided in the post which gave me the idea for this problem. It might be more performant (though to be honest I haven't benchmarked them), but if I'm going to be reading this code often in the future, I'd much prefer the clearest version which performs at an adequate level.

## Min and Max

Here's a problem! Given a list of elements, find the smallest and largest element of that list!

I'll show and discuss three different strategies for this one.

Here's the first:

```haskell
simpleMinMax :: Ord a => [a] -> (a, a)
simpleMinMax xs = (minimum xs, maximum xs)

>>> simpleMinMax [3, 1, 10, 5]
(1,10)
```

This is the simplest way we could imagine doing this sort of thing; and indeed it does work! Unfortunately, there are few skeletons from "legacy" haskell that are hidden in this closet. Look what happens if we try it on an empty list!

```haskell
>>> simpleMinMax []
(*** Exception: Prelude.minimum: empty list
```

Oops... Haskell isn't supposed to throw exceptions! That's okay though, there are some other good ways to accomplish this which won't blow up in our faces!

Time for the next one!

```haskell
boundedMinMax :: (Bounded a, Ord a) => [a] -> (a, a)
boundedMinMax xs = coerce $ foldMap (\x -> (Min x, Max x)) xs

>>> boundedMinMax [4, 1, 23, 7] :: (Int, Int)
(1,23)
>>> boundedMinMax [] :: (Int, Int)
(9223372036854775807,-9223372036854775808)
```

This implementation might be a bit confusing if you haven't learned enough about Semigroups and Monoids, but don't let that scare you!
These are both very common abstractions in Haskell and are used very often and to great effect!

A Semigroup is a type of interface which provides an implementation which lets us combine multiple elements together. Haskell has two semigroup type-wrappers which provide specific behaviour to whichever type we wrap: `Min` and `Max`!

These types define a combining operation which, any time we combine two elements, will keep only the smallest or largest value respectively!
I'm using `foldMap` here to project each list element into a tuple of these two types which, when the list is collapsed by `foldMap`, will all combine together and will include the lowest and highest elements, all in a single pass!

So what's up with the second example? Well, it's a bit unexpected, but not necessarily *wrong*. When we're missing any elements to compare foldMap will use the default value for each of our type wrappers, which it can do if they're monoids. For `Min` and `Max` the default value is the "smallest" and "largest" value of the wrapped type, which is defined by the `Bounded` interface that we require in the type signature. This works okay, and behaves as expected under _most_ circumstances, but maybe we can try one more time:

```haskell
import Data.Semigroup

minMax :: Ord a => [a] -> Maybe (a, a)
minMax xs = case foldMap (\a -> Just (Min a, Max a)) xs of
                Just (Min x, Max y) -> Just (x, y)
                _ -> Nothing

>>> minMax [4, 1, 9, 5]
Just (1,9)
>>> minMax []
Nothing
```

Okay! This is pretty much the same, but we needed an **explicit** way to correctly handle an empty list of values.
In this case, by wrapping our tuple in `Just` we invoke the `Maybe` monoid, and remember that `foldMap` is smart enough to return the "empty" element of that monoid if our list is empty! That means we get `Nothing` back if there are no elements.

This may seem like "magic" at first, but all of these *typeclasses* have **laws** which dictate their behaviour and make them predictable. I suggest learning more about monoids if you have time, they're fascinating and useful!

This is a very "safe" implementation, in fact much safer than most languages would offer. We explicitly return `Nothing` in the case that the list is empty, and the `Maybe` return type requires the caller to handle that case. I mentioned earlier how functions are composable, and it turns out that data-types are too! If we pair two objects with a semigroup together in a tuple, that tuple has a semigroup instance too, which combines respective element together when we combine tuples!

## Word Frequency

This is a pretty popular one too!

The challenge this time is, given a block of text, find the most common word!

Ultimately, this comes down to an understanding of data-structures.

```haskell
import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.Map as M

mostCommonWord :: String -> Maybe String
mostCommonWord str =
    if null wordCounts
       then Nothing
       else Just . fst . maximumBy (compare `on` snd) . M.toList $ wordCounts
  where
    wordCounts = M.unionsWith (+) . fmap (\w -> M.singleton w 1) . words $ str
```

There's a bit more going on this time, so let's break it down a bit!

In Haskell, we use "math-style" function composition using `.`, so we read most expressions from right-to-left.

Let's look at the `wordCounts` binding down in the `where` clause first. Reading from right to left, first we use the `words` function from the built-in Prelude to split the incoming stream into a list of words, then we create a key-value map out of each one, consisting of the word as the key with a value of `1` to start.

Now we have a list of key-value maps, and can add them up all up key-wise using `unionsWith` from the `Data.Map` library, this will count up the number of elements of each key and will result in a key-value mapping where the values represent occurrences.

We've got a mapping now, so let's find the largest count! 

First things first, to be safe we'll check whether the map has any values at all, if it doesn't then we'll return `Nothing`.
Otherwise, we can convert the map into a list of key-value pairs by calling `M.toList`, then we can use `maximumBy` to return the biggest element according to a comparison function that we specify! `on` comes in handy here and we can tell it to compare on the second element, which is the count. That will return us the key-value pair with the largest value, then we just need to grab the key as a result using `fst`!

Ultimately this is a bit of a naive implementation which won't work well on huge texts, but it should be enough to get you through the whiteboard portion of the interview 😄.

## Summary

That's all I've got for you today, nothing too revolutionary I'm sure, but hopefully you had a bit of fun, or maybe learned a thing or two about what code looks like in Haskell compared to your favourite language 😄
