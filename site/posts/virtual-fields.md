---
title: 'Virtual Record Fields Using Lenses'
author: "Chris Penner"
date: "Nov 26, 2020"
tags: [haskell, optics]
description: "Lenses are commonly used for getting and setting fields on records, but they're actually much more adaptable than that! This post dives into the idea of 'virtual fields' using optics."
image: virtual-fields.jpg
---

The following blog post is a short excerpt from by book on optics: ["Optics By Example"](https://leanpub.com/optics-by-example/c/virtual-fields). If you learn something from the post you'll likely enjoy the rest of the book too! 

Optics by Example provides a comprehensive example-driven guide to 
manipulating data with optics, covering Lenses, Traversals, Prisms, Isos, as well
as many design patterns and extension libraries.

As thanks for checking out the blog you can grab a copy on sale with [this link](https://leanpub.com/optics-by-example/c/virtual-fields) until the end of 2020.

---

Lenses are commonly used for getting and setting fields on records, but they're
actually much more adaptable than that! This post dives into the idea of "virtual fields" using optics. 

Virtual fields can can provide many benefits:

* They help you adapt to change in your modules and types without breaking backwards-compatibility
* Provide a uniform interface for "smart" getters and setters which maintain data invariants.
* Make your code more resilient to refactoring.

Let's dive in!

## What is a virtual field

To establish terms, I'll define a **virtual field** as any piece of data we might
be interested in which doesn't exist as a concrete field in a given record definition. 
In languages like Java or Python these are sometimes called "computed properties" or "managed attributes". 

Oftentimes we'll use virtual fields to present data from concrete fields in a 
more convenient way, or to maintain certain invariants on the concrete fields.
Sometimes virtual fields combine several concrete fields together, other times 
they're used to avoid introducing breaking changes when refactoring the structure of the record.

No matter what you use them for, at the end of the day they're really just 
normal lenses! Let's look at a concrete example.

## Writing a virtual field

Let's look at the following type:

```haskell
data Temperature =
    Temperature { _location :: String
                , _celsius  :: Float
                }
    deriving (Show)
makeLenses ''Temperature
```

This generates the field lenses:

```haskell
location :: Lens' Temperature String
celsius  :: Lens' Temperature Float
```

Which we can use to **get**, **set**, or **modify** the temperature in Celsius like so:

```haskell
>>> let temp = Temperature "Berlin" 7.0
>>> view celsius temp
7.0

>>> set celsius 13.5 temp
Temperature {_location = "Berlin", _celsius = 13.5}

-- Bump the temperature up by 10 degrees Celsius
>>> over celsius (+10) temp
Temperature {_location = "Berlin", _celsius = 17.0}
```

Now what about our American colleagues who'd prefer **Fahrenheit**? 
It's easy enough to write a function which converts **Celsius** to **Fahrenheit** 
and call that on the result of `celsius`, but we'd still need to **set** new temperatures in **Celsius**!
How can we avoid this dissonance between units?

First we'll define our conversion functions back and forth, nothing too interesting there, if I'm honest I just stole the formulas from wikipedia:

```haskell
celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5/9)
```

Here's *one way* we could get and set using Fahrenheit:

```haskell
>>> let temp = Temperature "Berlin" 7.0
-- View temp in Berlin in Fahrenheit 
>>> celsiusToFahrenheit . view celsius temp
44.6

-- Set temperature to 56.3 Fahrenheit
>>> set celsius (fahrenheitToCelsius 56.3) temp
Temperature {_location = "Berlin", _celsius = 13.5}

-- Bump the temp up by 18 degrees Fahrenheit
>>> over celsius (fahrenheitToCelsius . (+18) . celsiusToFahrenheit) temp
Temperature {_location = "Berlin", _celsius = 17.0}
```

The first two aren't **too** bad, but the `over` example is getting a bit clunky and error prone!
It's hard to see what's going on, and since every type is `Float` it'd be easy to
forget or misplace one of our conversions.

If we instead encode the **Fahrenheit** version of the temperature as a **virtual field** using optics
we gain improved usability, cleaner code, and avoid a lot of possible mistakes.

Let's see what that looks like.

We can write a `fahrenheit` lens in terms of the existing `celsius` lens! 
We embed the back-and-forth conversions into the lens itself.

```haskell
fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = set celsius (fahrenheitToCelsius f) temp
```

Look how much it cleans up the call sites:

```haskell
>>> let temp = Temperature "Berlin" 7.0
>>> view fahrenheit temp
44.6

>>> set fahrenheit 56.3 temp
Temperature {_location = "Berlin", _celsius = 13.5}

>>> over fahrenheit (+18) temp
Temperature {_location = "Berlin", _celsius = 17.0}
```

Much nicer, easier to read, and less error prone! Even though our `Temperature` 
record doesn't actually have a concrete field for 
the temperature in **Fahrenheit** we managed to fake it by using lenses to create a **virtual field**!
If we export a smart constructor for our Temperature type and only export the lenses
from our Temperature module then the two field lenses are completely indistinguishable.

## Breakage-free refactoring

In addition to providing more functionality in a really clean way, another 
benefit of using lenses instead of field accessors for interacting with our data 
is that we gain more freedom when refactoring. 

To continue with the Temperature example, let's say as we've developed our 
wonderful weather app further we've discovered that Kelvin is a much better 
canonical representation for temperature data. We'd love to swap our `_celsius` 
field for a `_kelvin` field instead and base all our measurements on that.

We'll consider two possible alternate universes, in the first, this post was never written, 
so we didn't use lenses to access our fields 😱

In the second (the one you're living in) I published this post and of course 
knew well enough to use lenses as the external interface instead.

## A world without lenses

In the sad universe without any lenses we had the following code scattered throughout our app:

```haskell
updateTempReading :: Temperature -> IO Temperature
updateTempReading temp = do
  newTempInCelsius <- readOutdoorTemp
  return temp{_celsius=newTempInCelsius}
```

Then we refactored our `Temperature` object to the following:

```haskell
data Temperature =
    Temperature { _location :: String
                , _kelvin  :: Float
                }
    deriving (Show)
makeLenses ''Temperature
```

Now, unfortunately, every file that used record update syntax now fails to compile.
This is because the `_celsius` field we are depending on with our record-update-syntax
no longer exists. If we had instead used positional pattern matching the situation would be even worse:

```haskell
updateTempReading :: Temperature -> IO Temperature
updateTempReading (Temperature location _) = do
  newTempInCelsius <- readOutdoorTemp
  return (Temperature location newTempInCelsius)
```

In this case the code **will still happily compile**, 
but we've switched measurement units this code is now completely incorrect!

## The glorious utopian lenses universe

Come with me now to the happy universe. In this world we've decided to use lenses 
as our interface for interacting with `Temperature`s, meaning we didn't expose 
the field accessors and thus disallowed fragile record-update syntax. 
We used the `celsius` lens to perform the update instead:

```haskell
updateTempReading :: Temperature -> IO Temperature
updateTempReading temp = do
  newTempInCelsius <- readTemp
  return $ set celsius newTempInCelsius temp
```

Now when we refactor, we can export a replacement `celsius` lens in place of the old generated one, and nobody need be aware of our refactoring!

```haskell
data Temperature =
    Temperature { _location :: String
                , _kelvin  :: Float
                }
    deriving (Show)
makeLenses ''Temperature

celsius :: Lens' Temperature Float
celsius = lens getter setter
  where
    getter = (subtract 273.15) . view kelvin
    setter temp c = set kelvin (c + 273.15) temp
```

By adding the replacement lens we **avoid** breaking any external users of the type! 
Even our `fahrenheit` lens was defined in terms of `celsius`, 
so it will continue to work perfectly.

This is a simple example, but this principle holds for more complex refactorings too.
When adopting this style it's important to avoid exporting the data type 
constructor or field accessors and instead export a "smart constructor" function
and the lenses for each field. 

When you're writing more complex virtual fields it's relatively easy to write
lenses that don't abide by the **lens laws**. In practice, this is usually 
perfectly fine. In many cases it's completely fine to break these laws for the sake of 
pragmatism (especially in application code). In fact the `lens` library itself
exports many law-breaking optics. The important thing is to think about whether the
lenses for your type behave in a way that's _intuitive_ to the caller or not,
and whether it maintains any invariants your type may have.

## Exercises
In [**Optics By Example**](https://leanpub.com/optics-by-example/c/virtual-fields) I include
exercises after each section to help readers sharpen their skills. The book
has answers too, but for this blog post you're on your own. Give these a try!

Consider this data type for the following exercises:

```haskell
data User =
  User { _firstName :: String
       , _lastName :: String
       , _username :: String
       , _email :: String
       } deriving (Show)
makeLenses ''User
```

1. We've decided we're no longer going to have separate usernames and emails; 
   now the email will be used in place of a username. Your task is to delete the 
   `_username` field and write a replacement `username` lens which reads and 
   writes from/to the `_email` field instead. 
   The change should be unnoticed by those importing the module. Assume we haven't
   exported the constructor, or any of our field accessors, only the generated lenses

2. Write a lens for the user's `fullName`. It should append the first and last names when "getting". When "setting" treat everything up to the first space as the first name, and everything following it as the last name.

It should behave something like this:

```haskell
>>> let user = User "John" "Cena" "invisible@example.com"
>>> view fullName user
"John Cena"
>>> set fullName "Doctor of Thuganomics" user
User
    { _firstName = "Doctor"
    , _lastName = "of Thuganomics"
    , _email = "invisible@example.com"
    }
```
