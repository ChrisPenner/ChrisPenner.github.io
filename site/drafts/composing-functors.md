---
title: "Data types as compositions of functors"
author: Chris Penner
date: Apr 8, 2019
tags: [haskell]
description: Data types can be expressed as compositions of various functors
image: postman/mail.jpg
---


Classes od data

# Empty

- `data Void`

# Nullary types

- `()`

# Unary Types ??

- `Identity a`

# Sums: 

- `Either b a = Left b | Right a"`
- `Just a | Nothing`
- `True | False`

# Products: 

- `(a, b)`

# Sums, Products, Unaries

- `Just a | Nothing`

# Add Fix
# Add Exponentials (e.g. functions)
