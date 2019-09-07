---
title: "Alternative Alternatives"
author: Chris Penner
date: Aug 31, 2019
tags: [haskell]
description: Spelunking for more correct alternatives and monoids
image: hkd-option-parsing/tools.jpg
---


Thought: All monoids which are also alternatives should have (<>) = liftA2 (<>)

It seems that there are two different kinds of applicative for things; the zip-like point-wise pairing, but also the cross-wise product like [].
