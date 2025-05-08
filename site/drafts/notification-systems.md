---
title: "How to build a notification system"
author: Chris Penner
date: May 5, 2025
tags: [programming, haskell]
description: "How to build a medium scale notification system"
image: flux-monoid/flux.jpg
---

I've encountered a lot of fresh challenges while building out the Unison language 
and its ecosystem of tools. It turns out building out a web-app with feature-parity to some 
sort of Github and npm hybrid involves thinking through a lot of fundamental systems.

* User creation and management
* An Identity and Authentication server system for OAuth'ing between apps
* An extensible Authorization system which supports roles and both resource and subject hierarchies.
* A code storage and syncing system for Unison's built-in version control
* A type-aware global code search


