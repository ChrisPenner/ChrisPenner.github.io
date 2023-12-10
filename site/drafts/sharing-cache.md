---
title: 'Save memory using a sharing cache'
author: "Chris Penner"
date: "Aug 6, 2022"
tags: [haskell]
description: "How to save memory in long-lived applications by enforcing sharing."
image: gadts.jpg
---

This post will teach you how, depending on your app, you might be able to add a very simple caching
layer which not only improves performance, but might also drastically reduce the memory residency of your program.

## Case Study

I work on the team building the [Unison Language](https://www.unison-lang.org/).
One unique thing about the language is that programmers interact with the language through the Unison Codebase Manager (a.k.a. `ucm`), which is an interactive shell.
Some users have started to amass larger codebases, and lately we'd been noticing that the memory usage of `ucm` was growing to unacceptable levels.
Loading one specific codebase (which I'll use for testing throughout this article) required 2.73GB to load into memory, and took about 90 seconds to do so, which 
is far larger and slower than we'd like. We have concrete plans to avoid loading _all_ of the code on startup, but those plans will take a little longer to execute so we were looking for a quicker fix.

There are 2 important facets of how Unison stores code that will help aid understanding of how I was able to achieve such good results.
This particular optimization was very well-suited to our problem domain, so it's likely your gains won't be quite so substantial, but it's
definitely worth knowing about how this optimization works, it just might help!

1. Unison code is immutable, and is referenced by a content-based hash.

On startup `ucm` loads the user's codebase into memory so they can interact with it. A codebase is essentially a tree with many branches, each branch may contain many definitions, and also has references to the history of the codebase at that point in the tree. In Unison, once a definition is added to the codebase it is immutable, this is similar to how commits work in git; commits can be built upon, and branches can change which commit they point to, but once a commit is created it cannot be changed and is uniquely identified by its hash. Unison tags definitions with hashes, but each branch state is also hashed and uniquely identifies the set of definitions in that branch and all of its children at a given point in time. 

1. A given codebase tree is likely to refer to a given library many times in different projects.

Unison's dependency management strategy is still in flux, but at the moment each project can pull in any libraries it depends on by simply copying that dependency into its `lib` namespace. Doing so is inexpensive because in effect we simply copy the hash which refers to a given snapshot of the library, we don't need to make copies of any of the underlying code. However, when loading the codebase into memory `ucm` hydrates each and every library reference into a full in-memory representation of that branch which we use for most operations.

There are certainly some larger design changes we can make to avoid loading so much code into memory, and we definitely have plans to continue to improve here, but those changes take a lot of work and time, so ideally there's some quicker fix which still provides a reasonable improvement.

Indeed, after about 80 lines of code, I was able to reduce both the memory residency and start-up load times by a whopping ~95%! 
From 90s -> 4s startup time, and from 2.73GB -> 148MB. All of these gains were realized by tweaking our app to enforce _sharing_ between identical objects in memory.

## What is sharing and why do I want it?

Sharing is a very simple concept at its core: rather than having multiple copies of the same identical object in memory, we should just have one.
Sharing a single instance of an object across all use-cases means we only use enough memory to fit the object once. E.g. if I load a 2MB configuration object from a JSON file in 3 different places, the Haskell runtime won't do anything clever to normalize them, so we end up with `3 * 2MB = 6MB`. The amount of times a given entity might be duplicated in your app depends of course on the sorts of things you're doing, I'll talk about some common cases a bit later on.

If you're working in a language where objects are mutable by default you'll want to think long and hard about whether sharing is sensible or even possible for your use-case, since mutating an object which is shared in many places could have disastrous consequences. Luckily for me, everything in Haskell is immutable by default so there's really no reason not to share identical objects across all places where they are used.

Here are a few of the benefits we can expect when we ensure our data is shared in memory:

* We only pay memory for each unique object we load _once_ across ALL instances rather than once per instance.
* Sharing thunks means each expensive computations can also be shared.
* It's easy to build the sharing cache around the location where the objects are initially constructed, meaning we can often avoid expensive computation or even database calls using the sharing cache.

## Goals

* Reduce memory residency by having a single in-memory representation for each branch, even if that branch exists at many spots in the codebase
* Improve load times by only building that branch from sqlite once
* Ensure that the cache doesn't persist objects in memory past their lifecycle


## Implementation

Okay let's build it!
