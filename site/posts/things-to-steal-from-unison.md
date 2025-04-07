---
title: "3 things other languages should steal from Unison"
author: Chris Penner
date: Apr 24, 2025
tags: [programming, haskell]
description: "Some things other languages should steal from Unison"
image: steal.jpg
---

New languages are coming out all the time, some experimental, some industrial, others are purpose built for a specific domain.
No single language has the people-power or scope to try every cool new feature, so
a critical step in designing a new language is to observe how experimental features 
have borne themselves out in practice.

As the saying goes, good \[language designers\] copy, great \[language designers\] steal.

If you've heard anything about the Unison Language it's not a surprise to you that it innovates in many areas.
Unison very much tries to reinvent Human-Compiler interactions for the 21st century, and in that pursuit 
has spawned fully integrated ecosystem between the compiler, codebase-manager, language server, version control and package manager.

While some of these features are still too new to have proven their worth (but we have our fingers crossed);
there are aspects that I think new languages should certainly consider as part of their designs.

## A Fully Interactive and Incremental Compiler

With the modern era of language servers and programming assistants, developers
greatly benefit from instant feedback on their work. 
With traditional batch compilers it's all too tempting to go for a coffee, or a 
walk, or a YouTube binge every time you kick off a big build. The context-switching induced by switching tasks while compiling
wastes developer time by paging things in and out of their working memory, not to mention: _it just feels bad_. 
After the build finishes, the developer is left with a giant wall of text, sentenced
to dig through a large list of compiler errors trying to find some 
root-cause error in the file they're working on.

Unison has a fully interactive compilation experience. The language-server is typechecking your scratch-file on every keystroke
providing error feedback right in your editor, and offering helpful information 
via hover-hints which use your codebase and typechecking info to help you orient yourself. 
It can even partially typecheck the file to suggest which types or operators you may want to fill into a given slot.

Once you're happy with a chunk of code, you can check it in to the codebase and it won't be compiled again unless
you want to change it, or an update is automatically propagated into it from a downstream change.

While most languages won't adopt Unison's scratch-file and codebase model;
having an interactive compiler with good support for 
caching of already-compiled-assets is a huge boon to productivity in any language.

On the topic of the language server, Unison's language server is built directly into the compiler. 
This ensures we avoid the awkward disagreements between the LSP and compiler that sometimes happen in other languages.
It can also help to avoid duplicate work, many languages are running the compiler independently and in their LSP at the same time 
without sharing any of the work between them, causing redundant work and a waste of precious resources.

## Codebase API

It's the compiler's job to understand your code intimately. It knows exactly how every
definition is linked together, even if you don't!
In many languages it can be frustrating to know that this information exists deep within the compiler,
but not having any access to it yourself!

Unison stores all your code as structured data within your codebase and exposes
the ability for you to ask it useful questions about your code, exposing that precious understanding
to you as a developer.

Unison allows searching by type, finding the dependencies of a definition, or 
inverting that relationship to finding all definitions which depend on a definition. 

Via the UCM CLI you can use utilities like `text.find` to search only string constants, or `find` to search only definition names.

Some codebase data is provided via an API which is exposed from the interactive UCM compiler, allowing developers to write tooling to customize their workflow.
For example, check out this [VS Code plugin](https://marketplace.visualstudio.com/items?itemName=TomSherman.unison-ui) someone wrote to view codebase definitions in the sidebar. In other languages you'd typically need to write a scrappy Regex or re-compile the code in a subprocess in order to achieve something similar.

It doesn't have to be an API, it could be a parquet file or a SQLite database or any number of things, the important part is that a language exposes its one-true-source of information about the codebase in some structured format for third-party tools to build upon.

## Smart docs

It doesn't matter how great your language's package ecosystem is if nobody can 
figure out how to use it! Documentation is critical for helping end users understand
and use functionality in your language, but it has a fatal flaw: documentation isn't
compiled and falls out of date with the code.

In Unison, docs are a data-type within the language itself.
This means that docs can be generated dynamically by _running Unison code_! We've leveraged this ability
to enable embedding typechecked runnable code examples into your docs.
These examples are compiled alongside the rest of your program, so they're **guaranteed to be kept up to date**,
and the outputs from your example code is run and updated whenever the source definitions change.

You can also write code which _generates_ documentation based on your real application code.
For example, you could write code which crawls your web-server's implementation and collects all 
the routes and parameters the server defines and displays them nicely as documentation.

Unison goes one step further here by providing special support for the documentation format
on Unison Share, ensuring any definitions mentioned in docs and code examples are hyper-linked
to make for a seamless package-browsing experience.

As an example of how far this can go, check out [this awesome project](https://share.unison-lang.org/@alvaroc1/circuit2/code/main/latest/terms/README) by community contributor Alvaro which 
generates mermaid graphs in the docs representing the behaviour of simulations.
The graphs are generated from the same underlying library code so they won't go out of date.

## Get stealing

This subset of topics doesn't touch on Unison's ability system, continuation capturing, 
or code serialization so I'll probably need at least a part 2!
