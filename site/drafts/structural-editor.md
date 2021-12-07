---
title: 'How to build a structural editor'
author: "Chris Penner"
date: "Nov 20, 2021"
tags: [haskell]
description: "Design considerations and abstractions useful for building a structural editor."
image: gadts.jpg
---


Let's talk about **structural editors**. Apparently I like to start my blog posts like I start my wedding toasts, so here's how [Wikipedia](https://en.wikipedia.org/wiki/Structure_editor) defines a Structure Editor:

> A structure editor, also structured editor or projectional editor, is any document editor that is cognizant of the document's underlying structure. Structure editors can be used to edit hierarchical or marked up text, computer programs, diagrams, chemical formulas, and any other type of content with clear and well-defined structure. In contrast, a text editor is any document editor used for editing plain text files.

In short, it's a _smart editor_ for a particular data format. There are a few reasons that you may prefer a structural editor over a text editor,
usually they provide _semantic_ operations over the data. For example many IDE's incorporate structural editing in the form of refactoring tools, auto-imports, "extract function" helpers, etc. Semantic editors also usually have the property that the document will remain syntactically and semantically valid throughout the course of editing.

In this post I'll be talking about [Jet](https://github.com/ChrisPenner/jet#readme), which is a structural editor for the **JSON** data format.

**Jet** provides a few semantic operations:

* Structured navigation: move around by selecting nodes of the document.
* Copying/Cutting/Pasting JSON subtrees.
* Ability to transpose elements to rearrange arrays.

**Jet** preserves a valid JSON document at all times.

Before I wrote Jet I had always been interested in the idea of structural editing. It seemed like a powerful concept, but I was aware of very few actual implementations of the idea. Maybe they were tricky to implement?

There are dozens of great resources online for writing text-editors, but when I looked around for structural editing resources I didn't have much luck.
This article aims to be what I wish I had in my initial research.

We'll investigate the _challenges_ of structural editors, and some _abstractions_ and _data models_ which are useful for thinking about and implementing an editor.

## Challenges

Text editors need to be able to view and edit big files efficiently, luckily, text is easy to split up and view in chunks, usually text editors only need to display about fifty lines at a time. Using clever tools like the [Rope data structure](https://en.wikipedia.org/wiki/Rope_(data_structure) allow editors to efficiently slice-and-dice their text to only actively manage text that's visible on screen at the current moment. Text is structurally very simple, it's just a long string of characters, so it's easy to slice up like this.

In contrast, most structural editors operate over some form of _self recursive_ data structure. Think of things like HTML, JSON, or the AST of your favourite programming language, each of these structures is comprised of _nodes_, each of which contains some data and/or some child nodes of the same type.

These self-recursive structures aren't as easily sliced and diced. We still need to display a string of characters to the screen, but internally we need to be aware of the mapping between that text and the structure of the document. If the user scrolls to line 150 of my JSON document, which node or nodes of the JSON document do I actually need to render?

This is the first challenge: **Rendering**. How do I correctly render a structure to the screen and keep track of which parts of that structure should be visible at any given time. Editors generally need to update what's on screen after every key-press, so we need to be able to do this very efficiently!

Next up is the challenge of **Editing**. When editing text I can either update bytes in-place, or split the sections before and after the edit and join everything back up afterwards. With structured data we need to edit _nodes_ instead. This means we need to keep track of _which_ node the user has selected, and we should be able to update that node within its structured context very efficiently. These nodes are embedded deep within a complex context, each node generally has data of its own, parents, and child nodes too.
Updating nodes within a deeply nested structure in a language with immutable data structures is generally very expensive both in terms of memory and cpu. If I change a single character in a leaf node of my JSON document, Haskell has to create new values for every node in the spine from that edit to the root of the tree.


## Abstractions
