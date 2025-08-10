---
title: "Building Industrial Strength Software without Unit Tests"
author: Chris Penner
date: Jun 2, 2025
tags: [programming, haskell, unison, testing]
description: "Unit tests aren't the only way."
image: transcript-tests.jpg
---

I don't know about you, but testing isn't my favourite part of software development.

It's usually the last thing standing between me and shipping a shiny new feature, and writing tests is often
an annoying process with a lot of boilerplate and fighting against your system to get your app 
into a good start starting for the test or mocking out whichever services your app depends on.

Much ink has been spilled about how to organize your code in order to make this easier, but the fact that so many blog posts and frameworks 
exist for this express purpose suggests to me that we as a community of software developers haven't quite solved this issue yet.

Keep reading to see how I've solved this problem for myself by simply avoiding unit testing altogether.

## An alternative testing method

When I first started at Unison Computing I was submitting my first feature when I 
learned there were precious few unit tests. I found it rather surprising 
for a codebase for a compiler for a programming language! How do you prevent regressions without unit tests?

The answer is what the Unison team has dubbed **transcript tests**. 
These are a variation on the concept of *golden-file tests*. 

A _Unison transcript_ is a markdown file which explains in standard 
what behaviour it is going to test, then intersperses code-blocks
which outline the steps involved in testing that feature using a mix of Unison code and UCM commands (UCM is Unison's CLI 
tool). After that comes the magic trick; UCM itself can understand and run these transcript files
directly and record the results of each block.

When running a transcript file with the `ucm transcript` command UCM produces a deterministic output 
file containing the result of processing each code block. 
Unless the behaviour of UCM has changed since the last time it was run the resulting file will always be the same.

Each block in the markdown file is either a command, which is sent to the UCM shell tool, 
or it represents an update to a file on the (virtual) file-system, in which case it will be
typechecked against the state of the codebase.

Here's a quick example of a transcript for testing UCM's view command so you can get a feel for it.

````
# Testing the `view` command

First, let's write a simple definition to view:

``` unison
isZero = cases
  0 -> true
  _ -> false
```

Now we add the definition to the codebase, and view it.

``` ucm
scratch/main> update
scratch/main> view isZero
```
````

We run this transcript file with `ucm transcript my-transcript.md` which produces the `my-transcript.output.md` 
file. 


Notice how compiler output is added inline, ignore the hashed names, It's because I'm skipping the step which adds names for Unison's builtins.

````
# Testing the `view` command

First, let's write a simple definition to view:

``` unison
isZero = cases
  0 -> true
  _ -> false
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      isZero : ##Nat -> ##Boolean
```

Now we add the definition to the codebase, and view it.

``` ucm
scratch/main> update

  Done.

scratch/main> view isZero

  isZero : ##Nat -> ##Boolean
  isZero = cases
    0 -> true
    _ -> false
```
````

Feel free to browse through the [collection of transcripts](https://github.com/unisonweb/unison/tree/86bf4b2/unison-src/transcripts-using-base) we test in CI to keep UCM working as expected.

## Testing in CI

Running transcript tests in CI is pretty trivial; we discover all markdown files within our transcript directory and run them all.
After the outputs have been written we can use `git diff --exit-code` which will then fail with a non-zero code if anything of the outputs have changed from what was committed.
Conveniently, git will also report _exactly_ what changed, and what the old output was.

This failure method allows the developer to know exactly which file has unexpected behaviour so they can easily re-run that file or recreate the state 
in their own codebase if they desire.

## Transcript tests in other domains

I liked the transcript tests in UCM so much that when I was tasked with building out the Unison Share webapp
I decided to use transcript-style testing for that too. 
Fast forward a few years and Unison Share is now a fully-featured package repository and code collaboration platform running in production
without a **single** unit test.

If you're interested in how I've adapted transcript tests to work well for a webapp, I'll leave a few notes at the end of the post.

## Benefits of transcript tests

Here's a shortlist of benefits I've found working with transcript tests over alternatives like unit tests.

**You write a transcript using the same syntax as you'd interact with UCM itself.**

This allows all your users to codify any buggy behaviour they've encountered into a deterministic transcript. 
Knowing exactly how to reproduce the behaviour your users are seeing is a huge boon, and having a single standardized format for 
accepting bug reports helps reduce a lot of the mental work that usually goes into reproducing bug reports from a variety of sources.
This also means that the bug report itself can go directly into the test suite if we so desire.

**All tests are written against the tool's _external_ interface.**

The tests use the same interface that the users of your software will employ, 
which means that **internal refactors won't ever break tests** unless there's
a change in behaviour that's externally observable. 

This has been a huge benefit for me personally. I'd often find myself hesitant to re-work code because I knew that 
at the end I'd be rewriting thousands of lines of tests. 
If you always have to rewrite your tests at the same time you've rewritten your code, 
how do you have any confidence that the tests still work as intended?

**Updating tests is trivial**

In the common case where transcripts are mismatched because some help message was altered, or perhaps 
the behaviour has changed but the change is intended, you don't need to rewrite any complex assertions, or mock out any new dependencies. 
You can simply look at the new output, and if it's reasonable you commit the changed transcript output files.

It can't be understated how convenient this is when making sweeping changes; e.g. making changes to Unison's pretty printer.
We don't need to manually update test-cases, we just run the transcripts locally and commit the output if it all looks good!

**Transcript changes appear in PR reviews**

Since all transcript outputs are committed, any change in behaviour will show up in the PR diff in an easy-to-read form.
This allows reviewers to trivially see the old and new behaviour for each relevant feature.

**Transcript tests are documentation**

Each transcript shows how a feature is intended to be used by end-users.

**Transcripts as a collaboration tool**

When I'm implementing new features in Unison Share I need to communicate the shape of a JSON API with our Frontend designer Simon.
Typically I'll just write a transcript test which exercises all possible variants of the new feature, then I can just point at the transcript output as the interface for those APIs. 

It's beneficial for both of us since I don't need to keep an example up-to-date for him, and he knows that the output is actually 
accurate since it's generated from an execution of the service itself.

## Transcript testing for Webapps

I've adapted transcript testing a bit for the Unison Share webapp.
I run the standard Share executable 
locally with its dependencies mocked out via docker-compose. I've got a SQL file which 
resets the database with a known set of test fixtures, then use a zsh script 
to reset my application state in between running each transcript. 

Each transcript file is just a zsh script that interacts with the running server using 
a few bash functions which wrap curl commands, but save the output to json files, which serve as the transcript output.

I've also got helpers for capturing specific fields from an API call into local variables which I can then interpolate into future queries, this is handy if you need to, for example, create a project then switch it from private to public, then fetch that project via API.

Here's a small snippet from one of my transcripts for testing Unison Share's project APIs:

```sh
#!/usr/bin/env zsh

# Fail the transcript if any command fails
set -e

# Load utility functions and variables for user credentials
source "../../transcript_helpers.sh"

# Run a UCM transcript to upload some code to load in projects.
transcript_ucm transcript prelude.md

# I should be able to see the fixture project as an unauthenticated user.
fetch "$unauthenticated_user" GET project-get-simple '/users/test/projects/publictestproject'

# I should be able to create a new project as an authenticated user.
fetch "$transcripts_user" POST project-create '/users/transcripts/projects/containers' '{
    "summary": "This is my project",
    "visibility": "private",
    "tags": []
}'

fetch "$transcripts_user" GET project-list '/users/transcripts/projects'
```

You can see the output files generated by the full transcript [in this directory](https://github.com/unisoncomputing/share-api/tree/f475f49/transcripts/share-apis/projects-flow).

## Requirements of a good transcript testing tool

After working with two different transcript testing tools across two different apps 
I've got a few criteria for what makes a good transcript testing tool, if you're
thinking of adding transcript tests to your app consider the following:

**Transcripts should be deterministic**

This is critical. Transcripts are only useful if they produce the same result on every run, on every operating system, at every time of day.

You may need to make a few changes in your app to adapt or remove randomness, at least when in the context of a transcript test.

In Share there were a lot of timestamps, random IDs, and JWTs (which contain a timestamp).
The actual values of these weren't important for the tests themselves, so I solved the issue by piping the curl output through a `sed` script
before writing to disk. The script matches timestamps, UUIDs, and JWTs and replaces them with placeholders like `<TIMESTAMP>`, `<UUID>`, and `<JWT>` accordingly.

A special mode in your app for transcript testing which avoids randomness can be useful, but use custom modes sparingly lest your app's behaviour differ too much during transcripts and you can't test the real thing.

I also make sure that the data returned by APIs is always sorted by something other than randomized IDs, it's a small price to pay, and reduces randomness and heisenbugs in the app as a helpful byproduct.

**Transcripts should be isolated**

Each individual transcript should be run in its own pristine environment. Databases should be reset to known state, if the file-system is used, it should be cleared or even better, a virtual file-system should be used.

**Transcripts should be self-contained**

Everything that pertains to a given test-case's state or configuration should be 
evident from within the transcript file itself. I've found that changes in behaviour
from the file's location or name can just end up being confusing.

## Difficulties working with Transcripts

**Transcripts often require custom tooling**

In UCM's case the transcript tooling has evolved slowly over many years, it has it's own parser, and you can even test UCM's API server by using special code blocks for that.

Share has a variety of `zsh` utility scripts which provide helpers for fetching endpoints using curl, and filtering output to capture data for future calls.
It also has a few tools for making database calls and assertions.

Don't shy away from investing a bit of time into making transcript testing sustainable and pleasant, it will pay dividends down the road.


**Intensive Setup**

As opposed to unit tests which are generally pretty lightweight; transcript tests are full integration tests,
and require setting up data, and sometimes executing entire flows so that we can get the system into a good state for testing each feature.

You can mitigate the setup time by testing multiple features with each transcript.

I haven't personally found transcript tests to take too much time in CI, 
largely because I think transcript testing tends to produce fewer tests, but of higher
value than unit testing. I've seen many unit test suites bogged down by particular
unit tests which generate hundreds of test cases that aren't actually providing real value.
Also, any setup/teardown is going to be more costly on thousands of unit-tests as compared to dozens or hundreds of transcript tests.

**Service Mocking**

Since transcript tests run against the system-under-test's external interface, 
you won't have traditional mocking/stubbing frameworks available to you. Instead, you'll mock out 
the system's dependencies by specifying custom services using environment variables, or wiring things up in docker-compose.

Most systems have a setup for local development anyways, so integrating transcript tests against it
has the added benefit that they'll ensure your local development setup is tested in CI, is consistent for all members of your team, and continues to work as expected.

## In Summary

Hopefully this post has helped you to consider your relationship with unit tests and perhaps think about whether other testing techniques may work better for your app.

Transcript tests surely aren't ideal for **all** possible apps or teams, but my last few years at Unison have proven to me that tests can be 
more helpful, efficient, and readable than I'd previously thought possible.

Let me know how it works out for you!
