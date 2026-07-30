My experience writing production Unison

* Updates:
  * Love having UCM find all the changes I need to make and put them all in one place.
* Love having a smart MCP
* Love the test caching
* Unison is capable of legitimately unique programming patterns in robust and reliable ways:
  * Memoized folds
  * Search-select
  * Card Select
  * Scheduled
  * Tables
  * {Remote} is fantastic.
* Durable data and immutability are MUSTS for distributed computing.
* The ability to run code on whichever system you want (e.g. evalBeside) is ergonomic and dope
* Being able to trigger a signal across node boundaries is also dope.
* I like being able to jump into any project on my system quickly.
    * Pulling code across project boundaries could be nicer
* Local interpreters for Remote and Cloud are great.
* Abilities are one of the better effects systems I've used, 
  * I do miss Applicatives, e.g. Forms API
  * I'm able to do things like co-operative pre-emption, e.g. fibers on top of Remote.
  * debugging ability
* Having a cloud built-in to the language is dope
* Streams are fantastic and every language should have them.
* Lots of people are mistaken on the primary benefits of hashing source code and values.
  * It's less about security, more about distributed computing, having identity for functions and values.
* FZF is a fantastic tool which more CLIs should integrate.
* {Route} is bad IMO
* Signals, Refs, are good.
* Learning curve was rough, for about 2 days.
* Argument order
* Share is a fantastic documentation tool
    * Type, definition, and project search
    * Smart diffs
    * Clickable hyper-links
* Share being source control, package manager, and documentation tool all in one is a big win 
* Interactive compiler is a huge win
* Deployment as a function is dope
* I really don't miss typeclasses as much as I thought,
  * Though there are still cases where they'd be nice.
* TDNR gets a bit weird and for me is largely an anti-feature, I'd rather be able to write some sort of qualified-scope thingy, like `use` but slightly different.
* Lib upgrades work pretty well, multiple libs can co-exist
  * however in the odd case where there's a conflict it can get hairy., still some unfinished workflows
* Codebase search in a CLI is SO NICE
* VCS is still unfinished, and is unusable for a lot of VCS things
* structural types are largely unused/non-feature, and in some cases are foot-guns (e.g. True/False)



Cons:
* Migrating tables is a pain
* Some changes during an update result in hashes being displayed, or types being changed without being able to pull dependents into the file
* Merges can be complex.
* Sync is slow
* 

