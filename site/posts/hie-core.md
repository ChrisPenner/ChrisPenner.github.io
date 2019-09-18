---
title: "Haskell IDE Support (hie-core lsp Sept. 2019)"
author: Chris Penner
date: Sep 7, 2019
tags: [haskell]
description: Configuring editor support for Haskell (hie-core 2019)
---

**EDIT**: This project has been renamed to `ghcide` now; you can find it [here](https://github.com/digital-asset/ghcide)!

Here's a super quick guide on adding hie-core to your workflow!

Disclaimer; this post depends on the state of the world as of Saturday Morning, Sept. 7th 2019; it's likely changed since then. I'm not a maintainer of any of these libraries, and this is a complicated and confusing process. There's a good chance this won't work for you, but I'm afraid I can't support every possible set up. Use it as a guide-post, but you'll probably need to fix a few problems yourself. Feel free to let me know if things are broken, but I make no guarantees that I can help, sorry! Good luck!

This is a guide for using it with stack projects, or at least using the stack tool. If your project isn't a stack project, you can probably just run `stack init` first.

`hie-core` currently requires a whole suite of tools to run, including `hie-bios`, `hie-core`, and `haskell-lsp`. Each of these need to be installed against the proper GHC version and LTS that you'll be using in your project. This is a bit annoying of course, but the end result is worth it.

We need separate binaries for every GHC version, so to avoid getting them all confused, we'll install everything in LTS specific sandboxes!

* First navigate to the project you want to run `hie-core` with
* Now `stack update`; sometimes stack doesn't keep your hackage index up-to-date and most of the packages we'll be using are pretty new.
* `stack build hie-bios hie-core haskell-lsp --copy-compiler-tool`
    * We need these three executables installed, using `stack build` doesn't install them globally (which is what we want to avoid conflicts), but `--copy-compiler-tool` allows us to share binaries with other projects of the same LTS.
    * This will probably FAIL the first time you run it, stack will suggest that you add extra-deps to your `stack.yaml`; go ahead and do that and try again. Repeat this process until success!

If you've got all those running, time to go for a walk, or make a cup of tea. It'll take a while.

If you're using an LTS OLDER than `14.1` then `haskell-lsp` will probably be too old to work with `hie-core`; you can **try** to fix it by adding the following to your extra-deps:

```yaml
extra-deps:
- haskell-lsp-0.15.0.0
- haskell-lsp-types-0.15.0.0
```

If that doesn't work, sorry, I really have no idea :'(

Okay, so now we've got all the tools installed we can start configuring the editor. 
I can't tell you how to install it for every possible editor, but the key parts to know is that it's a **language server**, so search for integrations for your editor that handle that protocol. Usually "$MyEditorName lsp" is a good google search. Once you find a plugin you need to configure it. Typically there's a spot in the settings to associate file-types with the language server binary. Punch in the Haskell filetype or extensions accordingly, the lsp binary is `stack exec hie-core -- --lsp`; this'll use the `hie-core` you install specifically for this LTS, and will add the other dependencies to the path properly. You'll likely need to specify the binary and arguments separately, see the following vim setup for an example.

## Vim Setup

Here's my setup for using `hie-core` with [Neovim](https://neovim.io/) using the amazing [Coc plugin](https://github.com/neoclide/coc.nvim). Note that you'll need to install Neovim from latest HEAD to get proper pop-up support, if you're on a Mac you can do that with `brew unlink neovim; brew install --HEAD neovim`. 

Follow the instructions in the Coc README for installing that however you like; then run `:CocConfig` inside neovim to open up the config file.

Here's my current config:

```json
{
"languageserver": {
  "haskell": {
    "command": "stack",
    "args": ["exec", "hie-core", "--", "--lsp"],
    "rootPatterns": [
      ".stack.yaml",
      "cabal.config",
      "package.yaml"
    ],
    "filetypes": [
      "hs",
      "lhs",
      "haskell"
    ],
    "initializationOptions": {
      "languageServerHaskell": {
      }
    }
  }
}
}
```

Also make sure to read the [Sample Vim Configuration](https://github.com/neoclide/coc.nvim#example-vim-configuration) for Coc to set up bindings and such.

After you've done all that, I **hope** it's working for you, if not, something crazy has probably changed and you're probably on your own. Good luck!

PS; I have a little bash script I use for installing this in every new project in case you want to see how terrible I am at writing BASH. It includes a helper which auto-adds all the necessary extra-deps for you: [My crappy bash script](https://github.com/ChrisPenner/dotfiles/blob/master/bin/hie-init)

You'll probably need to run the script more than once as it attempts to add all the needed extra-deps. Hopefully this'll get better as these tools get added to stackage.

