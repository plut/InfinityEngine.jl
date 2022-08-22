# Infinity Engine editor

Access InfinityEngine data from within a “real” programming language.
This will eventually be able to do what WeiDU does, with the following
advantages:

 - no need to learn several esoteric languages: `.tp2` and `.d` can both
   be replaced by standard Julia syntax;
 - WeiDU more or less always assumes that it is run interactively, which
   is quite inconvenient for large installations;
 - easier syntax for defining game objects, e.g. we can currently say
    Longsword("Sword of Infinity", +5, ...)
 - resolution of namespace conflicts via modules + automatic generation of
   resource names;
 - speed (all changes could be computed in one single execution of the program:
   no need to do many rewrites of `dialog.tlk` with total
   quadratic complexity...);
 - allows validation of mod content (future: translations in `.po` format?);
 - allows easier inclusion of mod metadata;
 - built-in portability (no need to call shell scripts or `.bat` files,
   Julia contains all the needed functions).


Current status: **very limited use cases**. This can currently load,
display and (sometimes) edit items and dialogs. It is not yet possible to
define a mod.

**This is currently limited to BGEE, BG2EE and EET games.**
Adding support for other IE games is a (long-term) goal
(only once this is able to run full BGEE mods!).

## Design goals
The design goals include:

### Robustness

This module tries to have an in-depth­view of game structures,
which means that checks will be quite easy to implement
(e.g. missing resources, missing translated strings etc.).
And using a “real” programming language means that at least
syntax is easy to validate.

(on the other hand, I had several WeiDU mods crash on install
because of a missing tilde in some released translation files:
**this should not happen**. And I'm not even counting the number of
UTF8-related errors!).

### User-friendliness (CLI style)

Prevent the user from needing to learn several esoteric languages (`tp2`,
`d`) and instead use a general-purpose language.

The module tries hard to abstract some of the game's ugliest points away
from the user: namely, it offers namespaces for game objects, to prevent
mod interference without requiring the use of two-byte prefixes;
and object properties are referred by name using standard Julia structs
(e.g. `item.type`).
This should help in writing efficient (and robust) code without requiring
frequent use of [IESDP documentation](https://gibberlings3.github.io/iesdp).

Finally, having mods as Julia programs should ideally ease their
development, testing, and validation before release.

### Portability

Julia itself is portable (at least on all platforms able to run IE
games), and code can be kept portable as long as some basic precautions
are taken (e.g. `joinpath` instead of using slashes, etc.).

In particular, since this tool is developed on Unix, at
least some care will be taken w.r.t filenames case-sensitivity.
Ideally this should be able to run without any ugly solution such as
`ciopfs` or a separate NTFS partition, which should help with speed.

Also, where some mods require the use of shell scripts or batch files,
Julia contains all the basic shell functions (`mv`, `mkdir` etc),
allowing to write such scripts in a portable way.

### Speed

Julia is a quite fast language (it is more-or-less a C code generator
after all) whereas WeiDU is a scripting language,
which may also call local shell scripts etc.

Also, since each WeiDU mod is a standalone program, huge files
(namely `dialog.tlk`) are rewritten many times (even for tiny
changes), leading to quadratic complexity. InfinityExplorer is written
in Java and hence quite slow.

Currently, this module can load all the un-modded game data
in almost-negligible time (about 2 milliseconds on a typical laptop
for a BG1EE installation, assuming Julia is already running).

## Dialogs

This module defines Julia syntax for IE dialogs.

The goal here is to have something simpler to use than Weidu's `.d`
syntax, by *not* writing a parser and using Julia's instead.

The feature wishlist could be summed up as “be more user-friendly than
WeiDU”:
 - define dialogue in a single file (instead of `.d`/`.tra`) by having
   inline strings and using proper translation formats (`.po`),
   for which appropriate editing tools already exist.
 - allow a simple way to modify several dialogues at once (à la Weidu's
   `CHAIN`).
 - prevent namespace conflicts without requiring the use of prefixes,
   by solving all the `resref` machinery without user intervention
   (a bit like what WeiDU does with `srref`, but in a more user-friendly
   way).

Here is a short example of this syntax, namely Imoen's dialog from BG1
prologue:
```text/julia
# actor 'imoen' with 10 states:
trigger("  NumberOfTimesTalkedTo(0)\r\n")
say(0 => "I'm surprised that stuffy ol' Gorion let you away...")
# 5 transitions: 
  reply("I'm afraid I cannot chat today, little one....")
  journal("My old friend Imoen pestered me today...")
 
  trigger("  ReactionLT(LastTalkedToBy,NEUTRAL_LOWER)\r\n")
  reply("I am sorry, child, but I am not to tell anyone what I am doing...")
```

Current status: **proof-of-concept**. What code exists is only to try and
reach a usable *syntax* for defining dialogs.

## But WeiDU already exists!

And it has certainly been used to write lots of wonderful work. But these
games have already been out for 20 years and will (hopefully) be out for
at least as long, so it's never too late to try and build something even
better than WeiDU.

Also, a (long-term) goal is to automate at least a part of translation
from WeiDU to this tool (it is very likely possible to do this at least
for `.d` files), making it easy to port a mod from one language to
another.
