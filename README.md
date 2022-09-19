[![Documentation|Dev](https://img.shields.io/badge/docs-latest-blue.svg)](https://plut.github.io/InfinityEngine.jl/dev/)
# Infinity Engine editor

Access InfinityEngine data from within a “real” programming language.
This should eventually be able to do what WeiDU does, with the following
advantages:

 - no need to learn several ad-hoc languages: `.tp2` and `.d` can both
   be replaced by standard Julia syntax;
 - WeiDU more or less always assumes that it is run interactively, which
   is quite inconvenient for large installations;
 - easier syntax for defining game objects, e.g. we can currently say
   `Longsword("Sword of Infinity", +5, ...)`
 - resolution of namespace conflicts via modules + automatic generation of
   resource names;
 - speed (all changes could be computed in one single execution of the program:
   no need to do many rewrites of `dialog.tlk` with total
   quadratic complexity...);
 - since everything is written in already-existing languages (code in
   Julia and translations in gettext's `.po` format), it is easy to
   validate contents;
 - easier inclusion of mod metadata;
 - built-in portability (no need to call shell scripts or `.bat` files,
   Julia contains all the needed functions);
 - automatic mod conflict detection.


Current status: **very limited use cases**. This can currently load,
display and (sometimes) edit items and dialogs. It is not yet possible to
define a mod.

**This is currently limited to BGEE, BG2EE and EET games.**
Adding support for other IE games is a (long-term) goal
(only once this is able to run full BGEE mods!).

## Design goals
The design goals include:

### User-friendliness (CLI style)

Prevent the user from needing to learn several ad-hoc languages
(`tp2`, `d`; all of them frankly quite esoteric)
and instead use a single general-purpose language,
quite simple to learn and for which loads of documentation already exist.

The module tries hard to abstract some of the game's ugliest points away
from the user/mod author.
Objects and their properties are represented by plain Julia structs:
```julia
i = item("sw1h01") # Bastard sword
i.min_strength = 14
i.abilities[1].damage = Crushing(2d4+1) # nonsense, but shows syntax
```
This should help in writing efficient (and robust) code without requiring
frequent use of [IESDP documentation](https://gibberlings3.github.io/iesdp).

Moreover, references to game objects (such as `"sw1h01"` above)
are namespaced. This should prevent mod interference without requiring
two-byte author prefixes.
While namespaces should be mostly transparent to mod authors,
they are explained in more detail in the relevant documentation.

Finally, having mods as Julia programs should ideally ease their
development, testing, and validation before release.

### Robustness

This module tries to have an in-depth­view of game structures,
which means that checks will be quite easy to implement
(e.g. missing resources, missing translated strings etc.).
And using a “real” programming language means that at least
syntax is easy to validate.

(on the other hand, this module's initial author had several WeiDU mods
crash on install because of a missing tilde
in some released translation files: **this should never happen**.
And I'm not even counting the number of UTF8-related errors!).

### Mod stack management

This project will be able to manage a whole set of mods,
including which files are replaced by which mods.
This will enable automatic detection of conflicts
(optimistically, it should even be possible to fine-grain
conflict detection, for example when two mods overwrite
the same object property, table entry, or dialog state).

On the other hand, WeiDU mods are stand-alone,
and therefore conflict detection (if any)
must be performed by the mods themselves.

Current status: **project**.
We have some code for managing a WeiDU mod stack
(basically a command-line equivalent of BigWorldSetup etc.);
this unreleased yet, but working.
A significant part of this code could be reused.

### Portability

Julia itself is portable (at least on all platforms able to run IE
games), and code can be kept portable as long as some basic precautions
are taken (e.g. `joinpath` instead of using slashes, not using
assumptions about symlinks, etc.).

In particular, since this tool is currently developed on Unix, at
least some care will be taken w.r.t filenames case-sensitivity.
Ideally this should be able to run mods without any ugly solution such as
`ciopfs` or a separate NTFS partition, which should help with speed.

Also, where some mods require the use of shell scripts or batch files,
Julia contains all the basic shell functions (`mv`, `mkdir` etc),
allowing to write such scripts in a self-supporting and portable way.

### Speed

This is a minor goal, but installing a big stack of WeiDU mods can take
hours, and fine-tuning a single mod also crucially depends on the speed
of installing this mod.

Julia is a quite fast language (it is more-or-less an assembly code generator
after all) whereas WeiDU is a scripting language,
which may also call local shell scripts etc.

Also, since each WeiDU mod is a standalone program, huge files
(namely `dialog.tlk`) are rewritten many times (even for tiny
changes), leading to quadratic complexity. InfinityExplorer is written
in Java and hence quite slow.

Currently, this module can load all the un-modded game data
in almost-negligible time (a few milliseconds on a typical laptop
for a BG1EE installation, assuming Julia is already running).

Some mods which would be particularly interesting to translate to this
language would be big mods such as SCS, for which
both the speed and readability gains would be quite consequent.

## Dialogs

This module defines Julia syntax for IE dialogs.

The goal here is to have something simpler to use (for mod authors)
than Weidu's `.d` syntax,
by *not* writing a parser and using Julia's instead.

The feature list could be summed up as “be more user-friendly than
WeiDU”:
 - define dialogue in a single file (instead of `.d`/`.tra`) by having
   inline strings and using proper translation formats (`.po`),
   for which appropriate editing tools already exist.
 - allow a simple way to modify several dialogues at once (à la Weidu's
   `CHAIN`).
 - prevent namespace conflicts without requiring the use of prefixes,
   by solving all the `resref` machinery without user intervention
   (a bit like what WeiDU does with `strref`, but in a more user-friendly
   way).

Here is a short example of this syntax, namely Imoen's dialog from BG1
prologue. The following code is actually decompiled from the game's data,
and does compile back to the exact same binary files:
```julia
# actor 'imoen' with 10 states:
trigger("  NumberOfTimesTalkedTo(0)\r\n")
say(0 => "I'm surprised that stuffy ol' Gorion let you away...")
# 5 transitions: 
  reply("I'm afraid I cannot chat today, little one....")
  journal("My old friend Imoen pestered me today...")
 
  trigger("  ReactionLT(LastTalkedToBy,NEUTRAL_LOWER)\r\n")
  reply("I am sorry, child, but I am not to tell anyone what I am doing...")
```

The syntax is roughly stabilized (although some details might still vary)
and most simple features are implemented
(extending dialogs, inserting transitions),
but significant work remains
(e.g. quest management or handling of `Strref`s in actions).

The work on translations (using standard `.po` files) is almost complete.
This will make it easier to translate mods using already-existing tools,
and also make the system more robust (e.g. partial or somewhat obsolete
translations will remain usable, and syntax errors in translation files
will **not** cause a mod crash).

## But WeiDU already exists!

And it has certainly been used to write lots of wonderful work.
But these games have already been out for 20 years
and will (hopefully) be out for at least as long,
so it's never too late to try and build something even better than WeiDU.
On the other hand, WeiDU does show both its age
(programming tools advanced quite a lot in 20 years)
and the preoccupations of its author.

Also, a (long-term) goal is to automate at least a part of translation
from WeiDU to this tool (it is very likely possible to do this at least
for `.d`/`.tra` files), making it easier to port a mod from one language
to another.
