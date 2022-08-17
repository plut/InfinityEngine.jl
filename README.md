# Infinity Engine editor

Access InfinityEngine data from within a “real” programming language.
Expected advantages w.r.t WeiDU:

 - no need to learn several esoteric languages: `.tp2` and `.d` can both
   be replaced by standard Julia syntax;
 - WeiDU more or less always assumes that it is run interactively, which
   is quite inconvenient for large installations;
 - we could have an easier syntax for defining e.g. items:
    Item("Sword of Infinity", etc.)
 - resolution of namespace conflicts via modules + automatic generation of
   resource names;
 - speed (besides this being faster than OCaml, all changes could be
   computed in one single execution of the program, e.g. no need to do
   many rewrites of `dialog.tlk` with total quadratic complexity...);
 - allows validation of mod content (e.g. translations in .po format?);
 - allows easier inclusion of mod metadata;
 - built-in portability (no need to call shell scripts or .bat files,
   Julia contains all the needed functions).


Current status: **very limited use cases**. This can currently load and
display strings, items and dialogues (but not yet modify them).
However, this indeed seems very much faster than NearInfinity for those
use cases.

## Design goals
The design goals include:
 - speed: WeiDU is a scripting language, which may also call local shell
   scripts etc. Also, since each mod is a standalone program, huge files
   (namely `dialog.tlk`) are rewritten many times (even for tiny
   changes), leading to quadratic complexity. InfinityExplorer is written
   in Java and hence quite slow. On the other hand, mods written as Julia
   scripts using these tools could run as standalone Julia programs and
   thus be quite fast.
 - portability: Julia is portable as long as some basic precautions are
   taken. (In particular, since these tools are developed on Unix, at
   least some care will be taken w.r.t filenames case-sensitivity.
   Ideally this should be able to run without any ugly solution such as
   `ciopfs` or a separate NTFS partition).
 - user-friendliness (CLI style): prevent the user from needing to learn
   several esoteric languages and instead use a general-purpose language.
   Also, having mods as Julia programs should ideally ease their
   development, testing, and validation before release.
   (on the other hand, I had *several* WeiDU mods crash on install
   because of a missing tilde in some released translation files:
   **this should not happen**. And I'm not even counting the number of
   UTF8-related errors!).
 - robustness: see above.



## Dialogs

This module also contains Julia syntax for defining dialogs.

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

Current status: **proof-of-concept**. What code exists is only to try and
reach a usable *syntax* for defining dialogs.
