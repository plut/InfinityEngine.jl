# IE tools

This is a collection of tools for accessing Infinity Engine database
from the Julia REPL.

Currently the collection comprises the following files:
 - `modtool.jl`: manages WeiDU mods (think of this as
   more-or-less a command-line version of BWS or BGEE modding tool);
 - `infinity.jl`: exposes the Infinity Engine database to Julia in a
   friendly and efficient way (this is more or less equivalent to Infinity
   Explorer);
 - `dialogs.jl`: define dialogs as Julia syntax (this is on its way to
   becoming a substitute for WeiDU).


## `modtool.jl`

 - Download, extract, install mods.
 - Contain a (big) database of known mods and their metadata.
 - Compute (mod-wise) install order.
 - Allow selecting mod components (in a persistent way).
 - Show an ad-hoc text user interface.
 - Somewhat easily register new mods in the database.
 - Tweak installation order by splitting mod in component groups
   (currently this is only applied to SCS).

I wrote this because BGEE modding tool is a bit weird to use (and non
scriptable) while Infinity Project is not portable (I could not run it on
Wine) and thus also probably not scriptable. This also has a few nice
extra features (i.e. automatic mod install order computation).

This should be, if not immediately portable (I don't have a Windows
installation to test it on), at least quite easy to adapt (I took care of
e.g. using `joinpath` instead of hardcoded slashes, etc.). This *does*
use a number of Unix-style executables, most of which should exist on
Windows: `vim`, `git`, `unrar` etc.

Current status: **usable** (I used this to heavily mod a BG2
installation).

### What this tool does not (currently) do
 - Offer a *graphical* (point-and-click) user interface.
 - Check conflicts and requirements.
 - Permute installed mods to reach a given install order.

## `infinity.jl`

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

## `dialogs.jl`

Julia syntax for defining dialogs.
The goal here is to have something simpler to use than Weidu's `.d`
syntax, by *not* writing a parser and using Julia's instead.

The feature wishlist could be summed up as “be more user-friendly than
WeiDU”:
 - define dialogue in a single file (instead of `.d`/`.tra`) by having
   inline strings and using proper translation formats (`.po`).
 - allow a simple way to modify several dialogues at once (à la Weidu's
   `CHAIN`).
 - prevent namespace conflicts without requiring the use of prefixes,
   by solving all the `resref` machinery without user intervention
   (a bit like what WeiDU does with `srref`, but in a more user-friendly
	 way).

Current status: **proof-of-concept**. What code exists is only to try and
reach a usable *syntax* for defining dialogs.

## System requirements
 - julia with a few packages (most of them quite standard or even stdlib:
   Git, HTTP, TOML, JSON, IniFile)
 - file extraction utilities: unrar, 7zr, unzip, tar
 - `vim` text editor (used and scripted as a text interface)

