# Home

This package provides an interface to Bioware's Infinity Engine games
databases.

!!! warning "Support"
    Currently, only BG(2)EE games (including EET) are supported.
    Supporting other games is a long-term goal.

## Running the module

The central access point for all information related to a particular
game installation is the `Game` data structure.
Most of the functions of this module take a `Game` value as their first
argument.
For convenience, this argument can be omitted;
in this case, the module will use its own global `Game` structure instead.

The `Game` structure is initialized from the directory
containing the `"chitin.key"` file.

```julia
using InfinityEngine
InfinityEngine.init!("/home/CHARNAME/baldursgate")
InfinityEngine.game()
```
```@docs
InfinityEngine.Game
InfinityEngine.init!
commit
```

### Game information status

All information related to a particular installation
can be retrieved from the corresponding `Game` object.
However, this is only saved to the filesystem
when the [`commit`](@ref) function is called.
This call saves game resources to the `override` directory,
as well as extra persistent information in the state file
`GAMEDIR/state.toml`.

Such information contains:
 - the mapping between filesystem resource names (8 bytes max) and
   Julia-side symbolic resource names (namespaced, arbitrary strings);
 - TODO translation information for game strings;

## Accessing game resources

The `Game` structure works as an index for all game information,
including game resources (items, creatures, scripts...)
and strings.

Resources of a given type are accessed via either selector functions
(e.g. `item("sw1h01")` is a bastard sword)
or iterators (e.g. `for itm in items()...`).
Iterator functions generally are the plural form of selectors.

The following resource types are currently implemented;
each one of them is described in its own documentation page:
 - [`Item`](@ref): game items;
 - [`Actor`](@ref): game dialogs;

### Editing resources

Game objects are handled as ordinary Julia structures:
```julia
albruin = item("sw1h34") # long sword Albruin
albruin.min_strength = 15 # silently marks the item for commit
```
The assignment operators are silently overloaded so that they mark
all modified game objects for commit to the `override` directory.

Dialogs have their own syntax for edition;
see the [`say`](@ref), [`reply`](@ref) etc. functions.

## Resource references and namespaces

From the user point of view, resources are always referred to by strings.
The module implements resource namespaces to prevent interference
between different authors.
Internally, each resource string is prefixed by a namespace,
in the form `"namespace/resource"`.
The current namespace is set by the `namespace()` function.
```@docs
namespace
```
The empty namespace `""` is reserved for resources handled
outside of this module (e.g. original game resources).
At module initialization, the namespace is set to `"user"`.

The user can designate a resource by a string using the following forms:
 - `"resource"` will look for an existing resource first in the current
   namespace, then (if not found) in the empty namespace, and if not found
   there, it will use the current namespace;
 - `"ns/resource"` identifies a resource in namespace `"ns"`;
 - in particular, `"/resource"` explicitly identifies a resource in the
   empty namespace.

This means that most of the time, the non-qualified `"resource"` form
will be enough: it either points to a (known) main-game resource,
or otherwise to a resource in the module's own namespace.
This provides namespace separation with minimal work from mod authors.

## Languages

Game strings are strings which will be displayed by the game engine,
and which will be translated to the player's selected game language.
Everywhere a game string is required (in dialogs, item names and
descriptions, etc.) the string must be entered in the source
in the form `_"string"`; the leading underscore marks this string
for translation (à la `gettext`).

```@docs
MarkedStrings.@__str
```

When assigned to relevant properties of game objects
(for example as object properties or dialog texts),
such strings are converted to `Strref` (numeric string references).
Translations for the strings are loaded from `.po` files
(in the standard format used by `gettext`, with tiny adaptations
for gendered languages), loaded by the `load_translations!` function
```@docs
load_translations!
```
and saved in all `dialog.tlk` and `dialogF.tlk` files.
For strings for which no translation is provided,
the string itself will be used instead.

The workflow for a mod author is thus the following:
1. systematically mark all game strings using the `_"string"` syntax
   (the module will help by throwing errors when the mark is forgotten);
2. whenever a string by itself would be ambiguous (e.g. `"Fine!"`),
   don't hesitate to provide extra context by prefixing it by a number of
   comment lines clarifying its meaning:
   comment lines immediately preceding a translated string
   will be extracted to the `.pot` file and shown to translators.
3. Short omments can also be included in the string using `?{keyword}`
   insertions: these will be automatically deleted when output to game
   files.
4. Finally produce the translation template `mod.pot` by running the
   `write_pot` function:
```julia
InfinityEngine.MarkedStrings.write_pot("source_file.jl", "mod.pot")
```

TODO: automatize this a bit more.

Workflow for translator (for the example, the French translator,
working on file `fr.po`):
1. either initialize or update the `.po` file with the commands
   `msginit -i mod.pot -o fr.po` or `msgmerge -U mod.pot fr.po`;
2. do the actual translation work with your preferred `.po` editing
   program (e.g. `POEdit`)

### Gendered languages

Some game languages are gendered: these language use two language files,
`dialog.tlk` and `dialogF.tlk`, depending on the grammatical gender
of CHARNAME.
Since most game strings (about 90% for French) are unchanged between
both files, it is a necessity to help the translator work on both
files at once.

Any marked string containing either the `?{F}` or `?{M}` keyword
will be output in the `.pot` file as *two* variants:
a variant marked with `?{M}`, whose translation will be saved
to `dialog.tlk`, and a variant marked with `?{F}`,
whose translation will be saved to `dialogF.tlk`.
(As a fall-back, if only one of these is translated,
then it will be used for both files).

It is thus advised to include a `?{M}` (or `?{F}`) mark in every game
string which grammatically refers to <CHARNAME>.
This helps the translator; even superfluous marks demand no extra work
for them, since they can always leave one of the translations empty
and the module will then use the other one, thus making
both translations identical.

For a short example, assume that the source string is:
```julia
_"?{M}Be strong, <CHARNAME>!"
```
Such a string will be extracted to two `.pot` entries:
```pot
msgid "?{M}Be strong, <CHARNAME>!"

msgid "?{F}Be strong, <CHARNAME>!"
```
This could be translated in French as two different messages:
```pot
msgid "?{M}Be strong, <CHARNAME>!"
msgstr "Sois fort, <CHARNAME> !"

msgid "?{F}Be strong, <CHARNAME>!"
msgstr "Sois forte, <CHARNAME> !"
```
However, in German this is gender-invariant, so the German translator
can always leave one of the strings empty:
```pot
msgid "?{M}Be strong, <CHARNAME>!"
msgstr ""

msgid "?{F}Be strong, <CHARNAME>!"
msgstr "Sei stark, <CHARNAME>!"
```
The non-empty message will then be saved to both `de_DE/dialog.tlk`
and `de_DE/dialogF.tlk`.


A translator seeing a string which requires gender-dependent translation,
but unmarked in the source file, can always manually edit the `.po` file
to include both versions;
however, contacting the source file author to correctly mark the input
is more robust and is the preferred solution.
