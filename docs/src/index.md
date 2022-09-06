# `InfinityEngine.jl`

This package provides an interface to Bioware's Infinity Engine games
databases.

!!! warning "Support"
    Currently, only BG(2)EE games (including EET) are supported.
    Supporting other games is a long-term goal.

## Running the module

The `Game` data structure holds all of the relevant game information.
Most of the API of this module takes a `::Game` value as their first
argument.
For convenience, this argument can always be omitted;
the module will use a global `Game` structure instead:
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

## Accessing the database

Accessing resource of a given type can be done via the `item`, `actor`
etc. functions, and the corresponding iterators in plural form:
```julia
item("sw1h01") # bastard sword
for i in items(); #= ... do something... =#; end
actor("imoen")
```

Game objects are handled as ordinary Julia structures:
```julia
albruin = item("sw1h34") # long sword Albruin
albruin.min_strength = 15 # silently marks the item for commit
```
The assignment operators are silently overloaded so that they mark
all modified game objects for commit to the `override` directory.


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
The empty namespace `""` corresponds to resources handled
outside of this module (e.g. original game resources).
At module initialization, the namespace is set to `"user"`.

A resource string can therefore take the following form:
 - `"resource"` will look for an existing resource first in the current
   namespace, then (if not found) in the root namespace, and if not found
   there, it will use the current namespace;
 - `"ns/resource"` identifies a resource in namespace `"ns"`;
 - in particular, `"/resource"` explicitly identifies a resource in the
   root namespace.

## Languages

Game strings are strings which will be displayed by the game engine
and which will be translated on the way.
Everywhere a game string is required (in dialogs, item names and
descriptions, etc.) the string must be entered in the source
in the form `_"string"`; the leading underscore marks this string
for translation (à la `gettext`).

These strings are converted to `Strref` (numeric string references)
on the fly when assigned to relevant parts of game structures
(for example as object properties or dialog texts).
Translations for the strings are loaded from `.po` files
(in the standard format used by `gettext`, with tiny adaptations
for gendered languages), loaded by the `load_translations!` function
```@docs
load_translations!
```
and saved in all `dialog.tlk` and `dialogF.tlk` files.
For strings for which no translation is provided,
the string itself will be used instead.
