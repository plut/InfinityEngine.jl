# `InfinityEngine.jl`

This package provides an interface to Bioware's Infinity Engine games
databases.

!!! warning "Support"
    Currently, only BG(2)EE games (including EET) are supported.
    Supporting other games is a long-term goal.

## Running the module

All information is held in the global `game` object.
This object can be implicitly passed to many of the module's methods:
```julia
using InfinityEngine
InfinityEngine.init!() # equivalently: init!(InfinityEngine.game)
InfinityEngine.game
```
```@docs
InfinityEngine.Game
save
```

## Accessing the database

Accessing resource of a given type can be done via the `item`, `actor`
etc. functions:
```julia
item("sw1h01") # bastard sword
for i in items(); #= ... =#; end
actor("imoen")
```

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

Game strings are entered as native strings.
The current language for those strings is declared
using the `language` function:
```@docs
language
```
These strings are converted to `Strref` (numeric string references)
on the fly.

**TODO**: use `gettext` to independently provide translations.
