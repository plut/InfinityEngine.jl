# Internals

## String references

The `Strref` type mirrors the type used in-game:
```@docs
InfinityEngine.Strref
```
These values are zero-indexed (Julia arrays are generally 1-indexed).
Index 0 is the string `"<NO TEXT>"`, which is technically valid,
but should never be actually used in-game;
we use this as a marker for invalid or missing strings,
which allows functions to gracefully fail by never referring
to an inexistent string.

## Resource references

The game admits several resource types (about 40 of them in total),
for referring to e.g. items, scripts or spells.

This module uses two systems for referring to game resources:
 - the game's own key type (“**shortrefs**”): 8-byte strings,
   used as either a key in the `chitin.key` file or as a file name
   in the filesystem;
 - user-friendly references (`Resref`), which can be arbitrary strings
   and are namespaced to prevent name collision between mods.

This double system should be transparent to mod authors;
this is a strong design goal of this module.
To this end, the `Game` structure maintains a dictionary
for `Resref` to shortref conversion.
This dictionary is stored in the `shortref` field of the
`GameResources` structure, and is saved in the `[resources]`
section of the `"state.toml"` file.

```@docs
InfinityEngine.Resref
InfinityEngine.ResIO
```

While resource names (either 8-byte or long) are dynamic properties,
resource type (“item”/“script” etc.) is a static, compile-time property;
thus it is implemented as a type parameter instead of as a structure
field. This allows handling resources via generic functions, which is the
best of both worlds:
 - it is as fast as writing a big list of similar functions `find_item`,
   `find_creature` etc.;
 - it is as readable/maintenable as writing a single function
   `find_resource(type, name)`.
Thanks Julia for generic functions!


## The `Game` structure

This structure holds global information about a particular game
installation.
It contains sub-structures dedicated to specific information:
`GameResources`, `GameStrings`, `DialogContext`.

```@docs
InfinityEngine.Game
```

### `GameResources`

This structure is responsible for accessing game resources
from their symbolic references,
as well as for saving them.

```@docs
InfinityEngine.GameResources
```

It stores mirrors of the `chitin.key` and `override` filesystem tables,
as well as the conversion from [resource references](@ref) to
short in-game references, and the table of modified resources.

The table of modified resources is the set of all game resources
needing to be written to the filesystem in the next `commit` call.

### `RootedResource`

This is the common subtype for all user-modifiable resources
(except dialogs, which have their own syntax).
The resources all know their own reference (via the `ref` field)
and child structures (e.g. item abilities) store a pointer to the
root resource (in this case, a pointer back to the item).

The `setproperty!` function is overloaded so that,
whenever a field of such a structure is modified,
the modified object will be stored in the appropriate table
in the `GameResource` object. Then the `commit` function
knows which structures need to be saved to the `override/` directory.

```@docs
InfinityEngine.RootResource
InfinityEngine.RootedResource
```

TODO: it would theoretically be possible to implement
ownership for resources (or even individual fields);
this would completely solve mod conflicts (but be quite hard to
implement).

### `GameStrings`

This structure manages translation of game strings.
It stores a dictionary between
newly added game strings (in untranslated, source form)
and in-game numeric `Strref`s.

```@docs
InfinityEngine.GameStrings
```

## [The state file](@id state.toml)

All persistent information from the `Game` structure
is stored in this file.

This file has sections `[resources]` and `[strings]`
corresponding to the sub-structures of `Game`.

