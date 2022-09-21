# Immediate
 - [ ] make state labels symbols (not strings) as a syntax safeguard
 - [ ] dialog docs: make some pictures with `graphviz`
 - [x] try a mini-mod: patch imoen.dlg
 - heavily-modded EET has 82k key resources, 90k overrides, 100k strings
# Mod structure
**NEEDS**:
 - API for a given mod:
  - [ ] `newmod(dir)`: creates skeleton
   - [ ] suggestion: `include("moddir/setup.jl")`
   - [ ] automatic generation of Julia module + `namespace()`
    - [ ] use `objectid(@__MODULE__)` in namespaces? or directory?
  - [ ] `translate(dir)`: creates/updates `.po` files
  - [ ] `install(dir)`: installs mod
   - [ ] binary resources!
  - [ ] `uninstall(dir)` (harder!)
  - [ ] `validate(dir)`
   - [ ] also checks correctness of metadata (in module global vars!)
   - [ ] module metadata: module name, description, author, url (for git)
   - [ ] global variables for mod metadata (`AUTHOR` etc. spring to mind)
 - [ ] **Mod components**
 - component list/tree + metadata: dependencies, parameters
# Translations
 - [ ] find something better than `?{M}` to indicate gendered dialog
 - `language()` also loads the translation files (any `.po` in current
   directory)
 - make xgettext-mode easily accessible from author-side
   e.g. `use InfinityEngine; translate_mod("foobar.jl")`
 - [x] translation is done for all game languages
 - [x] replace calls to `msgfmt` by a compilation to Julia dictionaries
  - [x] which need to be included by the mod(ule)
 - [ ] translation header should be filled from module info (package version
   etc.)
# Dialogs
## Remaining work
 - [ ] save dialog state priority in `state.toml` (this is big!)
  - needs: for each modified actor, offset + vector of Float32
  - encoded as `[priority] dialog1 = offset,prio1,prio2,...` etc.
 - [ ] finalize actor (i.e. add exit transition to any empty state)
  - see what to do with actions attached to these
 - [ ] allow specifying actor in `say`, e.g. `say(actor=>label=>text)`
## actions, triggers
  - [ ] see if triggers are not more logically placed **After** actions
  - [ ] is `\r\n` needed? check this by modifying Hull's actions
 - trigger/action text: replace strings by `Strref`
  - use interpolated strings...
  - a custom string mode? for interpolated strings...
    action"Journal($s)" # replaces $s by Strref
 - [ ] `reply(text => actor => state)`
 - [x] reindex states
  - [ ] all of them (across actors) before saving
 - [ ] journal flags: `journal(..., Unsolved)`?
 - [ ] see what to do with actions attached to transitions on `interject`
   calls (i.e. Weidu's `INTERJECT_COPY_TRANS2?`).
 - [ ] `.d` => julia syntactic transformation
# General work
 - [ ] try to be a bit faster by pre-hashing all the `Symbol`s used as keys
   in object tables (at parser stage)
   a good hash function is `last(s)+121*s[2]⊻238*s[1]`
   (only 2 collisions, on infrequent resources)
   do the 2-step hash thing
 - [ ] write a custom REPL mode (`HeaderREPLs.jl`) for mod manager
  - [ ] mod manager (WeiDU + Julia mods)
  - mod editing (generate, update, translations, validate)
 - [x] typed attack: `Blunt(1d4+1)`
  - converts to Dice?
 - [x] speed: maybe add a few `sizehints!` in KeyIndex and TlkStrings.
 - [x] consider using ImmutableDicts **DONE** and it is less efficient
 - [x] decompile dialog to Julia
  - [ ] missing flags
 - `args_to_kw`: use parameters e.g.
   `AbstractString => (:name, :description), Integer => (...)`
## Write a test set
 - [x] items
 - [x] dialogs
 - [x] try recreating a simple dialog (imoen.dlg, hull.dlg) from scratcho
  - [x] with a few translations
## Modified objects
 - saving a creature reorders the items as needed
## Object origin
 - use stacktraces
# Desired syntax for examples from WeiDU doc:
## _DONE_ 10.1
    for item in items(game, r"sw1h.*"i)
      item.damage = d6
    end
 - Almost done, except we need to say `item.ability[1].damage = d6`
 - we can also keep the damage bonus:
    x = item.ability[1]; x.damage = d6 + x.damage_bonus
## 10.2
    for script in scripts(game, r"ar[0.6].*")
      script = myscript * script
      XXX use prepend! ?
    end
## _DONE_ 10.3: change all longswords to have minimum strength 10
    for item in items(game)
      (item.type == Longsword) && (item.min_strength = 10)
    end
    save(game)
### _DONE_ Additionally:
    glamdring = Longsword("Glamdring", ...)
## 10.4 add an item to a creature:
    acolyte1 = creature(game, "acolyte1")
    acolyte1.shield == nothing && (acolyte1.shield = "shld01")

 - make `inventory(creature)` an iterator over all items
## 10.5 add an item to a store:
    ribald = store(game, "ribald")
    push!(ribald.items, ("hamm05", 0, 0, 0))
      # or simply "hamm05" and charges will be auto-determined
      # or insert! to determine position
#
## 10.7 `set_2da_entry`
    table(game, "kits")[i, j] = "value"
    # allow string indices

# Syntax diabetes
 + `damage = Crushing(1d6+1)`
 - `timing = Delay_Seconds(...)`

vim: et:
