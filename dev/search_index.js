var documenterSearchIndex = {"docs":
[{"location":"dialogs/#Dialogs","page":"Dialogs","title":"Dialogs","text":"","category":"section"},{"location":"dialogs/#Structure","page":"Dialogs","title":"Structure","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"All in-game dialog is encoded as a state machine (a single state machine can represent the whole game dialog). States of this machine correspond to NPC text, while transitions are (usually) PC replies.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"On a given state (when presented when a text), the PC can choose which reply to select, i.e. which transition to follow to a target state. Some transitions are final (they close the dialog instead of having a target state), and some transitions also happen to not have PC text associated.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Since each state has a NPC speaker, states are indexed by pairs (actor, key), where the actor is the identity of the speaker; this is also how dialog is encoded in the game files: all dialog belonging to a single actor (states of this actor, and transitions from these states) are stored in a single game resource. Accordingly, the Actor data structure corresponds to all text said by a given NPC speaker, and all PC replies to this text.","category":"page"},{"location":"dialogs/#Displaying-dialogs","page":"Dialogs","title":"Displaying dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Reading an actor is as simple as loading it and letting the REPL display it: just try invoking actor(\"imoen\") and look at the result.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"actor","category":"page"},{"location":"dialogs/#InfinityEngine.actor","page":"Dialogs","title":"InfinityEngine.actor","text":"actor([game], \"name\")\n\nLoads the named actor from game files, or creates an empty actor if none exists with this name.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#Creating-new-dialogs","page":"Dialogs","title":"Creating new dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Several functions enable creating and editing game dialogs. The easiest feature is creation of entirely new dialogs. For this, it is enough to give a list of states connected by transitions.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The currently active actor is selected by the actor function. It is legal to call this on a non-existing actor name; it simply creates an empty actor, to which states and transitions will then be attached.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The say function creates states, while reply creates transitions. The module maintains a “last added state” variable. This variable is updated by calls to say to the last state added, and used by calls to reply to determine from which state a transition must be created.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"say\nreply","category":"page"},{"location":"dialogs/#InfinityEngine.say","page":"Dialogs","title":"InfinityEngine.say","text":"say({text | (label => text)}*; priority, trigger)\n\nIntroduces states of dialog for the current actor. If the label is omitted, a default (numeric, increasing) label will be inserted (although inserting an explicit label makes the state easier to reach).\n\nA single say call is equivalent to several successive say calls for the same current actor.\n\nSpecial cases\n\nimplicit, text-less transitions: say(text1) say(text2);\nmulti-say: say(text1, text2, ...) — actually equivalent to the previous form;\nchain with actor change: actor(name1) say(text1) actor(name2) say(text2)...;\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#InfinityEngine.reply","page":"Dialogs","title":"InfinityEngine.reply","text":"reply(text => label)\n\nIntroduces a state transition (player reply) pointing to the given label. The label may be one of:\n\n(\"actor\", state) (equivalently \"actor\" => state);\nstate  (uses current target actor);\nexit (creates a final transition).\n\nState may be either numeric (referring to the base game's states) or string. In the latter case, if it does not contain a slash, it will be prefixed by the current namespace : \"namespace/state\". This prevents states from different namespaces from interfering.\n\nSpecial forms:\n\nreply(exit) creates a text-less, final transition;\nreply(text) creates a pending transition: this will be connected to the next state inserted (via say).\n\nExamples:\n\n# chain to other actor:\nreply(\"Say Hi to Hull\" => \"hull\" => 0)\n# connect pending transition:\nsay(\"How do you do?\") reply(\"Fine!\") say(\"Let'sa go!\") reply(exit)\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#State-labels","page":"Dialogs","title":"State labels","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Numeric state keys correspond in principle to original game dialog. New labels should use strings.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Label strings are namespaced to avoid collisions.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Internally, a numeric label is produced by hashing the strings (to 64-bit integers). This allows adding approximately 2³² states (4 billion) to any actor before risking a key collision, and approximately 2⁵⁰ states before hitting any target lower than 16000 (where original game labels presumably reside).","category":"page"},{"location":"dialogs/#Final-transitions","page":"Dialogs","title":"Final transitions","text":"","category":"section"},{"location":"dialogs/#Chaining-and-implicit-transitions","page":"Dialogs","title":"Chaining and implicit transitions","text":"","category":"section"},{"location":"dialogs/#Pending-transitions","page":"Dialogs","title":"Pending transitions","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The implicit transitions created by chaining are all text-less transitions. If PC comments are needed, this can be done via pending transitions.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"A pending transition is a transition with no target state indicated. The transition will be connected to the next state to be added. Inserting such transitions in the middle of a say chain has the effect of inserting PC text while maintaining the structure of the chain: for example, say(A); reply(a); say(B) is equivalent to say(A); reply(a => labelB); say(labelB => B), without the need to give an explicit label to the target state.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The current pending transition must be connected (by calling say or interject, both of which always resolve existing pending transitions) before any other transition is created (by calling reply).","category":"page"},{"location":"dialogs/#Extending-existing-dialogs","page":"Dialogs","title":"Extending existing dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The from function allows changing the value of the “last added state” variable to any actor and any state.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"from","category":"page"},{"location":"dialogs/#InfinityEngine.from","page":"Dialogs","title":"InfinityEngine.from","text":"from([game], [actor], label)\n\nSets current state to actor, label. The state must exist.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Note that this is not always the same actor as the speaking actor selected by actor: namely, from selects a source actor (i.e. already existing states), whereas actor selects a target actor, for which states will be inserted.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"With a combination of from, actor and reply it is possible to extend existing states for an actor (by adding new transitions) and to add new states.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"from(\"imoen\", 0)\nreply(\"Oh, hi Imoen!\" => \"new state\")\nsay(\"new state\" => \"Hi you! Now we go on to our normal conversation.\")","category":"page"},{"location":"dialogs/#Inserting-into-existing-dialogs","page":"Dialogs","title":"Inserting into existing dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"New states can also be inserted into an existing transition. Say that A is the last created (source) state, with transitions tᵢ to states Bᵢ: A ——tᵢ——→ Bᵢ. The function interject can insert a new state X at the tail end of all the arrows tᵢ: A ——→X——tᵢ——→Bᵢ. Namely, there is now a single text-less transition from A to X, and all the original transitions from A now start from state X.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"interject","category":"page"},{"location":"dialogs/#InfinityEngine.interject","page":"Dialogs","title":"InfinityEngine.interject","text":"interject({text | (label => text)}*; priority, trigger)\n\nInserts text inside existing dialog. The new state(s) are inserted just after the current state, using tail insertions. \n\n\n\n\n\n","category":"function"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"interject moves the “source state” pointer to the newly created state X, so that it is possible to chain calls. For example, after from(A); interject(X); interject(Y), the result will be something like A——→X——→Y——tᵢ——→Bᵢ.","category":"page"},{"location":"dialogs/#interject-and-pending-transitions","page":"Dialogs","title":"interject and pending transitions","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"If the source state A has a pending transition when interject is called, then instead of creating a new text-less transition A→X, this pending transition is used (and connected) instead. This allows replacing the default text-less transition A→X by a transition with text A——x—→X in the following way: from(A); reply(x); interject(X).","category":"page"},{"location":"dialogs/#Deleting-existing-dialogs","page":"Dialogs","title":"Deleting existing dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Not currently possible (and not a very high priority).","category":"page"},{"location":"dialogs/#Attaching-data-to-dialogs","page":"Dialogs","title":"Attaching data to dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"TODO.","category":"page"},{"location":"dialogs/#State-priority","page":"Dialogs","title":"State priority","text":"","category":"section"},{"location":"dialogs/#Triggers","page":"Dialogs","title":"Triggers","text":"","category":"section"},{"location":"dialogs/#Actions-and-journal","page":"Dialogs","title":"Actions and journal","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"trigger\naction\njournal","category":"page"},{"location":"dialogs/#InfinityEngine.trigger","page":"Dialogs","title":"InfinityEngine.trigger","text":"trigger(string)\n\nAttaches a trigger to the next transition or state.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#InfinityEngine.action","page":"Dialogs","title":"InfinityEngine.action","text":"action(string)\n\nAttaches an action to the latest transition.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#InfinityEngine.journal","page":"Dialogs","title":"InfinityEngine.journal","text":"journal(string)\n\nAttaches a journal entry to the latest transition.\n\n\n\n\n\n","category":"function"},{"location":"#Home","page":"Home","title":"Home","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"This package provides an interface to Bioware's Infinity Engine games databases.","category":"page"},{"location":"","page":"Home","title":"Home","text":"warning: Support\nCurrently, only BG(2)EE games (including EET) are supported. Supporting other games is a long-term goal.","category":"page"},{"location":"#Running-the-module","page":"Home","title":"Running the module","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The central access point for all information related to a particular game installation is the Game data structure. Most of the functions of this module take a Game value as their first argument. For convenience, this argument can be omitted; in this case, the module will use its own global Game structure instead.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The Game structure is initialized from the directory containing the \"chitin.key\" file.","category":"page"},{"location":"","page":"Home","title":"Home","text":"using InfinityEngine\nInfinityEngine.init!(\"/home/CHARNAME/baldursgate\")\nInfinityEngine.game()","category":"page"},{"location":"","page":"Home","title":"Home","text":"InfinityEngine.init!\ncommit","category":"page"},{"location":"#InfinityEngine.init!","page":"Home","title":"InfinityEngine.init!","text":"init!(directory)\n\nInitializes the global game structure from this directory (the directory containing the chitin.key file).\n\n\n\n\n\n","category":"function"},{"location":"#InfinityEngine.commit","page":"Home","title":"InfinityEngine.commit","text":"commit([game])\n\nSaves all changed game data to the disk installation.\n\n\n\n\n\n","category":"function"},{"location":"#Game-information-status","page":"Home","title":"Game information status","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"All information related to a particular installation can be retrieved from the corresponding Game object. However, this is only saved to the filesystem when the commit function is called. This call saves game resources to the override directory, as well as extra persistent information in the state file GAMEDIR/state.toml.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Such information contains:","category":"page"},{"location":"","page":"Home","title":"Home","text":"the mapping between filesystem resource names (8 bytes max) and Julia-side symbolic resource names (namespaced, arbitrary strings);\nTODO translation information for game strings;","category":"page"},{"location":"#Accessing-game-resources","page":"Home","title":"Accessing game resources","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"The Game structure works as an index for all game information, including game resources (items, creatures, scripts...) and strings.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Resources of a given type are accessed via either selector functions (e.g. item(\"sw1h01\") is a bastard sword) or iterators (e.g. for itm in items()...). Iterator functions generally are the plural form of selectors.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The following resource types are currently implemented; each one of them is described in its own documentation page:","category":"page"},{"location":"","page":"Home","title":"Home","text":"Item: game items;\nActor: game dialogs;","category":"page"},{"location":"#Editing-resources","page":"Home","title":"Editing resources","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Game objects are handled as ordinary Julia structures:","category":"page"},{"location":"","page":"Home","title":"Home","text":"albruin = item(\"sw1h34\") # long sword Albruin\nalbruin.min_strength = 15 # silently marks the item for commit","category":"page"},{"location":"","page":"Home","title":"Home","text":"The assignment operators are silently overloaded so that they mark all modified game objects for commit to the override directory.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Dialogs have their own syntax for edition; see the say, reply etc. functions.","category":"page"},{"location":"#Resource-references-and-namespaces","page":"Home","title":"Resource references and namespaces","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"From the user point of view, resources are always referred to by strings. The module implements resource namespaces to prevent interference between different authors. Internally, each resource string is prefixed by a namespace, in the form \"namespace/resource\". The current namespace is set by the namespace() function.","category":"page"},{"location":"","page":"Home","title":"Home","text":"namespace","category":"page"},{"location":"#InfinityEngine.namespace","page":"Home","title":"InfinityEngine.namespace","text":"namespace([game], s)\n\nSets the current namespace for game resources being defined to s. The following default namespaces are used:\n\n\"\" for original game resources;\n\"user\" is the default namespace for added resources.\n\n\n\n\n\n","category":"function"},{"location":"","page":"Home","title":"Home","text":"The empty namespace \"\" is reserved for resources handled outside of this module (e.g. original game resources). At module initialization, the namespace is set to \"user\".","category":"page"},{"location":"","page":"Home","title":"Home","text":"The user can designate a resource by a string using the following forms:","category":"page"},{"location":"","page":"Home","title":"Home","text":"\"resource\" will look for an existing resource first in the current namespace, then (if not found) in the empty namespace, and if not found there, it will use the current namespace;\n\"ns/resource\" identifies a resource in namespace \"ns\";\nin particular, \"/resource\" explicitly identifies a resource in the empty namespace.","category":"page"},{"location":"","page":"Home","title":"Home","text":"This means that most of the time, the non-qualified \"resource\" form will be enough: it either points to a (known) main-game resource, or otherwise to a resource in the module's own namespace. This provides namespace separation with minimal work from mod authors.","category":"page"},{"location":"#Languages","page":"Home","title":"Languages","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Game strings are strings which will be displayed by the game engine, and which will be translated to the player's selected game language. Everywhere a game string is required (in dialogs, item names and descriptions, etc.) the string must be entered in the source in the form _\"string\"; the leading underscore marks this string for translation (à la gettext).","category":"page"},{"location":"","page":"Home","title":"Home","text":"MarkedStrings.@__str","category":"page"},{"location":"","page":"Home","title":"Home","text":"When assigned to relevant properties of game objects (for example as object properties or dialog texts), such strings are converted to Strref (numeric string references). Translations for the strings are loaded from .po files (in the standard format used by gettext, with tiny adaptations for gendered languages), loaded by the load_translations! function","category":"page"},{"location":"","page":"Home","title":"Home","text":"InfinityEngine.load_translations!","category":"page"},{"location":"#InfinityEngine.load_translations!","page":"Home","title":"InfinityEngine.load_translations!","text":"load_translations!([directory])\n\nLoads translations from all .po files in directory inside the game structures.\n\nIf directory is not provided, the default value is the directory containing the calling .jl file.\n\n\n\n\n\n","category":"function"},{"location":"","page":"Home","title":"Home","text":"and saved in all dialog.tlk and dialogF.tlk files. For strings for which no translation is provided, the string itself will be used instead.","category":"page"},{"location":"","page":"Home","title":"Home","text":"The workflow for a mod author is thus the following:","category":"page"},{"location":"","page":"Home","title":"Home","text":"systematically mark all game strings using the _\"string\" syntax (the module will help by throwing errors when the mark is forgotten);\nwhenever a string by itself would be ambiguous (e.g. \"Fine!\"), don't hesitate to provide extra context by prefixing it by a number of comment lines clarifying its meaning: comment lines immediately preceding a translated string will be extracted to the .pot file and shown to translators.\nShort omments can also be included in the string using ?{keyword} insertions: these will be automatically deleted when output to game files.\nFinally produce the translation template mod.pot by running the write_pot function:","category":"page"},{"location":"","page":"Home","title":"Home","text":"InfinityEngine.MarkedStrings.write_pot(\"source_file.jl\", \"mod.pot\")","category":"page"},{"location":"","page":"Home","title":"Home","text":"TODO: automatize this a bit more.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Workflow for translator (for the example, the French translator, working on file fr.po):","category":"page"},{"location":"","page":"Home","title":"Home","text":"either initialize or update the .po file with the commands msginit -i mod.pot -o fr.po or msgmerge -U mod.pot fr.po;\ndo the actual translation work with your preferred .po editing program (e.g. POEdit)","category":"page"},{"location":"#Gendered-languages","page":"Home","title":"Gendered languages","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Some game languages are gendered: these language use two language files, dialog.tlk and dialogF.tlk, depending on the grammatical gender of CHARNAME. Since most game strings (about 90% for French) are unchanged between both files, it is a necessity to help the translator work on both files at once.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Any marked string containing either the ?{F} or ?{M} keyword will be output in the .pot file as two variants: a variant marked with ?{M}, whose translation will be saved to dialog.tlk, and a variant marked with ?{F}, whose translation will be saved to dialogF.tlk. (As a fall-back, if only one of these is translated, then it will be used for both files).","category":"page"},{"location":"","page":"Home","title":"Home","text":"It is thus advised to include a ?{M} (or ?{F}) mark in every game string which grammatically refers to <CHARNAME>. This helps the translator; even superfluous marks demand no extra work for them, since they can always leave one of the translations empty and the module will then use the other one, thus making both translations identical.","category":"page"},{"location":"","page":"Home","title":"Home","text":"For a short example, assume that the source string is:","category":"page"},{"location":"","page":"Home","title":"Home","text":"_\"?{M}Be strong, <CHARNAME>!\"","category":"page"},{"location":"","page":"Home","title":"Home","text":"Such a string will be extracted to two .pot entries:","category":"page"},{"location":"","page":"Home","title":"Home","text":"msgid \"?{M}Be strong, <CHARNAME>!\"\n\nmsgid \"?{F}Be strong, <CHARNAME>!\"","category":"page"},{"location":"","page":"Home","title":"Home","text":"This could be translated in French as two different messages:","category":"page"},{"location":"","page":"Home","title":"Home","text":"msgid \"?{M}Be strong, <CHARNAME>!\"\nmsgstr \"Sois fort, <CHARNAME> !\"\n\nmsgid \"?{F}Be strong, <CHARNAME>!\"\nmsgstr \"Sois forte, <CHARNAME> !\"","category":"page"},{"location":"","page":"Home","title":"Home","text":"However, in German this is gender-invariant, so the German translator can always leave one of the strings empty:","category":"page"},{"location":"","page":"Home","title":"Home","text":"msgid \"?{M}Be strong, <CHARNAME>!\"\nmsgstr \"\"\n\nmsgid \"?{F}Be strong, <CHARNAME>!\"\nmsgstr \"Sei stark, <CHARNAME>!\"","category":"page"},{"location":"","page":"Home","title":"Home","text":"The non-empty message will then be saved to both de_DE/dialog.tlk and de_DE/dialogF.tlk.","category":"page"},{"location":"","page":"Home","title":"Home","text":"A translator seeing a string which requires gender-dependent translation, but unmarked in the source file, can always manually edit the .po file to include both versions; however, contacting the source file author to correctly mark the input is more robust and is the preferred solution.","category":"page"},{"location":"internals/#Internals","page":"Internals","title":"Internals","text":"","category":"section"},{"location":"internals/#String-references","page":"Internals","title":"String references","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"The Strref type mirrors the type used in-game:","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"InfinityEngine.Strref","category":"page"},{"location":"internals/#InfinityEngine.Strref","page":"Internals","title":"InfinityEngine.Strref","text":"Strref\n\nIndex (32-bit) referring to a translated string in a \"dialog.tlk\" file.\n\n\n\n\n\n","category":"type"},{"location":"internals/","page":"Internals","title":"Internals","text":"These values are zero-indexed (Julia arrays are generally 1-indexed). Index 0 is the string \"<NO TEXT>\", which is technically valid, but should never be actually used in-game; we use this as a marker for invalid or missing strings, which allows functions to gracefully fail by never referring to an inexistent string.","category":"page"},{"location":"internals/#Resource-references","page":"Internals","title":"Resource references","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"The game admits several resource types (about 40 of them in total), for referring to e.g. items, scripts or spells.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"This module uses two systems for referring to game resources:","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"the game's own key type (“shortrefs”): 8-byte strings, used as either a key in the chitin.key file or as a file name in the filesystem;\nuser-friendly references (Resref), which can be arbitrary strings and are namespaced to prevent name collision between mods.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"This double system should be transparent to mod authors; this is a strong design goal of this module. To this end, the Game structure maintains a dictionary for Resref to shortref conversion. This dictionary is stored in the shortref field of the GameResources structure, and is saved in the [resources] section of the \"state.toml\" file.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"InfinityEngine.Resref\nInfinityEngine.ResIO","category":"page"},{"location":"internals/#InfinityEngine.Resref","page":"Internals","title":"InfinityEngine.Resref","text":"Resref{T}\n\nA (long) resource descriptor: this contains a (static) type and a (dynamic) namespace and name uniquely identifying the resource.\n\n\n\n\n\n","category":"type"},{"location":"internals/#InfinityEngine.ResIO","page":"Internals","title":"InfinityEngine.ResIO","text":"ResIO{Type}, ResIO\"TYPE\"\n\nAn IO object marked (statically) with the type of resource being read (either from filesystem or from a BIF content), as well as (dynamically) with global properties of the resource (i.e. resource name, root object).\n\nSince the resource type determines the read/write methods (and only takes a finite set of values) this is a type parameter.\nThe resource name (i.e. basename of the file without extension) is stored as a field. The name is canonicalized as upper-case.\n\nDefined methods include:\n\nResIO\"EXT\": macro defining static value for this file type.\n\n\n\n\n\n","category":"type"},{"location":"internals/","page":"Internals","title":"Internals","text":"While resource names (either 8-byte or long) are dynamic properties, resource type (“item”/“script” etc.) is a static, compile-time property; thus it is implemented as a type parameter instead of as a structure field. This allows handling resources via generic functions, which is the best of both worlds:","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"it is as fast as writing a big list of similar functions find_item, find_creature etc.;\nit is as readable/maintenable as writing a single function find_resource(type, name).","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"Thanks Julia for generic functions!","category":"page"},{"location":"internals/#The-Game-structure","page":"Internals","title":"The Game structure","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"This structure holds global information about a particular game installation. It contains sub-structures dedicated to specific information: GameResources, GameStrings, DialogContext.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"InfinityEngine.Game","category":"page"},{"location":"internals/#InfinityEngine.Game","page":"Internals","title":"InfinityEngine.Game","text":"Game\n\nMain structure holding all top-level data for a game installation, including:\n\nresource repository (three kinds: key/bif, override, memory database of modified resources);\ntlk strings,\nconversion of resource references to 8-byte short references;\ndialog-building context.\n\nThis structure works as a pseudo-dictionary: indexing it with keys of a given Resref type returns data structures of the corresponding resource type.\n\ngame[resref]: returns the data structure described by this resource.\nget and get! methods, e.g. get(game, resref) do ... end\nkeys(game, type): returns a vector of all names of existing resources of this type.\n\n\n\n\n\n","category":"type"},{"location":"internals/#GameResources","page":"Internals","title":"GameResources","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"This structure is responsible for accessing game resources from their symbolic references, as well as for saving them.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"InfinityEngine.GameResources","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"It stores mirrors of the chitin.key and override filesystem tables, as well as the conversion from resource references to short in-game references, and the table of modified resources.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"The table of modified resources is the set of all game resources needing to be written to the filesystem in the next commit call.","category":"page"},{"location":"internals/#RootedResource","page":"Internals","title":"RootedResource","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"This is the common subtype for all user-modifiable resources (except dialogs, which have their own syntax). The resources all know their own reference (via the ref field) and child structures (e.g. item abilities) store a pointer to the root resource (in this case, a pointer back to the item).","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"The setproperty! function is overloaded so that, whenever a field of such a structure is modified, the modified object will be stored in the appropriate table in the GameResource object. Then the commit function knows which structures need to be saved to the override/ directory.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"InfinityEngine.RootResource\nInfinityEngine.RootedResource","category":"page"},{"location":"internals/#InfinityEngine.RootResource","page":"Internals","title":"InfinityEngine.RootResource","text":"RootResource\n\nThe root of a resource tree, i.e. a resource which will be saved in a game file. This resource also holds an identifier designating it (and the game file).\n\n\n\n\n\n","category":"type"},{"location":"internals/","page":"Internals","title":"Internals","text":"TODO: it would theoretically be possible to implement ownership for resources (or even individual fields); this would completely solve mod conflicts (but be quite hard to implement).","category":"page"},{"location":"internals/#GameStrings","page":"Internals","title":"GameStrings","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"This structure manages translation of game strings. It stores a dictionary between newly added game strings (in untranslated, source form) and in-game numeric Strrefs.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"InfinityEngine.GameStrings","category":"page"},{"location":"internals/#InfinityEngine.GameStrings","page":"Internals","title":"InfinityEngine.GameStrings","text":"GameStrings\n\nCollection of game strings, indexed by string and language.\n\nInterface\n\nStrref(gamestrings, x): string key to Strref conversion.\ngamestrings[language, strref]: Strref to String conversion.\ncommit(gamestrings): saves state to filesystem.\ninit!(directory): loads state from filesystem.\n\nFields\n\nlet N0 = number of base-game strings (34000)     N1 = number of new tlk strings     N2 = number of memory tlk strings\n\nThen Strref(0..N0-1) are base-game strings      Strref(N0..N0+N1+N2-1) are new strings = indexed by strings\n\nStrref(i + N0-1) ⇔ new_string[i]\n\nStored value is N0-1 == 33999 for BG2; constant even when modifying tlk\n\nDisplaying a string for Strref(i): if i ≤ #tlk-1 then tlk[i+1]   else new_stringi-N0+1\n\n\n\n\n\n","category":"type"},{"location":"internals/#state.toml","page":"Internals","title":"The state file","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"All persistent information from the Game structure is stored in this file.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"This file has sections [resources] and [strings] corresponding to the sub-structures of Game.","category":"page"},{"location":"items/#Items","page":"Items","title":"Items","text":"","category":"section"},{"location":"items/","page":"Items","title":"Items","text":"item\nitems","category":"page"},{"location":"items/#InfinityEngine.item","page":"Items","title":"InfinityEngine.item","text":"item([game], ref)\n\nReturns the item with given reference.\n\n\n\n\n\n","category":"function"},{"location":"items/#InfinityEngine.items","page":"Items","title":"InfinityEngine.items","text":"items([game], [regex])\n\nReturns an iterator over all items. If a regex is provided then only those items with matching reference are included.\n\n\n\n\n\n","category":"function"}]
}
