var documenterSearchIndex = {"docs":
[{"location":"dialogs/#Dialogs","page":"Dialogs","title":"Dialogs","text":"","category":"section"},{"location":"dialogs/#Structure","page":"Dialogs","title":"Structure","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"All in-game dialog is encoded as a state machine (a single state machine can represent the whole game dialog). States of this machine correspond to NPC text, while transitions are (usually) PC replies.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"On a given state (when presented when a text), the PC can choose which reply to select, i.e. which transition to follow to a target state. Some transitions are final (they close the dialog instead of having a target state), and some transitions also happen to not have PC text associated.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Since each state has a NPC speaker, states are indexed by pairs (actor, key), where the actor is the identity of the speaker; this is also how dialog is encoded in the game files: all dialog belonging to a single actor (states of this actor, and transitions from these states) are stored in a single game resource. Accordingly, the Actor data structure corresponds to all text said by a given NPC speaker, and all PC replies to this text.","category":"page"},{"location":"dialogs/#Displaying-dialogs","page":"Dialogs","title":"Displaying dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Reading an actor is as simple as loading it and letting the REPL display it: just try invoking actor(\"imoen\") and look at the result.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"actor","category":"page"},{"location":"dialogs/#InfinityEngine.actor","page":"Dialogs","title":"InfinityEngine.actor","text":"actor([game], \"name\")\n\nLoads the named actor from game files, or creates an empty actor if none exists with this name.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#Creating-new-dialogs","page":"Dialogs","title":"Creating new dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Several functions enable creating and editing game dialogs. The easiest feature is creation of entirely new dialogs. For this, it is enough to give a list of states connected by transitions.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The currently active actor is selected by the actor function. It is legal to call this on a non-existing actor name; it simply creates an empty actor, to which states and transitions will then be attached.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The say function creates states, while reply creates transitions. The module maintains a “last added state” variable. This variable is updated by calls to say to the last state added, and used by calls to reply to determine from which state a transition must be created.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"say\nreply","category":"page"},{"location":"dialogs/#InfinityEngine.say","page":"Dialogs","title":"InfinityEngine.say","text":"say({text | (label => text)}*; priority, trigger)\n\nIntroduces states of dialog for the current actor. If the label is omitted, a default (numeric, increasing) label will be inserted (although inserting an explicit label makes the state easier to reach).\n\nA single say call is equivalent to several successive say calls for the same current actor.\n\nSpecial cases\n\nimplicit, text-less transitions: say(text1) say(text2);\nmulti-say: say(text1, text2, ...) — actually equivalent to the previous form;\nchain with actor change: actor(name1) say(text1) actor(name2) say(text2)...;\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#InfinityEngine.reply","page":"Dialogs","title":"InfinityEngine.reply","text":"reply(text => label)\n\nIntroduces a state transition (player reply) pointing to the given label. The label may be one of:\n\n(\"actor\", state) (equivalently \"actor\" => state);\nstate  (uses current target actor);\nexit (creates a final transition).\n\nState may be either numeric (referring to the base game's states) or string. In the latter case, if it does not contain a slash, it will be prefixed by the current namespace : \"namespace/state\". This prevents states from different namespaces from interfering.\n\nSpecial forms:\n\nreply(exit) creates a text-less, final transition;\nreply(text) creates a pending transition: this will be connected to the next state inserted (via say).\n\nExamples:\n\n# chain to other actor:\nreply(\"Say Hi to Hull\" => \"hull\" => 0)\n# connect pending transition:\nsay(\"How do you do?\") reply(\"Fine!\") say(\"Let'sa go!\") reply(exit)\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#State-labels","page":"Dialogs","title":"State labels","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Numeric state keys correspond in principle to original game dialog. New labels should use strings.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Label strings are namespaced to avoid collisions.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Internally, a numeric label is produced by hashing the strings (to 64-bit integers). This allows adding approximately 2³² states (4 billion) to any actor before risking a key collision, and approximately 2⁵⁰ states before hitting any target lower than 16000 (where original game labels presumably reside).","category":"page"},{"location":"dialogs/#Final-transitions","page":"Dialogs","title":"Final transitions","text":"","category":"section"},{"location":"dialogs/#Chaining-and-implicit-transitions","page":"Dialogs","title":"Chaining and implicit transitions","text":"","category":"section"},{"location":"dialogs/#Pending-transitions","page":"Dialogs","title":"Pending transitions","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The implicit transitions created by chaining are all text-less transitions. If PC comments are needed, this can be done via pending transitions.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"A pending transition is a transition with no target state indicated. The transition will be connected to the next state to be added. Inserting such transitions in the middle of a say chain has the effect of inserting PC text while maintaining the structure of the chain: for example, say(A); reply(a); say(B) is equivalent to say(A); reply(a => labelB); say(labelB => B), without the need to give an explicit label to the target state.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The current pending transition must be connected (by calling say or interject, both of which always resolve existing pending transitions) before any other transition is created (by calling reply).","category":"page"},{"location":"dialogs/#Extending-existing-dialogs","page":"Dialogs","title":"Extending existing dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"The from function allows changing the value of the “last added state” variable to any actor and any state.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"from","category":"page"},{"location":"dialogs/#InfinityEngine.from","page":"Dialogs","title":"InfinityEngine.from","text":"from([game], [actor], label)\n\nSets current state to actor, label. The state must exist.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Note that this is not always the same actor as the speaking actor selected by actor: namely, from selects a source actor (i.e. already existing states), whereas actor selects a target actor, for which states will be inserted.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"With a combination of from, actor and reply it is possible to extend existing states for an actor (by adding new transitions) and to add new states.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"from(\"imoen\", 0)\nreply(\"Oh, hi Imoen!\" => \"new state\")\nsay(\"new state\" => \"Hi you! Now we go on to our normal conversation.\")","category":"page"},{"location":"dialogs/#Inserting-into-existing-dialogs","page":"Dialogs","title":"Inserting into existing dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"New states can also be inserted into an existing transition. Say that A is the last created (source) state, with transitions tᵢ to states Bᵢ: A ——tᵢ——→ Bᵢ. The function interject can insert a new state X at the tail end of all the arrows tᵢ: A ——→X——tᵢ——→Bᵢ. Namely, there is now a single text-less transition from A to X, and all the original transitions from A now start from state X.","category":"page"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"interject","category":"page"},{"location":"dialogs/#InfinityEngine.interject","page":"Dialogs","title":"InfinityEngine.interject","text":"interject({text | (label => text)}*; priority, trigger)\n\nInserts text inside existing dialog. The new state(s) are inserted just after the current state, using tail insertions. \n\n\n\n\n\n","category":"function"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"interject moves the “source state” pointer to the newly created state X, so that it is possible to chain calls. For example, after from(A); interject(X); interject(Y), the result will be something like A——→X——→Y——tᵢ——→Bᵢ.","category":"page"},{"location":"dialogs/#interject-and-pending-transitions","page":"Dialogs","title":"interject and pending transitions","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"If the source state A has a pending transition when interject is called, then instead of creating a new text-less transition A→X, this pending transition is used (and connected) instead. This allows replacing the default text-less transition A→X by a transition with text A——x—→X in the following way: from(A); reply(x); interject(X).","category":"page"},{"location":"dialogs/#Deleting-existing-dialogs","page":"Dialogs","title":"Deleting existing dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"Not currently possible (and not a very high priority).","category":"page"},{"location":"dialogs/#Attaching-data-to-dialogs","page":"Dialogs","title":"Attaching data to dialogs","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"TODO.","category":"page"},{"location":"dialogs/#State-priority","page":"Dialogs","title":"State priority","text":"","category":"section"},{"location":"dialogs/#Triggers","page":"Dialogs","title":"Triggers","text":"","category":"section"},{"location":"dialogs/#Actions-and-journal","page":"Dialogs","title":"Actions and journal","text":"","category":"section"},{"location":"dialogs/","page":"Dialogs","title":"Dialogs","text":"trigger\naction\njournal","category":"page"},{"location":"dialogs/#InfinityEngine.trigger","page":"Dialogs","title":"InfinityEngine.trigger","text":"trigger(string)\n\nAttaches a trigger to the next transition or state.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#InfinityEngine.action","page":"Dialogs","title":"InfinityEngine.action","text":"action(string)\n\nAttaches an action to the latest transition.\n\n\n\n\n\n","category":"function"},{"location":"dialogs/#InfinityEngine.journal","page":"Dialogs","title":"InfinityEngine.journal","text":"journal(string)\n\nAttaches a journal entry to the latest transition.\n\n\n\n\n\n","category":"function"},{"location":"#InfinityEngine.jl","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"","category":"section"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"This package provides an interface to Bioware's Infinity Engine games databases.","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"warning: Support\nCurrently, only BG(2)EE games (including EET) are supported. Supporting other games is a long-term goal.","category":"page"},{"location":"#Running-the-module","page":"InfinityEngine.jl","title":"Running the module","text":"","category":"section"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"The Game data structure holds all of the relevant game information. Most of the API of this module takes a ::Game value as their first argument. For convenience, this argument can always be omitted; the module will use a global Game structure instead:","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"using InfinityEngine\nInfinityEngine.init!(\"/home/CHARNAME/baldursgate\")\nInfinityEngine.game()","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"InfinityEngine.Game\nInfinityEngine.init!\ncommit","category":"page"},{"location":"#InfinityEngine.Game","page":"InfinityEngine.jl","title":"InfinityEngine.Game","text":"Game\n\nMain structure holding all top-level data for a game installation, including:\n\nresource repository (three kinds: key/bif, override, memory database of modified resources);\ntlk strings,\nconversion of resource references to 8-byte short references;\ndialog-building context.\n\nThis structure works as a pseudo-dictionary: indexing it with keys of a given Resref type returns data structures of the corresponding resource type.\n\ngame[resref]: returns the data structure described by this resource.\nget and get! methods, e.g. get(game, resref) do ... end\nkeys(game, type): returns a vector of all names of existing resources of this type.\n\n\n\n\n\n","category":"type"},{"location":"#InfinityEngine.init!","page":"InfinityEngine.jl","title":"InfinityEngine.init!","text":"init!(directory)\n\nInitializes the global game structure from this directory (the directory containing the chitin.key file).\n\n\n\n\n\n","category":"function"},{"location":"#InfinityEngine.commit","page":"InfinityEngine.jl","title":"InfinityEngine.commit","text":"commit([game])\n\nSaves all changed game data to the disk installation.\n\n\n\n\n\n","category":"function"},{"location":"#Accessing-the-database","page":"InfinityEngine.jl","title":"Accessing the database","text":"","category":"section"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"Accessing resource of a given type can be done via the item, actor etc. functions, and the corresponding iterators in plural form:","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"item(\"sw1h01\") # bastard sword\nfor i in items(); #= ... do something... =#; end\nactor(\"imoen\")","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"Game objects are handled as ordinary Julia structures:","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"albruin = item(\"sw1h34\") # long sword Albruin\nalbruin.min_strength = 15 # silently marks the item for commit","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"The assignment operators are silently overloaded so that they mark all modified game objects for commit to the override directory.","category":"page"},{"location":"#Resource-references-and-namespaces","page":"InfinityEngine.jl","title":"Resource references and namespaces","text":"","category":"section"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"From the user point of view, resources are always referred to by strings. The module implements resource namespaces to prevent interference between different authors. Internally, each resource string is prefixed by a namespace, in the form \"namespace/resource\". The current namespace is set by the namespace() function.","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"namespace","category":"page"},{"location":"#InfinityEngine.namespace","page":"InfinityEngine.jl","title":"InfinityEngine.namespace","text":"namespace([game], s)\n\nSets the current namespace for game resources being defined to s. The following default namespaces are used:\n\n\"\" for original game resources;\n\"user\" is the default namespace for added resources.\n\n\n\n\n\n","category":"function"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"The empty namespace \"\" corresponds to resources handled outside of this module (e.g. original game resources). At module initialization, the namespace is set to \"user\".","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"A resource string can therefore take the following form:","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"\"resource\" will look for an existing resource first in the current namespace, then (if not found) in the root namespace, and if not found there, it will use the current namespace;\n\"ns/resource\" identifies a resource in namespace \"ns\";\nin particular, \"/resource\" explicitly identifies a resource in the root namespace.","category":"page"},{"location":"#Languages","page":"InfinityEngine.jl","title":"Languages","text":"","category":"section"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"Game strings are strings which will be displayed by the game engine and which will be translated on the way. Everywhere a game string is required (in dialogs, item names and descriptions, etc.) the string must be entered in the source in the form _\"string\"; the leading underscore marks this string for translation (à la gettext).","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"These strings are converted to Strref (numeric string references) on the fly when assigned to relevant parts of game structures (for example as object properties or dialog texts). Translations for the strings are loaded from .po files (in the standard format used by gettext, with tiny adaptations for gendered languages), loaded by the load_translations! function","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"load_translations!","category":"page"},{"location":"","page":"InfinityEngine.jl","title":"InfinityEngine.jl","text":"and saved in all dialog.tlk and dialogF.tlk files. For strings for which no translation is provided, the string itself will be used instead.","category":"page"},{"location":"items/#Items","page":"Items","title":"Items","text":"","category":"section"},{"location":"items/","page":"Items","title":"Items","text":"item\nitems","category":"page"},{"location":"items/#InfinityEngine.item","page":"Items","title":"InfinityEngine.item","text":"item([game], ref)\n\nReturns the item with given reference.\n\n\n\n\n\n","category":"function"},{"location":"items/#InfinityEngine.items","page":"Items","title":"InfinityEngine.items","text":"items([game], [regex])\n\nReturns an iterator over all items. If a regex is provided then only those items with matching reference are included.\n\n\n\n\n\n","category":"function"}]
}