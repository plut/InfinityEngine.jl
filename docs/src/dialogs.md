# Dialogs


## The structure of IE dialogs

IE dialog is encoded as a state machine
(a single state machine can represent the whole game dialog).
States of this machine correspond to NPC text,
while transitions are (usually) PC replies.

On a given state (when presented when a text), the PC can choose
which reply to select, i.e. which transition to follow to a target state.
Some transitions are final (they close the dialog instead of
having a target state), and some transitions also happen to
not have PC text associated.

Since each state has a NPC speaker, states are indexed by pairs
`(actor, key)`, where the actor is the identity of the speaker;
this is also how dialog is encoded in the game files:
all dialog belonging to a single actor (states of this actor, and
transitions from these states) are stored in a single file.

## Displaying dialogs

Reading an actor is as simple as loading it and letting the REPL display
it: just try invoking `actor("imoen")` and look at the result.
```@docs
actor
```

## Creating new dialogs

Several functions enable creating and editing game dialogs.
The easiest feature is creation of entirely new dialogs.
For this, it is enough to give a list of states
connected by transitions.

The currently active actor is selected by the `actor` function.
It is legal to call this on a non-existing actor name;
it simply creates an empty actor, to which states and transitions
will then be attached.

The `say` function creates states, while `reply` creates transitions.
The module maintains a “last added state” variable.
This variable is updated by calls to `say` to the last state added,
and used by calls to `reply` to determine from which state a transition
must be created.
```@docs
say
reply
```

### State labels

Numeric state keys correspond in principle to original game dialog.
New labels should use strings.

Label strings are namespaced to avoid collisions.

Internally, a numeric label is produced by hashing the strings
(to 64-bit integers). This allows adding approximately
2³² states (4 billion) to any actor before risking a key
collision, and approximately 2⁵⁰ states before hitting any target
lower than 16000 (where original game labels presumably reside).

### Final transitions

### Chaining and implicit transitions

### Pending transitions

The implicit transitions created by chaining are all text-less
transitions. If PC comments are needed, this can be done via pending
transitions.

A pending transition is a transition with no target state indicated.
The transition will be connected to the next state to be added.
Inserting such transitions in the middle of a `say` chain has the effect
of inserting PC text while maintaining the structure of the chain:
for example,
`say(A); reply(a); say(B)` is equivalent to
`say(A); reply(a => labelB); say(labelB => B)`,
without the need to give an explicit label to the target state.

The current pending transition must be connected (by calling `say`
or `interject`, both of which always resolve existing pending transitions)
before any other transition is created (by calling `reply`).

## Extending existing dialogs

The `from` function allows changing the value of the “last added state”
variable to any actor and any state.
Note that this is not always the same actor as the speaking actor
selected by `actor`:
namely, `from` selects a *source* actor (i.e. already existing states),
whereas `actor` selects a *target* actor, for which states will be
inserted.

With a combination of `from`, `actor` and `reply` it is possible
to extend existing states for an actor (by adding new transitions)
and to add new states.
```julia
from("imoen", 0)
reply("Oh, hi Imoen!" => "new state")
say("new state" => "Hi you! Now we go on to our normal conversation.")
```

## Inserting into existing dialogs

New states can also be inserted into an existing transition.
Say that `A` is the last created (source) state,
with transitions `tᵢ` to states `Bᵢ`: `A ——tᵢ——→ Bᵢ`.
The function `interject` can insert a new state `X` at the tail end
of all the arrows `tᵢ`: `A ——→X——tᵢ——→Bᵢ`.
Namely, there is now a single text-less transition from `A` to `X`,
and all the original transitions from `A` now start from state `X`.

```@docs
interject
```

`interject` moves the “source state” pointer to the newly created state
`X`, so that it is possible to chain calls. For example, after `from(A);
interject(X); interject(Y)`, the result will be something like
`A——→X——→Y——tᵢ——→Bᵢ`.

### `interject` and pending transitions

If the source state `A` has a pending transition when `interject` is
called, then instead of creating a new text-less transition `A→X`,
this pending transition is used (and connected) instead.
This allows replacing the default text-less transition `A→X` by a
transition with text `A——x—→X` in the following way:
`from(A); reply(x); interject(X)`.

## Deleting existing dialogs

Not currently possible (and not a very high priority).


## Attaching data to dialogs

**TODO**.

### State priority

### Triggers

### Actions and journal

```@docs
trigger
action
journal
```
