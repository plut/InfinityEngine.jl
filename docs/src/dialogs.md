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

### Implicit transitions

### Pending transitions

## Extending existing dialogs

The `state` function allows changing the value of the “last added state”
variable to any actor and any state.
Note that this is not always the same actor as the speaking actor
selected by `actor`:
namely, `state` selects a *source* actor (i.e. already existing states),
whereas `actor` selects a *target* actor, for which states will be
inserted.

With a combination of `state`, `actor` and `reply` it is possible
to extend existing states for an actor (by adding new transitions)
and to add new states.
```julia
state("imoen", 0)
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
`X`, so that it is possible to chain calls. For example, after `state(A);
interject(X); interject(Y)`, the result will be something like
`A——→X——→Y——tᵢ——→Bᵢ`.

### `interject` and pending transitions

If the source state `A` has a pending transition when `interject` is
called, then instead of creating a new text-less transition `A→X`,
this pending transition is used instead.
This allows replacing the default text-less transition `A→X` by a
transition with text `A——x—→X` in the following way:
`state(A); reply(x); interject(X)`.

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
