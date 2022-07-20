#= TODO:
 - use a global db for symbolic names of files (to solve namespace conflicts)
 - transition to extern state: Weidu EXTERN [if_file_exists] "file" label
  => Extern("file", label)
 - journal: journal(text), journal(:unsolved, "text"), journal(:solved, "text")
 - transition without text: transition()
 - multi-text state (should create a default transition)
 - if two consecutive say(), then this builds a chain
   -equivalently, say("text1", "text2", ...)
 - chain():
CHAIN ~G3BEV~ LadyBevKelseyExchange @11117 = @11232
== ~J#KLSYJ~ @11233
== ~G3BEV~ @11112
  COPY_TRANS G3BEV KelseyNotAtFault
becomes:
say("text1", "text2")
change("kelsey")
say("text3")
change("g3bev")
say("text4")


This could become global:
dialog("kelsey") do

end

 => translated as:
dialog() do
actor("kelsey") etc.
end
=#
"""
# State machine description:

## States and transitions

A dialogue is a set of states and transitions (+ metadata).
The low-level description looks like this:

    state(...)
    transition(...)
    transition(...)
    state(...)

All the `transition` declarations create transitions from the current state,
i.e. the last state created. (It is illegal to start by a transition).

## States 
### Type of states

Each state corresponds to a given actor (NPC).
Thus, there is a canonical function {states} -> {actors}.
In computer terms, the state is represented as a tuple
`(actor, label)::Tuple{String,Any}`.

*TODO: insert some form of namespaces. Add an extra string? Also allow using other namespaces as needed.*

### Creating states

 * Basic form: `state(actor, label => text)`

 * Chained states: `state(actor, label1 => text1, label2 => text2, ...)`

This actually creates several states linked by a single (implicit)
transition chain. (This naturally specializes to form 1).
All these share the same actor.
 
Any of the labels may be omitted; in this case, implicit state labels
will be generated, and the corresponding nodes will not be reachable from
outside the chain.

It is illegal to re-use a `(actor, label)` pair.

### Current actor

To avoid repeating `actor` variables too often, a global current actor is introduced. This value is set by

    change(actor)

and used by the shortcut

    say(label => text...) # understood as state(current_actor, label => text...)

In particular, this means that chains can also be written as `say()` calls.

### Appending to previous state

    change(actor, label)

This resets the “current state” global variable to this one.
It is then possible to insert new transitions for this state.

## Transitions

### Creating transitions

Transitions are created relatively to the last created state by the call:

    transition(actor, label, transition data...)
    # or equivalently:
    answer(text => (actor, label))

Here `text` is said by the PC.

TODO: define transition flags.

### Transition targets

The target of a transition is a state. For the `answer` form this is indicated
as either a `(actor, label)` form or as a single label; in this case,
the current actor (see above) is used.

The special target `exit` means that the conversation ends there:

    answer(text => exit)

### Implicit transitions

If two states are created successively without any transition,
then an implicit transition between them is inserted:

    state(actor1, label1 => text1)
    # implicitly inserted: `transition(actor2, label2, nothing)`
    state(actor2, label2 => text2)

### Chains

A conversation chain can be built using implicit transitions:

    change(actor1)
    say(text1, text2, text3)
    change(actor2)
    say(text4, text5) # etc.

## Attaching metadata

### Conditions

Conditions define when a given state or transition may or may not be visible
from the game. The default condition, `""` (empty string), is always true.

The `state` and `transition` functions accept `condition` keyword argument
allowing to declare a non-empty condition.

As a shortcut, the `condition()` function call sets the “current condition”,
which will be used by the next `say()` or `answer()` call.

    condition("is(ELF)")
    say("Hello, elven friend!")

### Actions

Transitions (not states) may have actions attached.
These are either passed to the `transition()` function via the `action`
keyword argument, or later via the `action()` function.

The `action()` function modifies the action of the last transition defined:

    answer("then fight me!" => exit)
    action("Enemy()")

!!! warning

    Note that conditions are declared *before* states and transitions,
    while actions are declared *after* them. This follows both WeiDU's
    convention (we aim for simple syntactic transformation) and in-game
    order of evaluation.

### On-the-fly substitution

When an {action,condition,string} is entered, its value is stored in a table
(global for strings, per-actor for actions/conditions). Duplicates are merged.
The state machine structure will use the index into this table for its internal representation.

State labels need to remain in their natural form: they are globally unique
(so a table would gain nothing by merging) and need to remain explicit
(so that later transitions could point to existing labels).

## Compilation

### Intermediate form

All the `state()`, `transitions()` etc. calls build a (global)
intermediate form for the dialog, as a (potentially big) state machine.

When several mods are loaded they will all append to the same state machine.
Namespaces prevent bad mod interactions (but still allow on-purpose access
to other parts of the state machine).

The state machine is stored as an intermediate representation
in which all indices remain in their symbolic form, allowing future growth.

### `dialog()` call

XXX: what is this useful for exactly? returning a (non-global) value for detecting interaction?

A syntactic shortcut is provided, using Julia's `do..end` notation:

    dialog() do
     change("actor1")
     say(label1 => "text1")
     answer("good bye" => exit)
    end

This will append the `compile()` call after the contents of the `do`
block is executed.

As a shortcut (since an initial actor always needs to be declared),
`dialog(actor) do...` is equivalent to `dialog() do change(actor)...`.

### `compile()` call

Once the state machine is fully defined the `compile()` call reduces it to
a canonic form and merges it with the existing `.dlg` dialogue.


### Compiled form

 - Extracting `.dlg` state machine:

For this machine the states are canonically labeled by `("resref", index)`
where the index is the state ordering in the `.dlg` file.

 - Canonicalizing text:

All labels are replaced by `(actor, unique_integer_index)`,
where the index corresponds to the `.dlg` index table
(i.e. new indices are placed after the existing `.dlg` indices).
Original indices are marked in a special way (e.g. DlgIndex(i)).

Actions and conditions are replaced by `(actor, integer_index)`
(duplicates for the same actor are merged; this is actually done on-the-fly).

All strings are replaced by `(integer_index)`
(duplicates are globally merged; this is actually done on-the-fly).
Based on the language in which this dialogue is written, all strings are
inserted into strref table (duplicates merged).

A `.pot` file for strings is also written (for each namespace: this
allows per-mod translation).

 - Output

For each language, the string table is updated with the translated strings
(if they exist) or the original ones (if not).

### Modifying existing dialogue

The intermedia

"""
module Dialogs

# Collector ««1
# Collects value of a given type, building an index system for these
# values along the way.
struct Collector{T}
	index::Dict{T,Int}
	Collector{T}() where{T} = new(Dict{T,Int}())
end
@inline Base.convert(T::Type{<:Collector}, ::Tuple{}) = T()
@inline Base.length(c::Collector) = length(c.index)
@inline Base.empty!(c::Collector) = empty!(c.index)
@inline Base.getindex(c::Collector, s) = getindex(c.index, s)
function Base.push!(c::Collector, s; unique=false)
	i = get(c.index, s, nothing)
	isnothing(i) && return c.index[s] = length(c.index) # zero-based indexing...
	unique && error("repeated index: $s")
	return i
end
function Base.collect(c::Collector)
	list = Vector{keytype(c.index)}(undef, length(c.index))
	for (k, v) in pairs(c.index)
		list[v+1] = k
	end
	return list
end
# »»

const trigger_warning = """
!!! note

    Since we are mimicking Weidu's syntax,
    triggers come *before* states and transitions,
		while actions come *after* transitions.
"""
"""    trigger(text)
Prepends a trigger to the next `state` or `answer`.

$trigger_warning
"""
function trigger(text::AbstractString)
	@assert global_status.trigger == nothing "top-level trigger defined twice"
	global_status.trigger = text
end
function get_current_trigger(trigger)
	if isnothing(trigger)
		r = something(global_status.trigger, "")
	else
		@assert global_status.trigger == nothing "passed trigger argument when a top-level current trigger exists"
		r = trigger
	end
	global global_status.trigger = nothing
	return r
end

function active_transition()
	@assert !isempty(global_status.states) "No states or transitions are defined at this point"
	s = last(global_status.states)
	@assert s.ntransitions > 0 "No transitions defined for current state at this point"
	return global_status.transitions[s.firsttransition + s.ntransitions - 1]
end
"""    action(text)

Appends an action to the last transition.

It is an error to call this when no transition is active
(i.e. either when no state is defined, or just after a `say()` call).

$trigger_warning
"""
function action(text::AbstractString)
	tr = active_transition()
	@assert isempty(tr.action) "Attempted to attach two actions to the same transition"
	tr.action = text
end
"""    State

Holds data for one state of the dialog: text, trigger (if any),
transitions.
"""
mutable struct State
	text::String
	trigger::String
	firsttransition::Int32
	ntransitions::Int32
	State(text::AbstractString, trigger::AbstractString = "", ft = 1) =
		new(text, trigger, ft, 0)
end

"""    say(label, text; [trigger])

Defines a state of the dialog with associated text.

If `trigger()` was called before `say()` then this defines the trigger
conditioning this state; otherwise a trigger may be defined using the
keyword syntax. (Doing both raises an error).
"""
function say(x::X, text::AbstractString; trigger = nothing) where{X}
	s = State(text, get_current_trigger(trigger), 1+length(global_status.transitions))
	push!(global_status.state_index, x; unique=true)
	push!(global_status.states, s)
	@assert length(global_status.state_index) == length(global_status.states)
end

mutable struct Transition{X}
	flags::UInt32
	answer::String
	journal::String
	action::String
	trigger::String
	target::X
	@inline Transition{X}(text::AbstractString, target;
		journal = "", action="", trigger="", flags=0) where{X} =
		new{X}(flags, text, journal, action, trigger, target)
end
@inline Transition(text, target::X; kwargs...) where{X} =
	Transition{X}(text, target; kwargs...)

settarget(t::Transition, x) =
	Transition(t.answer, x; t.journal, t.action, t.trigger, t.flags)


"""    answer(text => label; [trigger = text])

Attaches a transition to the previously defined `say()`.

The label must either be:
 - a label (of any type) attached to a state (possibly a state defined
   later; this will only be checked when the dialog is compiled);
 - or the keyword `exit`, meanin that this transition ends the dialogue.

It is an error to call this function without having defined a state.

A trigger may be attached to this transition, using either the keyword
or a previous `trigger()` invocation (doing both is an error).
"""
function answer((text, x)::Pair{<:AbstractString}; trigger = nothing, kwargs...)
	@assert !isempty(global_status.states) "Impossible to define a transition without a state"
	trigger = get_current_trigger(trigger)
	a = Transition{Any}(text, x; trigger, kwargs...)
	push!(global_status.transitions, a)
	last(global_status.states).ntransitions+= 1
end

mutable struct GlobalStatus
	trigger::Union{Nothing,String} # last created trigger
	actor::Union{Nothing,String}
	state_index::Collector{Any}
	states::Vector{State} # all created states; “active” state is last
	transitions::Vector{Transition{Any}} # all created transitions
	@inline GlobalStatus() = reset(new())
end
@inline function Base.reset(g::GlobalStatus)
	g.trigger = nothing
	g.state_index = ()
	g.states = []
	g.transitions = []
	return g
end
const global_status = GlobalStatus()

function change(name)
	global_status.actor = name
end

"""    done()

This compiles all previous `say()`, `answer()`, etc. invocations
into a self-contained `Dialogue` object.
"""
function done()
	action_coll = Collector{String}()
	trigger_coll = Collector{String}()
	for (i,s) in pairs(global_status.states)
		println("state \e[1m<$i\e[m> \"$(s.text)\":")
		if !isempty(s.trigger)
			i = push!(trigger_coll, s.trigger)
			println("  trigger $i = \e[33m$(s.trigger)\e[m")
		end
		r = s.firsttransition .+ (0:s.ntransitions-1)
		println("  transitions $r")
		for i in r
			t = global_status.transitions[i]
			if t.target == exit
				g = "(final)"
			else
				g = "\e[1m<"*string(global_status.state_index[t.target])*">\e[m"
			end
			println("  $i: \"$(t.answer) => $g")
			if !isempty(t.action)
				i = push!(action_coll, t.action)
				println("  action $i = \e[32m$(t.action)\e[m")
			end
		end
	end
	reset(global_status)
	return nothing
end

function dialog(f, actor::AbstractString)
	reset(global_status)
# 	change(actor)
	f()
end
function dialog(f)
	reset(global_status)
	f()
end

export say, answer, trigger, action, done, dialog

#»»1
end
D = Dialogs
for f in names(D); f == nameof(D) && continue
	eval(:(@inline $f(args...; kwargs...) = D.$f(args...; kwargs...)))
end
# @inline say(args...; kwargs...) = D.say(args...; kwargs...)
# @inline trigger(args...; kwargs...) = D.trigger(args...; kwargs...)

dialog("blah") do
trigger("at night")
say(:begin, "a beautiful night")
 answer("indeed" => 3)
 answer("again?" => :begin)
 answer("good bye" => exit)
  action("continue script here")

say(3, "the stars are aligned")
 answer("farewell" => exit)
  action("continue script here")
end


# better would be:
# x = say("a beautiful night")
# x: "
#
