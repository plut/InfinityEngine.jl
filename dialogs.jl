#= TODO:
# annotate strings with language
 State machine description:
  + state()
	 + actor()
	 + say()
	+ transition()
	 + answer()
	 + implicit transitions
   - transition without text: transition()
	+ trigger()
	 - FIXME: should we rename this condition()?
  - priority() (is this useful? check if it is used at all in WeiDU examples)
	+ journal()
	 * TODO first: load state machine from .dlg resource
	 * check in examples about solved/unsolved etc.
	+ action)
	 * check about strrefs in action code!
	 - define a Julia syntax for action code?
 - Editing existing entries
  - define edition actions (defer to end once everything is loaded ?)
	 - MUCH easier to load state machine first...
	- primary need = interjections
	- main edit types:
	 - edit state:
	  - insert transition from existing state: almost possible w/ curr. syntax
state(actor, label) # loads existing state XXX ensure merging!
transition(...) # inserts transition; TODO: set insertion point
    - select transition for change
    - delete transition (not very useful)
    - change state payload
   - edit transition:
	  - change target
		- change payload (already mostly possible)

 - Compilation
  - importing .dlg database
	- merging databases
	 - canonicalize state keys
	- exporting
 - Nice stuff
  - detection of too-long text
	- detection of no-exit situations

 - use a global db for symbolic names of files (to solve namespace conflicts)
 - journal: journal(text), journal(:unsolved, "text"), journal(:solved, "text")
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

    actor("Bob")

and used by the shortcut

    say(label => text...) # understood as state(current_actor, label => text...)

In particular, this means that chains can also be written as `say()` calls.

### Appending to previous state

    actor("Bob", label)

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

All keys are replaced by `(actor, unique_integer_index)`,
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

module Collectors
"""    Collector{T,unique?}

Represents a searchable, ordered, mutable list of values of type `T`,
supporting the following operations:
 - value lookup: `T` -> integer
 - index lookup: integer -> `T`
 - value swapping
 - value appending (at end of list)
Collects values of type `T`, assigning them sequential indices.
If `unique?` is true then only unique values are allowed, otherwise
duplicates are merged.

    push!(collector, key)

Appends a new key and returns the corresponding index.

    collect(collector)

Returns the sequence of values collected, i.e. a `list` of keys
such that `list[collector[key]] == key`.
"""
struct Collector{T,I<:Integer} <: AbstractDict{T,I}
	index::Dict{T,I}

	Collector{T,I}() where{T,I<:Integer} = new(Dict{T,I}())

	# default constructor for functoriality
	Collector{T,I}(index::Dict{T,I}) where{T,I} = new{T,I}(index)
end
Collector{T}(args...) where{T} = Collector{T,Int}(args...)
(C::Type{<:Collector{T}})(args::T...) where{T} = (c = C(); push!(c, args...); c)
Collector{T,I}(::Tuple{}) where{T,I} = Collector{T,I}()

# @inline Base.valtype(c::Collector{T,I}) where{T,I} = I
@inline Base.convert(T::Type{<:Collector}, ::Tuple{}) = T()
for f in (:length, :empty!, :isempty, :haskey, :getindex, :get,
	:pairs, :iterate, :setindex!)
	@eval Base.$f(c::Collector, args...) = $f(c.index, args...)
end
function Base.push!(c::Collector, s; unique = false)
	unique && haskey(c.index, s) && error("repeated index: $s")
	get!(c.index, s, valtype(c.index)(1+length(c.index)))
end
Base.push!(c::Collector, s...; kw...) = [push!(c, x; kw...) for x ∈ s]
function Base.collect(c::Collector)
	list = Vector{keytype(c.index)}(undef, length(c.index))
	for (k, v) in pairs(c.index)
		list[v] = k
	end
	return list
end
function Base.get(f::Function, c::Collector, x)
	i = get(c, x, nothing)
	!isnothing(i) && f(i)
end
function Base.findfirst(c::Collector, i::Integer)
	for (k, j) in pairs(c); i == j && return k; end
end

export Collector
end
using .Collectors

# ««1 State machine
struct State{I,S}
	transitions::Vector{I}
	data::S
end
mutable struct Transition{K,T}
	target::K
	data::T
end
"""    StateMachine{I,S,T,K}

`I` is index type, `S` is state payload, `T` is transition payload,

States are indexed by keys of type `K`. Transitions are indexed
by the state.
"""
struct StateMachine{I,S,T,K}
	states::Vector{State{I,S}}
	transitions::Vector{Transition{K,T}}
	# the “proper” way to be able to compile out the keys would be to
	# instead use a C <: AbstractDict{K,I} type, with the option to use
	# either Collector or some trivial collector implementing identity map
	# (+ count number of entries — to be able to list all the keys).
	#
	# also: reorder states? (harder, we need to build a list of *incoming*
	# transitions)
	keys::Collector{K,I}
	StateMachine{I,S,T,K}() where{I,S,T,K} = new{I,S,T,K}([], [], ())
end

indextype(::StateMachine{I}) where{I} = I
statedata(m::StateMachine{I,S}) where{I,S} = S
transitiondata(m::StateMachine{I,S,T}) where{I,S,T} = T
Base.keytype(::StateMachine{I,S,T,K}) where{I,S,T,K} = K
statetype(m::StateMachine) = eltype(m.states)
transitiontype(m::StateMachine) = eltype(m.transitions)

state(m::StateMachine, key) = m.states[m.keys[key]]
Base.keys(m::StateMachine) = collect(m.keys)

function add_state!(m::StateMachine, key, data)
	s = statetype(m)(transitiontype(m)[], data)
	i = push!(m.keys, key; unique=true)
	push!(m.states, s)
	@assert i == length(m.states) # invariant
	return s
end

function add_transition!(m::StateMachine, source, target, data, position)
	# source is a state, target is a key
	t = transitiontype(m)(target, data)
	push!(m.transitions, t)
	insert!(source.transitions, position, length(m.transitions))
	return t
end

#««1 Interpretation context

"structure translating all numerical info held in the machine to string values"
struct Context{I<:Integer,S,T}
	strings::Collector{S,I}
	actions::Collector{T,I}
	state_triggers::Collector{T,I}
	transition_triggers::Collector{T,I}
	languages::Collector{String,I}

	# XXX default constructor is needed for functoriality?
	Context{I,S,T}() where{I<:Integer,S,T} = new{I,S,T}((), (), (), (), ())
end
Base.empty!(c::Context) = for f in fieldnames(typeof(c))
	empty!(getfield(c, f))
end
# ««2 Indexing
indextype(::Context{I,S}) where{I,S} = I
stringtype(::Context{I,S}) where{I,S} = S
actiontype(::Context{I,S,T}) where{I,S,T} = T

# instead of doing lots of Union{Nothing,...}, we simply
# mark invalid keys by 0xffff, which is how they will eventually be
# written in the file anyway:
isindex(i::Integer) = !iszero(~i)
noindex(i) = ~zero(i)
noindex(c::Context) = noindex(indextype(c))
# special case useful for handling '::Nothing' kwargs:
Base.push!(c::Collector, ::Nothing) = noindex(valtype(c))

#««1 State machine builder: mutable information while machine is being built

"pointers to current status of machine being built, i.e. all data which
becomes useless once the build is complete"
mutable struct Builder{I<:Integer}
	state::I
	transition::I
	language::I
	namespace::Union{Nothing,String}
	actor::Union{Nothing,String}
	trigger::Union{Nothing,String}
	pending_transition::Bool
	Builder{I}() where{I} = new{I}(0, 0, 0, nothing, nothing, nothing, false)
end

function add_state!(b::Builder, m::StateMachine, key, args...)
	println("add node $key")
	b.pending_transition && println("\e[32m  solve pending transition to $key\e[m")
	b.pending_transition &&
		(current_transition(b, m).target = key; b.pending_transition = false)
	add_state!(m, key, args...);
	b.state = length(m.states)
end

function add_transition!(b::Builder, m::StateMachine, source, target,
		data, position; pending = false)
	@assert !b.pending_transition "unsolved pending transition"
	if pending
		println("  \e[31madd pending transition\e[m")
	else
		println("  add transition to $target")
	end
	add_transition!(m, source, target, data, position)
	b.transition = position
	b.pending_transition = pending
end

current_state(b::Builder, m::StateMachine) =
	(@assert !iszero(b.state); m.states[b.state])
if_current_state(f::Function, b::Builder, m::StateMachine) =
	!iszero(b.state) && f(m.states[b.state])

current_transition(b::Builder, m::StateMachine) = let s = current_state(b, m)
	t = b.transition
	m.transitions[s.transitions[t]]
end
if_current_transition(f::Function, b::Builder, m::StateMachine) =
	if_current_state(b, m) do s
	t = b.transition
	!iszero(t) && f(m.transitions[s.transitions[t]])
end

#««2 Language indexing
string_idx(::Builder, c::Context, ::Nothing) = noindex(c)
string_idx(b::Builder, c::Context, s::AbstractString) =
	(@assert isindex(b.language); push!(c.strings, (b.language, s)))

# ««2 Trigger handling: this is actually a push queue with a max length of 1
"sets the current trigger value; only allowed if it is not set yet"
trigger!(b::Builder, str::AbstractString) =
	(@assert isnothing(b.trigger); b.trigger = str)
trigger!(b::Builder, ::Nothing) = nothing
"gets (and then erases) the current trigger value"
trigger(b::Builder) = (x = b.trigger; b.trigger = nothing; return x)
"gets a trigger value, from either supplied x, or current trigger"
get_trigger(b::Builder, x) = (trigger!(b, x); return trigger(b))



# ««1 Data fields for the global machine
# ««2 State keys
"state key type for the global state machine being built"
struct StateKey{X}
	namespace::String
	actor::String
	label::X
	@inline StateKey{X}(ns::AbstractString, a::AbstractString, l::X) where{X} =
		new{X}(ns, a, l)
end
@inline (T::Type{<:StateKey})(b::Builder, c::Context, namespace::AbstractString,
	actor::AbstractString, label) = T(namespace, actor, label)
# special case — exit transition keys are canonicalized:
@inline (T::Type{<:StateKey})(b::Builder, c::Context, ::AbstractString,
	::AbstractString, ::typeof(exit)) = T("", "", exit)
@inline (T::Type{<:StateKey})(b::Builder, c::Context, actor::AbstractString,
		label) = T(b, c, b.namespace, actor, label)
@inline (T::Type{<:StateKey})(b::Builder, c::Context, label) =
	T(b, c, b.actor, label)

# ««2 State data
"state data type for the global state machine being built"
mutable struct StateData{I<:Integer}
	priority::Float32
	# Payload: (we could also turn all of these into a type parameter)
	text::I
	trigger::I
	StateData{I}(; text=~zero(I), trigger=~zero(I), priority=-eps(Float32)
		) where{I<:Integer}= new{I}(priority, text, trigger)
end
@inline function (T::Type{<:StateData})(b::Builder, c::Context;
		text, trigger = nothing, kw...) # kw = priority
	text = string_idx(b, c, text)
	trigger = push!(c.state_triggers, get_trigger(b, trigger))
	return T(; text, trigger, kw...)
end
" lowest-level state creating function. context resolves triggers and text;
we build the state data; builder bookkeeps current state."
@inline function add_state!(b::Builder, c::Context, m::StateMachine, key; kw...)
	key::keytype(m)
	add_state!(b, m, key, statedata(m)(b, c; kw...))
end


# ««2 Transition flags
"module holding syntactic sugar for transition flags"
module Flags
	set(; text = false, trigger = false, action = false, terminates = false,
		journal = false, interrupt = false, unsolved = false, journalentry = false,
		solved = false) =
		UInt32(text<<0 | trigger<<1 | action<<2 | terminates<<3 | journal << 4 |
			interrupt <<5 | unsolved << 6 | journalentry << 7 | solved << 8)
	Base.contains(x, flag::Symbol) =
		!iszero(x & set(; NamedTuple{(flag,),Tuple{Bool}}((true,))...))
	function string(x; default="0") # not importing Base!
		kw = Base.kwarg_decl(first(methods(set).ms))
		iszero(x) ? default :
		join((Base.string.(k) for k in kw if contains(x, k)), '|')
	end
end


# ««2 Transition data
"transition data type for the global state machine being built"
mutable struct TransitionData{I<:Integer}
	text::I
	journal::I
	trigger::I
	action::I
	flags::UInt32
	function TransitionData{I}(; text, journal, trigger, action,
			flags = nothing, kwargs...) where{I<:Integer}
		flags = something(flags, Flags.set(;
			text = isindex(text), journal = isindex(journal),
			action = isindex(action), trigger = isindex(trigger), kwargs...))
		return new{I}(text, journal, trigger, action, flags)
	end
end

@inline function (T::Type{<:TransitionData})(b::Builder, c::Context;
		text::Union{Nothing,AbstractString} = nothing,
		journal::Union{Nothing,AbstractString} = nothing,
		action::Union{Nothing,AbstractString} = nothing,
		trigger::Union{Nothing,AbstractString} = nothing,
		kwargs...) # passed to TransitionData (and hence to Flags)
	text = string_idx(b, c, text)
	journal = string_idx(b, c, journal)
	action = push!(c.actions, action)
	trigger = push!(c.transition_triggers, get_trigger(b, trigger))
	return T(; text, journal, trigger, action, kwargs...)
end
"low-level transition adding function. context resolves triggers, text and journal; we build the transition data; builder inserts and bookkeeps."
@inline function add_transition!(b::Builder, c::Context, m::StateMachine,
		source, target; position::Integer = 1+length(source.transitions),
		pending = false, kwargs...)
	source::statetype(m)
	target::keytype(m)
	data = transitiondata(m)(b, c; kwargs...)
	add_transition!(b, m, source, target, data, position; pending)
end

# ««1 All code manipulating global data goes here
function init(I::Type{<:Integer}, S::DataType, T::DataType, X::DataType)
	SK = StateKey{X}
	SD = StateData{I}
	TD = TransitionData{I}
	SM = StateMachine{I,SD,TD,SK}
	# strings are labelled with their language: (language, string)
	# (language slightly expanded to also define PC gender)
	global builder = Builder{I}()
	global machine = SM()
	global context = Context{I,Tuple{I,S},T}()
end
#««2 Misc.: namespace, actor, language, trigger

@inline namespace(text) = (builder.namespace = text)
@inline actor(text) = (builder.actor = text)
@inline language(text) = (builder.language = push!(context.languages, text))
@inline trigger(str::AbstractString) = trigger!(builder, str)

function action(str::AbstractString; override=false)
	t = current_transition(builder, machine)
	@assert(override || !contains(t.data.flags, :action),
		"action already defined for this transition")
	t.data.flags |= Flags.set(action = true)
	t.data.action = push!(context.actions, str)
end
function journal(str::AbstractString; override=false)
	t = current_transition(builder, machine)
	@assert(override || !contains(t.data.flags, :journal),
		"journal entry already defined for this transition")
	t.data.flags |= Flags.set(journal = true)
	t.data.journal = string_idx(builder, context, str)
end

#««2 States
"""
    state(namespace, actor, label; text)
    state(actor, label; text) # adds a state with text
    state(label; text) # idem (shorthand)
    state(actor, label) # moves to this label (to edit it)
    state(label)
    say(args...)

where each `say` argument is either:

    (actor, label) => text
    label => text
    text

If more than one state is given then they are linked by implicit
transitions.

The multi-state form is equivalent to consecutive invocations of `state`.
"""
function state(namespace::AbstractString, actor::AbstractString, label;
		text::Union{Nothing,AbstractString} = nothing, kw...)
	key = keytype(machine)(builder, context, namespace, actor, label)
	if_current_state(builder, machine) do s
		if isempty(s.transitions)
			println("  implicit transition needed")
# 			transition()
			add_transition!(builder, context, machine, s, key)
		end
	end
	state(key, text; kw...)
end
@inline state(actor::AbstractString, label; kw...) =
	state(builder.namespace, actor, label; kw...)
@inline state(label; kw...) = state(builder.actor, label; kw...)

function state(k::StateKey, ::Nothing; override = false,
		trigger::Union{Nothing,AbstractString} = nothing)
	error("todo: move current_state around")
end
@inline state(k::StateKey, text::AbstractString; kw...) =
	add_state!(builder, context, machine, k; text, kw...)

# @inline state(actor::AbstractString, args::SayText...; kwargs...) =
# 	for a in args; state(actor, a; kwargs...); end
# @inline state(::Nothing, args...; kw...) = error("no current actor defined")
# 
"""    say([label =>] text...; trigger)

Creates one (or more) state(s). The state keys are
`(current_namespace, current_actor, label)`.
In other words this is equivalent to (and defined as)
`state(current_actor, [label =>] text...)`.
Labels are automatically generated (but unreachable) if not provided.
"""
@inline say((key, text)::Pair{<:Any,<:AbstractString}; kw...) =
	say2(text, key; kw...)
@inline say(text::AbstractString; kw...) = say(gensym() => text; kw...)
@inline say(args::Union{AbstractString,Pair{<:Any,<:AbstractString}}...;kw...)=
	say.(args; kw...)
@inline say2(text, key::Tuple; kw...) = state(key...; text, kw...)
@inline say2(text, key; kw...) = state(key; text, kw...)


#««2 Transitions
"""
    transition(namespace, actor, label; text...)
    transition(actor, label; text...)
    transition(label; text...)
    transition(exit) # special case of the previous
    transition() # not needed: this is the implicit transition!

    answer(text => (namespace, actor, label))
    answer(text => (actor, label))
    answer(text => label)
    answer(text => exit) # special case of previous
    answer(text)

Special considerations:
 - an exit key sets the `terminates` flag.
 - `answer` always has text, transition (in general) does not.
 - if no label is provied then the transition will point to the next state.
"""
@inline function transition(namespace::AbstractString, actor::AbstractString,
	label; kw...)
	key = keytype(machine)(builder, context, namespace, actor, label)
	add_transition!(builder, context, machine, current_state(builder, machine),
		key; terminates = (key.label == exit), kw...)
end
@inline transition(actor::AbstractString, label; kw...) =
	transition(builder.namespace, actor, label; kw...)
@inline transition(label; kw...) =
	transition(builder.namespace, builder.actor, label; kw...)
@inline transition(; kw...) = transition(undef; pending = true, kw...)

@inline answer((text, k)::Pair{<:AbstractString,<:Tuple}; kw...) =
	transition(k...; text, kw...)
@inline answer((text, label)::Pair{<:AbstractString}; kw...) =
	transition(label; text, kw...)
@inline answer(text::AbstractString; kw...) = transition(; text, kw...)

# ««1 (disabled for now) printing
Base.show(io::IO, k::StateKey) =
	print(io, "\"", k.namespace, "/", k.actor, "\"", k.label|>repr, "")

function printmachine(io::IO, m::StateMachine;
		printstate = nothing, printtransition = nothing, printkey = nothing)
	keylist = m.keys|>collect
	for (i,k) in keylist|>pairs
		!isnothing(printkey) && printkey(io, i, k)
	end
	println(io)
	for (i, s) in pairs(m.states)
		println(io, "state \e[1m<", keylist[i], '=', i, ">:\e[m")
		!isnothing(printstate) && printstate(io, s.data)
		for j in s.transitions
			t = m.transitions[j]
			if t.target.label == exit
				print(io, "  \e[7m(final)\e[m")
			else
				print(io, " `-> \e[38;5;7m<", t.target, haskey(m.keys, t.target) ?
					"="*string(m.keys[t.target]) : " \e[7mnot found\e[m", ">\e[m")
			end
			!isnothing(printtransition) && printtransition(io, t.data)
		end
	end
end

@inline printkey(io::IO, i, k) = println(io, i, '=', k)
function printstate(io::IO, c::Context, s::StateData)
	isindex(s.trigger) && println(io, "trigger=\e[33m",
		findfirst(c.state_triggers,s.trigger)|>repr, "\e[m")
	isindex(s.text) && println(io, "\e[34m",
		findfirst(c.strings, s.text)|>repr, "\e[m")
end
@inline printstate() = (io::IO, s) -> printstate(io, context, s)

function printtransition(io::IO, c::Context, t::TransitionData)
	!iszero(t.flags) && print(io, "  flags=", Flags.string(t.flags))
	println(io)
	isindex(t.trigger) && println(io, "trigger=\e[33m",
		findfirst(c.transition_triggers, t.trigger)|>repr, "\e[m")
	isindex(t.text) && println(io, "  \e[36m",
		findfirst(c.strings, t.text)|>repr, "  \e[m")
	isindex(t.action) && println(io, "  action=\e[31m",
		findfirst(c.actions, t.action)|>repr, "\e[m")
	isindex(t.journal) && println(io, "  \e[35m",
		findfirst(c.strings, t.journal)|>repr, "  \e[m")
end
@inline printtransition() = (io::IO, t) -> printtransition(io, context, t)
Base.show(io::IO, ::MIME"text/plain", m::StateMachine) =
	printmachine(io, m; printkey = printkey)

@inline printmachine() =
	printmachine(stdout, machine;
		printkey = printkey, printstate = printstate(),
		printtransition = printtransition())

# »»1
export namespace, actor, trigger, action, journal, language
export state, say, transition, answer
end
D = Dialogs
for f in names(D); f == nameof(D) && continue
	eval(:(@inline $f(args...; kwargs...) = D.$f(args...; kwargs...)))
end

Dialogs.init(Int32, String, String, Any)

language("en")
namespace("main")
actor("Alice")
trigger("WEATHER(NICE)")
say(:toto => "this morning...")
say(:hello => "weather is nice today", "sunny and all!")
  answer("bye" => exit)
		action("QUIT()")
say(:hello2 => "it rains.. again", "but tomorrow it will be sunny"; priority=-1)
	answer("let's hope so" => :hello)
	answer("what does B say about this?" => ("Bob", :hello); position=1)
	  journal("Today I asked a question to Bob")
	transition(exit)
actor("Bob")
say(:hello => "I am Bob!!!")
	transition(exit)

D.printmachine()
