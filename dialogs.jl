#= TODO:
 Figure a way to make the global `machine` a constant
 and still export the `say` functions etc.
 “simple” way: once we figure the appropriate type,
 make __init__ call init with really constant types

#  “simple” way: make the machine a constant in InfinityEngine,
#  export the say/answer etc. functions from Dialogs -> InfinityEngine,
#  and overload the *single* method calling the global for each function
#   => maybe even better: make this a method, current_machine() ?
#  or call Dialogs.eval(quote init(...))

 State machine description:
	+ trigger()
	 - FIXME: should we rename this as: condition()?
  - priority() (is this useful? check if it is used at all in WeiDU examples)
	+ journal()
	 * TODO first: load state machine from .dlg resource
	 * check in examples about solved/unsolved etc.
	+ action)
	 - check about strrefs in action code!
	 - define a Julia syntax for action code?
 - Editing existing entries
  - finish writing state() so that it allows pointing to an existing state
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
	i ∈ 1:length(c) || return nothing # O(1) shortcut for frequent case
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

"""    StateMachine{I,S,T,K,C}

`I` is index type, `S` is state payload, `T` is transition payload,

States are indexed by keys of type `K`. Transitions are indexed
by the state.

`C` is context type. This is also used for building state and transition data,
via constructors for the `S` and `T` types:

    S(context; text, trigger, priority)
    T(context; text, journal, action, trigger, flags)

For displaying the following methods need to exist:

    read(context, K) -> (isexit, string)
    read(context, S) -> (text, trigger)
    read(context, T) -> (text, journal, trigger, action, flags)
"""
struct StateMachine{I,S,T,K,C}
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
	# the context structure holds the global data:
	context::C
	# A few mutable fields useful while building the machine:
	current_state::Base.RefValue{I}
	current_transition::Base.RefValue{I}
	pending_transition::Base.RefValue{Bool}

	StateMachine{I,S,T,K,C}() where{I,S,T,K,C} =
		new{I,S,T,K,C}([], [], (), C(), Ref(zero(I)), Ref(zero(I)), Ref(false))
end

indextype(::StateMachine{I}) where{I} = I
statedata(m::StateMachine{I,S}) where{I,S} = S
transitiondata(m::StateMachine{I,S,T}) where{I,S,T} = T
Base.keytype(::StateMachine{I,S,T,K}) where{I,S,T,K} = K
statetype(m::StateMachine) = eltype(m.states)
transitiontype(m::StateMachine) = eltype(m.transitions)

Base.keys(m::StateMachine) = collect(m.keys)

@inline statekey(m::StateMachine, args) =
	keytype(m)(m.context, args...)

"    add_state!(machine, args...; kwargs...)

Builds a state with key defined from args and data from kwargs.
"
function add_state!(m::StateMachine, kargs; kwargs...)
	key = statekey(m, kargs)::keytype(m)
	if m.pending_transition[]
		current_transition(m, m).target = key
		m.pending_transition[] = false
	end
	data = statedata(m)(m.context; kwargs...)::statedata(m)
	s = statetype(m)(transitiontype(m)[], data)
	i = push!(m.keys, key; unique=true)
	push!(m.states, s)
	@assert i == length(m.states) # invariant
	m.current_state[] = length(m.states)
	return s
end
function add_transition!(m::StateMachine, source, ::Nothing, args...; kwargs...)
	println("  \e[31madd pending transition\e[m")
	add_transition(m, source, first(keys(m.keys)); kwargs...)
	m.pending_transition[] = true
end
function add_transition!(m::StateMachine, source, kdata, args...;
		position = 1 + length(source.transitions), kwargs...)
	source::statetype(m)
	target = statekey(m, kdata)::keytype(m)
	@assert !m.pending_transition[] "unsolved pending transition"
	data = transitiondata(m)(m.context, args...; kwargs...)
	t = transitiontype(m)(target, data)
	push!(m.transitions, t)
	insert!(source.transitions, position, length(m.transitions))
	m.current_transition[] = position
	return t
end

current_state(m::StateMachine) =
	(@assert !iszero(m.current_state[]); m.states[m.current_state[]])
if_current_state(f::Function, m::StateMachine) =
	!iszero(m.current_state[]) && f(m.states[m.current_state[]])

current_transition(m::StateMachine) = let s = current_state(m)
	t = m.current_transition[]
	m.transitions[s.transitions[t]]
end
if_current_transition(f::Function, m::StateMachine) =
	if_current_state(m) do s
	t = m.current_transition[]
	!iszero(t) && f(m.transitions[s.transitions[t]])
end

#««1 Test types
# These structures hold the API for interacting with another module:
#««2 Context
mutable struct Context{I,S,T}
	language::I
	namespace::Union{Nothing,String}
	actor::Union{Nothing,String}
	trigger::Union{Nothing,String}

	strings::Collector{S,I}
	actions::Collector{T,I}
	state_triggers::Collector{T,I}
	transition_triggers::Collector{T,I}
	languages::Collector{String,I}

	@inline Context{I,S,T}() where{I,S,T} = new{I,S,T}(zero(I), nothing,
		nothing, nothing, (), (), (), (), ())
end
indextype(::Context{I}) where{I} = I
stringtype(::Context{I,S}) where{I,S} = S
actiontype(::Context{I,S,T}) where{I,S,T} = T

Base.push!(c::Collector, ::Nothing) = zero(valtype(c))

#««2 Context accessors
string_idx(c::Context, ::Nothing) = zero(indextype(c))
string_idx(c::Context, s::AbstractString) =
	(@assert !iszero(c.language); push!(c.strings, (c.language, s)))

@inline namespace(c::Context, str) = (c.namespace = str)
@inline actor(c::Context, str) = (c.actor = str)
@inline language(c::Context, str) = (c.language = push!(c.languages, str))
function action(m::StateMachine, str::AbstractString; override=false)
	t = current_transition(m)
	@assert(override || !contains(t.data.flags, :action),
		"action already defined for this transition")
	t.data.flags |= Flags.set(action = true)
	t.data.action = push!(m.context.actions, str)
end
function journal(m::StateMachine, str::AbstractString; override=false)
	t = current_transition(m)
	@assert(override || !contains(t.data.flags, :journal),
		"journal entry already defined for this transition")
	t.data.flags |= Flags.set(journal = true)
	t.data.journal = string_idx(m.context, str)
end

"sets the current trigger value; only allowed if it is not set yet"
trigger!(c::Context, str::AbstractString) =
	(@assert isnothing(c.trigger); c.trigger = str)
trigger!(c::Context, ::Nothing) = nothing
"gets (and then erases) the current trigger value"
get_trigger(c::Context) = (x = c.trigger; c.trigger = nothing; return x)
"gets a trigger value, from either supplied x, or current trigger"
get_trigger(c::Context, x) = (trigger!(c, x); return get_trigger(c))

get_state_trigger(c::Context, x) =
	push!(c.state_triggers, get_trigger(c, x))
get_transition_trigger(c::Context, x) =
	push!(c.transition_triggers, get_trigger(c, x))

get_string(c::Context, str::AbstractString) =
	(@assert !iszero(c.language); LangString(c.language, str))
# special case: no text
get_string(c::Context, ::Nothing) = LangString(0, "")

# ««2 State keys
struct StateKey{X}
	namespace::String
	actor::String
	label::X
	@inline StateKey{X}(ns::AbstractString, a::AbstractString, l) where{X} =
		new{X}(ns, a, l)
end
@inline (T::Type{<:StateKey})(::Context, namespace::AbstractString,
	actor::AbstractString, label) = T(namespace, actor, label)
@inline (T::Type{<:StateKey})(c::Context, actor::AbstractString, label) =
	T(c, c.namespace, actor, label)
@inline (T::Type{<:StateKey})(c::Context, label) = T(c, c.actor, label)

# special case 1: exit transition keys are canonicalized
@inline (T::Type{<:StateKey})(c::Context, ::AbstractString,
	::AbstractString, ::typeof(exit)) = T("", "", exit)

# displaying API: return
Base.read(c::Context, k::StateKey) = string(k)

# ««2 String with language
struct LangString
	lang::Int8
	str::String
end
lang(s::LangString) = s.lang
str(s::LangString) = s.string
# ««2 Stte data
struct StateData{I<:Integer}
	priority::Float32
	text::Union{Int32,LangString}
	trigger::I
# 	@inline StateData{I}(p::Float32, t, i) where{I} = new{I}(p, t, i)
end
@inline (T::Type{<:StateData})(c::Context; text = nothing, trigger = nothing,
		priority::Real = -eps(Float32)) =
	T(priority, get_string(c, text), get_state_trigger(c, trigger))

Base.read(c::Context, x::StateData) = (
	text = string(x.text),
	trigger = findfirst(c.state_triggers,x.trigger))

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
mutable struct TransitionData{I<:Integer}
	text::Union{Int32,LangString}
	journal::Union{Int32,LangString}
	trigger::I
	action::I
	flags::UInt32
end
function (T::Type{<:TransitionData})(c::Context;
	text = nothing, journal = nothing, action = nothing, trigger = nothing,
	flags = UInt32(0), kwargs...)
	text = get_string(c, text)
	journal = get_string(c, journal)
	action = push!(c.actions, action)
	trigger = get_transition_trigger(c, trigger)
	flags = something(flags, Flags.set(;
		text = !iszero(text), journal = !iszero(journal),
		action = !iszero(action), trigger = !iszero(trigger), kwargs...))
	return T(text, journal, trigger, action, flags)
end

Base.read(c::Context, x::TransitionData) = (
	text = x.text, journal = x.journal, flags = x.flags,
	trigger = findfirst(c.transition_triggers, x.trigger),
	action = findfirst(c.actions, x.action))

# ««1 Dialog-building API: all code manipulating global data goes here
"""    init(I,S,T,K,C)

Sets up the global state machine to have index type `I`, state data type `S`,
transition data type `T`, state key type `K`, and context type `C`.

This function should be removed once we figure the correct value
for all its parameters (and thus are able to turn them into constants).
"""
function init(I::Type{<:Integer}, S::DataType, T::DataType,
	K::DataType, C::DataType)
	global machine = StateMachine{I,S,T,K,C}()
end
function init_test(I::Type{<:Integer}, S::DataType, T::DataType, X::DataType)
	SK = StateKey{X}
	SD = StateData{I}
	TD = TransitionData{I}
	CX = Context{I,Tuple{I,S},T}
	SM = StateMachine{I,SD,TD,SK,CX}
	# strings are labelled with their language: (language, string)
	# (language slightly expanded to also define PC gender)
	global machine = SM()
end
#««2 Misc.: namespace, actor, language, trigger

@inline namespace(str) = namespace(machine.context, str)
@inline actor(str) = actor(machine.context, str)
@inline language(str) = language(machine.context, str)
@inline trigger(str::AbstractString) = trigger!(machine.context, str)
@inline journal(args...; kwargs...) = journal(machine, args...; kwargs...)
@inline action(args...; kwargs...) = action(machine, args...; kwargs...)

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
function state(kargs...; text::Union{Nothing,AbstractString} = nothing, kw...)
	# k is key data:
# 	key = statekey(machine, args...)
	if_current_state(machine) do s
		if isempty(s.transitions)
			println("  implicit transition needed")
# 			transition()
			add_transition!(machine, s, key)
		end
	end
	add_state!(machine, key; text, kw...)
end
function state(k::StateKey, ::Nothing; override = false,
		trigger::Union{Nothing,AbstractString} = nothing)
	error("todo: move current_state around")
end

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
@inline transition(args...; kwargs...) =
	add_transition!(machine, current_state(machine), statekey(machine, args...);
		kwargs...)
@inline transition(; kwargs...) =
	add_transition!(machine, current_state(machine), nothing; kwargs...)

@inline answer((text, k)::Pair{<:AbstractString,<:Tuple}; kw...) =
	transition(k...; text, kw...)
@inline answer((text, label)::Pair{<:AbstractString}; kw...) =
	transition(label; text, kw...)
@inline answer(text::AbstractString; kw...) = transition(; text, kw...)

# ««1 printing
Base.show(io::IO, k::StateKey) =
	print(io, "\"", k.namespace, "/", k.actor, "\"", k.label|>repr, "")

function printstate(io::IO, c::Context, s::StateData)
end
function Base.show(io::IO, mime::MIME"text/plain", m::StateMachine)
	keylist = m.keys|>collect
	for (i,k) in keylist|>pairs
		println(io, i, " = ", k)
	end
	println(io)
	for (i, s) in pairs(m.states)
		x = read(m.context, s.data)
		println(io, "state \e[1m<", keylist[i], '=', i, ">:\e[m with ",
			length(s.transitions), " transitions")
		!isnothing(x.trigger) &&
			println(io, "trigger=\e[33m", x.trigger|>repr, "\e[m")
		!isnothing(x.text) && println(io, "\e[34m", x.text|>repr, "\e[m")
		for j in s.transitions
			t = m.transitions[j]
			y = read(m.context, t.data)
# 			if hasfield(typeof(t),:target) && getfield(t.target,:.label == exit
# 				print(io, "  \e[7m(final)\e[m")
# 			else
			print(io, " `-> \e[38;5;7m<", t.target, haskey(m.keys, t.target) ?
				"="*string(m.keys[t.target]) : " \e[7mnot found\e[m", ">\e[m")
# 			end
			!iszero(y.flags) && print(io, "  flags=", Flags.string(y.flags))
			println(io)
			!isnothing(y.trigger) &&
				println(io, "  trigger=\e[33m", y.trigger|>repr, "\e[m")
			!isnothing(y.text) &&
				println(io, "  \e[36m", y.text|>repr, "\e[m")
			!isnothing(y.action) &&
				println(io, "  action=\e[31m", y.action|>repr, "\e[m")
			!isnothing(y.journal) &&
				println(io, "  \e[35m", y.journal|>repr, "\e[m")
		end
	end
end

# »»1
export namespace, actor, trigger, action, journal, language
export state, say, transition, answer
end
# D = Dialogs
# for f in names(D); f == nameof(D) && continue
# 	eval(:(@inline $f(args...; kwargs...) = D.$f(args...; kwargs...)))
# end
# 
# Dialogs.init_test(Int32, String, String, Any)
# 
# language("en")
# namespace("main")
# actor("Alice")
# trigger("WEATHER(NICE)")
# say(:toto => "this morning...")
# say(:hello => "weather is nice today", "sunny and all!")
#   answer("bye" => exit)
# 		action("QUIT()")
# say(:hello2 => "it rains.. again", "but tomorrow it will be sunny"; priority=-1)
# 	answer("let's hope so" => :hello)
# 	answer("what does B say about this?" => ("Bob", :hello); position=1)
# 	  journal("Today I asked a question to Bob")
# 	transition(exit)
# actor("Bob")
# say(:hello => "I am Bob!!!")
# 	transition(exit)
# 
# D.machine
