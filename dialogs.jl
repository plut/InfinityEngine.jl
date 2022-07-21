#= TODO:
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
	 - TODO first: load state machine from .dlg resource
	 - check in examples about solved/unsolved etc.
	+ action)
	 - check about strrefs in action code!
	 - define a Julia syntax for action code?
 - Editing existing entries
  - define edition types (defer to end once everything is loaded ?)
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

Collects values of type `T`, assigning them sequential indices.
If `unique?` is true then only unique values are allowed, otherwise
duplicates are merged.

    push!(collector, key)

Appends a new key and returns the corresponding index.

    collect(collector)

Returns the sequence of values collected, i.e. a `list` of keys
such that `list[collector[key]] == key`.
"""
struct Collector{T,B,I<:Integer} <: AbstractDict{T,I}
	index::Dict{T,I}
	Collector{T,B,I}() where{T,B,I<:Integer} = new(Dict{T,I}())
	Collector{T,B}(args...) where{T,B} = Collector{T,B,Int}(args...)
	function (C::Type{<:Collector{T}})(args::T...) where{T}
		c = C()
		push!(c, args...)
		return c
	end
	# default constructor for functoriality
	Collector{T,B,I}(index::Dict{T,I}) where{T,B,I} = new{T,B,I}(index)
end
@inline Base.valtype(c::Collector{T,B,I}) where{T,B,I} = I
@inline isunique(c::Collector{T,B}) where{T,B} = B
@inline Base.convert(T::Type{<:Collector}, ::Tuple{}) = T()
for f in (:keytype, :length, :empty!, :isempty, :haskey, :getindex, :get,
	:pairs, :iterate, :setindex!)
	@eval Base.$f(c::Collector, args...) = $f(c.index, args...)
end
function Base.push!(c::Collector, s; unique = isunique(c))
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
export Collector
end
using .Collectors
mutable struct Transition{I<:Integer,K}
	target::K
	# Payload: (we could also turn all of these into a type parameter,
	# but would this really be useful?)
	text::I
	journal::I
	trigger::I
	action::I
	flags::UInt32
	Transition{I,K}(target::K; text = ~zero(I), journal=~zero(I),
		trigger=~zero(I), action=~zero(I), flags=UInt32(0)) where{I<:Integer,K} =
		new{I,K}(target, text, journal, trigger, action, flags)
end
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
mutable struct State{I<:Integer}
	transitions::Vector{I} # index into transition table
	priority::Float32
	# Payload: (we could also turn all of these into a type parameter)
	text::I
	trigger::I
	State{I}(; text=~zero(I), trigger=~zero(I), priority=-eps(Float32),
		transitions=I[]) where{I<:Integer}=
		new{I}(transitions, priority, text, trigger)
end
indextype(T::Type{<:State{I}}) where{I} = I

mutable struct StateMachine{I<:Integer,S,T,K}
# TODO: keep states ordered (use a priority system)
# - first created state has highest priority
# - states with explicit priority > states with implicit priority
# i.e. priority is a triple:
# (is-explicit, given-priority, created-first) (in lex ordering)
# i.e. (Bool, Real, Int)
# we can do slightly better: use -eps for implicit priority
# so that this is (explicit-priority||-Inf, -counter).
	states::Vector{State{I}}
	transitions::Vector{Transition{I,K}}
	state_keys::Collector{K,true,I}
	current_state::I
	current_transition::I

	StateMachine{I,S,T,X}() where{I<:Integer,S,T,X} = reset(new{I,S,T,X}())
	# FIXME: the following stuff is not really generic for all state
	# machines, yet we really need those:
	# (but this means that we would also need to
	# abstract away the state and action payloads as type parameters)
	strings::Collector{S,false,I}
	actions::Collector{T,false,I}
	triggers::Collector{T,false,I}

	current_namespace::Union{Nothing,String}
	current_actor::Union{Nothing,String}
	current_trigger_idx::I
	# default constructor is needed for functoriality:
	StateMachine{I,S,T,X}(a1,a2,a3,a4,a5,a6,a7,a8,a9,aa,ab) where{I,S,T,X} =
		new{I,S,T,X}(a1,a2,a3,a4,a5,a6,a7,a8,a9,aa,ab)
end

indextype(::StateMachine{I}) where{I} = I
stringtype(::StateMachine{I,S}) where{I,S} = S
actiontype(::StateMachine{I,S,T}) where{I,S,T} = T
Base.keytype(m::StateMachine) = keytype(m.state_keys)
statetype(m::StateMachine) = eltype(m.states)
transitiontype(m::StateMachine) = eltype(m.transitions)

# instead of doing lots of Union{Nothing,...}, we simply
# mark invalid keys by 0xffff, which is how they will eventually be
# written in the file anyway:
isindex(i::Integer) = !iszero(~i)
noindex(m::StateMachine) = ~zero(indextype(m))
# special case: appending a `nothing` object to a collector does nothing:
Base.push!(c::Collector, ::Nothing) = ~zero(valtype(c))


transitions(m::StateMachine,s::State)= (m.transitions[i] for i in s.transitions)

function Base.reset(m::StateMachine)
	for f in (:states, :transitions, :state_keys, :strings, :actions, :triggers)
		setfield!(m, f, fieldtype(typeof(m), f)())
	end
	m.current_state = ~zero(m.current_state)
	m.current_transition = ~zero(m.current_transition)
	m.current_namespace = nothing
	m.current_actor = nothing
	m.current_trigger_idx = noindex(m)
	return m
end

struct PrintData{S,T,X}
	strings::Vector{S}
	triggers::Vector{T}
	actions::Vector{T}
	keys::Vector{X}
end
	
function Base.show(io::IO, ::MIME"text/plain", m::StateMachine)
	str = collect(m.strings)
	tri = collect(m.triggers)
	act = collect(m.actions)
	p = collect(pairs(m.states))
	revk = collect(m.state_keys)
	for (k, s) in sort(collect(pairs(m.states));
		by=((k,s),)->(s.priority, -k), rev=true)
		println(io, "state \e[1m<$k>\e[m: ($(s.priority))",
			"\e[34m\"$(str[s.text])\"\e[m",
		)
		if isindex(s.trigger)
			println(io, "  has trigger: \e[33m\"$(tri[s.trigger])\"\e[m")
		end
		println(io, "  $(length(s.transitions)) transitions:")
		for j in s.transitions
			t = m.transitions[j]
			print(io, "  $j = flags(", Flags.string(t.flags), ")  ",
				contains(t.flags, :terminates) ? "\e[7m(final)\e[m" :
					"=> \e[37m<$(get(m.state_keys,t.target,undef))>\e[m",
				" ",
				contains(t.flags, :text) ? "\e[36m\"$(str[t.text])\"\e[m" : "(no text)",
				" ",
				contains(t.flags, :action) ? "\n\e[31m\"$(act[t.action])\"\e[m" :
					"(no action)",
				contains(t.flags, :journal) ? "\n\e[35m\"$(str[t.journal])\"\e[m" : "",
			)
			println(io)
		end
	end
end

struct StateKey{X}
	namespace::String
	actor::String
	label::X
end

"global object describing the current state-machine being built."
global machine = StateMachine{Int32,String,String,StateKey{Any}}()
key(actor::AbstractString, label) =
	keytype(machine)(machine.current_namespace,actor,label)
key(label) = key(machine, machine.current_actor, label)

# default constructor, used for when no key is needed (final transitions):
key(::typeof(undef)) = keytype(machine)("", "", undef)

#««2 Small stuff: namespace, actor, trigger
namespace(text) = (machine.current_namespace = text)
actor(text) = (machine.current_actor = text)

"sets current trigger. It is an error to call this twice."
function current_trigger!(m::StateMachine, text::AbstractString)
	@assert !isindex(m.current_trigger_idx)
	m.current_trigger_idx = push!(m.triggers, text)
end
current_trigger!(::StateMachine, ::Nothing) = nothing

# methods called by add_state!, add_transition!:
"erases current trigger, returning erased value."
function current_trigger_idx(m::StateMachine)
	x = m.current_trigger_idx
	m.current_trigger_idx = noindex(m)
	return x
end
"selects between current trigger and provided value"
current_trigger_idx(m::StateMachine, text::Union{Nothing,AbstractString}) =
	(current_trigger!(m, text); return current_trigger_idx(m))

#««2 Higher-level: `trigger`, `action`, `journal`
trigger(text::AbstractString) = current_trigger!(machine, text)

function action(text::AbstractString; override=false)
	t = current_transition(machine)
	@assert(override || !contains(t.flags, :action),
		"action already defined for this transition")
	t.flags |= Flags.set(action = true)
	t.action = push!(machine.actions, text)
	machine
end

function journal(text::AbstractString; override=false)
	t = current_transition(machine)
	@assert(override || !contains(t.flags, :journal),
		"journal entry already defined for this transition")
	t.flags |= Flags.set(journal = true)
	t.journal = push!(machine.strings, text)
	machine
end

#««2 States
" lowest-level state adding function for a state machine."
function add_state!(m::StateMachine, k, text;
		trigger = nothing, kwargs...) # only priority in kwargs
	k::keytype(m)
	haskey(m.state_keys, key) && error("duplicate state key: $key")
	text = push!(m.strings, text)
	trigger = current_trigger_idx(m, trigger)
	s = statetype(m)(;text, trigger, kwargs...)
	# create implicit transition if needed
	if !isempty(m.states) && isempty(last(m.states).transitions)
		println("implicit transition from state $(length(m.states)) to next")
		add_transition!(m, last(m.states), k)
	end
	push!(m.states, s)
	push!(m.state_keys, k)
	m.current_state = length(m.states)
	m
end

current_state(m::StateMachine) =
	(@assert !isempty(m.states); m.states[m.current_state])

""""
    state(actor, ([label =>] text)*; trigger)

Creates one (or more) states. The state keys are (current_namespace,
actor, label). Labels are automatically generated if not provided.

If more than one state is given then they are linked by implicit
transitions.
"""
state(actor::AbstractString, (label, text)::Pair{<:Any,<:AbstractString};
		kw...) = 
	add_state!(machine, key(actor, label), text; kw...)
state(actor::AbstractString, text::AbstractString; kw...) =
	state(actor, gensym() => text; kw...)

const StateText = Union{AbstractString,Pair{<:Any,<:AbstractString}}
state(actor::AbstractString, args::StateText...; kwargs...) =
	for a in args; state(actor, a; kwargs...); end

"""    say([label =>] text...; trigger)

Creates one (or more) state. The state keys are
`(current_namespace, current_actor, label)`.
In other words this is equivalent to (and defined as)
`state(current_actor, [label =>] text...)`.
Labels are automatically generated (but unreachable) if not provided.
"""
say(args::StateText...; kw...) = state(machine.current_actor, args...; kw...)

#««2 Transitions
"""    add_transition!(machine, source state, target key; position, payload...)

 - `position` is the insertion point in the transition table for this state (default is last).
"""
function add_transition!(m::StateMachine, s, target;
		text::Union{Nothing,AbstractString} = nothing,
		journal::Union{Nothing,AbstractString} = nothing,
		action::Union{Nothing,AbstractString} = nothing,
		trigger::Union{Nothing,AbstractString} = nothing,
		position::Union{Nothing,Integer} = nothing,
		kwargs...) # KW: terminates
	target::keytype(m)
	s::statetype(m)
	position = something(position, 1+length(s.transitions))
	text = push!(m.strings, text)
	journal = push!(m.strings, journal)
	action = push!(m.strings, action)
	trigger = current_trigger_idx(m, trigger)
	flags = Flags.set(; text = isindex(text), journal = isindex(journal),
		action = isindex(action), trigger = isindex(trigger), kwargs...)
	t = transitiontype(m)(target; text, journal, trigger, action, flags)
	push!(m.transitions, t)
	insert!(s.transitions, position, length(m.transitions))
	m.current_transition = position
	m
end

current_transition(m::StateMachine) = let s = current_state(m)
	@assert !isempty(s.transitions)
	m.transitions[s.transitions[m.current_transition]]
end

transition(actor::AbstractString, label, args...; kwargs...) =
	transition_key(key(actor, label), args...; kwargs...)
transition(::typeof(exit), args...; kwargs...) =
	transition_key(key(undef), args...; terminates = true, kwargs...)

transition_key(k, text::AbstractString; kwargs...) =
	add_transition!(machine, current_state(machine), k; text, kwargs...)
transition_key(k; kwargs...) =
	add_transition!(machine, current_state(machine), k; kwargs...)

"""    answer(text => (actor, label); flags)
    answer(text => label; flags)

Creates a transition from the current state. Equivalent to
`transition(actor, label, text)`.
In the second form, the current actor is used for the target."""
answer((text, (actor, label))::Pair{<:AbstractString,Tuple{<:AbstractString,<:Any}}; kw...) =
	transition(actor, label, text; kw...)
answer((text, label)::Pair{<:AbstractString,typeof(exit)}; kw...) =
	transition(exit, text; kw...)
answer((text, label)::Pair{<:AbstractString,<:Any}; kw...) =
	transition(machine.current_actor, label, text; kw...)
	
#»»2

export namespace, actor, trigger, action, journal
export state, say, transition, answer
end
# D = Dialogs
# for f in names(D); f == nameof(D) && continue
# 	eval(:(@inline $f(args...; kwargs...) = D.$f(args...; kwargs...)))
# end
# 
# namespace("g")
# actor("Alice")
# trigger("weather is nice")
# say(:hello => "weather is nice today", "sunny and all!")
#   answer("bye" => exit)
# 		action("i am gone")
# say(:hello2 => "it rains.. again", "but tomorrow it will be sunny"; priority=-1)
# 	answer("let's hope so" => :hello)
# 	answer("what does B say about this?" => ("Bob", :hithere); position=1)
# 	  journal("Today I asked a question to Bob")
