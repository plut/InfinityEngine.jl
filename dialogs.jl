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

export Collector
end
using .Collectors

# ««1 State machine
struct State{I,S}
	transitions::Vector{I}
	data::S
end
struct Transition{K,T}
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
	keys::Collector{K}
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

struct Builder{I<:Integer,M<:StateMachine{I}}
	machine::M
	# current values of state and transition:
	current_state::Base.RefValue{I} #  into machine.states
	current_transition::Base.RefValue{I} # into machine.transitions[state]
	Builder{I,M}() where{I<:Integer,M<:StateMachine{I}} =
		new{I,M}(M(), Ref(zero(I)), Ref(zero(I)))
end
machinetype(::Builder{I,M}) where{I,M} = M
indextype(::Builder{I}) where{I} = I
statedata(b::Builder) = statedata(b.machine)
transitiondata(b::Builder) = transitiondata(b.machine)
statetype(b::Builder) = statetype(b.machine)
transitiontype(b::Builder) = transitiontype(b.machine)
Base.keytype(b::Builder) = keytype(b.machine)

set_state!(b::Builder, key) = b.current_state[] = b.machine.keys[key]

function add_state!(b::Builder, args...)
	add_state!(b.machine, args...)
	b.current_state[] = length(b.machine.states)
end

set_transition!(b::Builder, i) = b.current_transition[] = i

function add_transition!(b::Builder, source, target, data, position)
	add_transition!(b.machine, source, target, data, position)
	b.current_transition[] = position
end

current_state(b::Builder) =
	(@assert !iszero(b.current_state[]); b.machine.states[b.current_state[]])
if_current_state(f::Function, b::Builder) =
	!iszero(b.current_state[]) && f(b.machine.states[b.current_state[]])

current_transition(b::Builder) = let s = current_state(b)
	t = b.current_transition[]
	b.machine.transitions[s.transitions[t]]
end
if_current_transition(f::Function, b::Builder) = if_current_state(b) do s
	t = b.current_transition[]
	!iszero(t) && f(b.machine.transitions[s.transitions[t]])
end


# State machine builder ««1
"state key type for the global state machine being built"
struct StateKey{X}
	namespace::String
	actor::String
	label::X
end

"state data type for the global state machine being built"
mutable struct StateData{I<:Integer}
	priority::Float32
	# Payload: (we could also turn all of these into a type parameter)
	text::I
	trigger::I
	StateData{I}(; text=~zero(I), trigger=~zero(I), priority=-eps(Float32)
		) where{I<:Integer}= new{I}(priority, text, trigger)
end

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

# Build context ««1

"context for the state machine builder. S is string type, T is action type."
mutable struct Context{I<:Integer,S,T}
	strings::Collector{S,I}
	actions::Collector{T,I}
	state_triggers::Collector{T,I}
	transition_triggers::Collector{T,I}
	languages::Collector{String,I}

	namespace::Union{Nothing,String}
	actor::Union{Nothing,String}
	trigger::Union{Nothing,T}
	language::I
	# XXX default constructor is needed for functoriality?
	Context{I,S,T}() where{I<:Integer,S,T} = reset(new{I,S,T}())
end
function Base.reset(c::Context)
	for f in (:strings, :actions, :state_triggers,:transition_triggers,:languages)
		setfield!(c, f, fieldtype(typeof(c), f)())
	end
	c.namespace = nothing
	c.actor = nothing
	c.trigger = nothing
	c.language = noindex(c)
	return c
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

string_idx(c::Context, ::Nothing) = noindex(c)
string_idx(c::Context, s::AbstractString) =
	(@assert isindex(c.language); push!(c.strings, (c.language, s)))

# ««2 Trigger handling: this is actually a push queue with a max length of 1
"sets the current trigger value; only allowed if it is not set yet"
trigger!(c::Context, str::AbstractString) =
	(@assert isnothing(c.trigger); c.trigger = str)
trigger!(c::Context, ::Nothing) = nothing
"gets (and then erases) the current trigger value"
trigger(c::Context) = (x = c.trigger; c.trigger = nothing; return x)
"gets a trigger value, from either supplied x, or current trigger"
get_trigger(c::Context, x) = (trigger!(c, x); return trigger(c))

# function Base.show(io::IO, ::MIME"text/plain", m::StateMachine)#««
# 	str = collect(m.strings)
# 	s_tri = collect(m.state_triggers)
# 	t_tri = collect(m.transition_triggers)
# 	act = collect(m.actions)
# 	p = collect(pairs(m.states))
# 	revk = collect(m.state_keys)
# 	for (k, s) in sort(collect(pairs(m.states));
# 		by=((k,s),)->(s.priority, -k), rev=true)
# 		println(io, "state \e[1m<$k $(revk[k])>\e[m: ($(s.priority))",
# 			"\e[34m$(str[s.text]|>repr)\e[m",
# 		)
# 		if isindex(s.trigger)
# 			println(io, "  has trigger: \e[33m$(s_tri[s.trigger]|>repr)\e[m")
# 		end
# 		println(io, "  $(length(s.transitions)) transitions: $(s.transitions)")
# 		for j in s.transitions
# 			j ∈ eachindex(m.transitions) ||
# 				(println(io, "\e[31;2mtransition not found: $j\e[m"); continue)
# 
# 			t = m.transitions[j]
# 			print(io, "  $j = flags(", Flags.string(t.flags), ")  ",
# # 					"=> \e[37m<$(get(m.state_keys,t.target,exit)) = $(t.target)>\e[m",
# # 					"\e[36m$(get(str,t.text,nothing)|>repr)\e[m",
# # 					" ",
# # 					"\n$(t.action)=\e[31m$(get(act,t.action,nothing)|>repr)\e[m",
# # 					 "\n\e[35m$(get(str,t.journal,nothing)|>repr)\e[m",
# 				contains(t.flags, :terminates) ? "\e[7m(final)\e[m" :
# 					"=> \e[37m<$(get(m.state_keys,t.target,undef))>\e[m",
# 				" ",
# 				contains(t.flags, :text) ? "\e[36m$(str[t.text]|>repr)\e[m" :
# 					"(no text)",
# 				" ",
# 				contains(t.flags, :action) ? "\n\e[31m\"$(act[t.action])\"\e[m" :
# 					"(no action)",
# 				contains(t.flags, :journal) ? "\n\e[35m$(str[t.journal]|>repr)\e[m" :
# 					"",
# 			)
# 			println(io)
# 		end
# 	end
# end#»»
# ««1 All code manipulating global data goes here
# ««2 Initialization
function new_context(I::Type{<:Integer}, S::DataType, T::DataType, X::DataType)
	SK = StateKey{X}
	SD = StateData{I}
	TD = TransitionData{I}
	SM = StateMachine{I,SD,TD,SK}
	# strings are labelled with their language: (language, string)
	# (language slightly expanded to also define PC gender)
	return (Builder{I,SM}(), Context{I,Tuple{I,S},T}())
end

global (builder, context) = new_context(Int32, String, String, Any)

#««2 Small stuff: key, namespace, actor, language
key(namespace, actor, label) = keytype(builder)(namespace, actor, label)
key(actor::AbstractString, label) = key(context.namespace, actor, label)
key(label) = key(context.current_actor, label)
# default constructor, used for when no key is needed (final transitions):
key(::typeof(undef)) = key("", "", undef)

namespace(text) = (context.namespace = text)
actor(text) = (context.actor = text)
language(text) = (context.language = push!(context.languages, text))

#««2 Higher-level: `trigger`, `action`, `journal`
trigger(str::AbstractString) = trigger!(context, str)

function action(str::AbstractString; override=false)
	t = current_transition(builder)
	@assert(override || !contains(t.flags, :action),
		"action already defined for this transition")
	t.flags |= Flags.set(action = true)
	t.action = push!(builder.actions, str)
end

function journal(str::AbstractString; override=false)
	t = current_transition(builder)
	@assert(override || !contains(t.flags, :journal),
		"journal entry already defined for this transition")
	t.flags |= Flags.set(journal = true)
	t.journal = string_idx(builder, str)
end

#««2 States
" lowest-level state creating function. context resolves triggers and text;
we build the state data; builder bookkeeps current state."
function add_state!(c::Context, b::Builder, key; text, trigger=nothing, kw...)
	# kw... passed to StateData: priority
	key::keytype(b)
	text = string_idx(c, text)
	trigger = push!(c.state_triggers, get_trigger(c, trigger))
	data = statedata(b)(; text, trigger, kw...)
	add_state!(b, key, data)
end

""""
    state(actor, ([label =>] text)*; trigger)

Creates one (or more) states. The state keys are (current_namespace,
actor, label). Labels are automatically generated if not provided.

If more than one state is given then they are linked by implicit
transitions.
"""
function state(actor::AbstractString,
	(label, text)::Pair{<:Any,<:AbstractString}; kw...) 
	println("** adding state with label $label")
	k = key(actor, label)
	if_current_state(builder) do s
		println("add implicit transition from $s to $label")
		isempty(s.transitions) && add_transition!(context, builder, s, k)
	end
	add_state!(context, builder, k; text, kw...)
end
state(actor::AbstractString, text::AbstractString; kw...) =
	state(actor, gensym() => text; kw...)

const StateText = Union{AbstractString,Pair{<:Any,<:AbstractString}}
state(actor::AbstractString, args::StateText...; kwargs...) =
	for a in args; state(actor, a; kwargs...); end

"""    say([label =>] text...; trigger)

Creates one (or more) state(s). The state keys are
`(current_namespace, current_actor, label)`.
In other words this is equivalent to (and defined as)
`state(current_actor, [label =>] text...)`.
Labels are automatically generated (but unreachable) if not provided.
"""
say(args::StateText...; kw...) = state(context.actor, args...; kw...)

#««2 Transitions
"low-level transition adding function. context resolves triggers, text and journal; we build the transition data; builder inserts and bookkeeps."
function add_transition!(c::Context, b::Builder, source, target;
		text::Union{Nothing,AbstractString} = nothing,
		journal::Union{Nothing,AbstractString} = nothing,
		action::Union{Nothing,AbstractString} = nothing,
		trigger::Union{Nothing,AbstractString} = nothing,
		position::Integer = 1 + length(source.transitions),
		kwargs...) # passed to TransitionData (and hence to Flags)
	source::statetype(b)
	target::keytype(b)
	text = string_idx(c, text)
	journal = string_idx(c, journal)
	action = push!(c.actions, action)
	trigger = push!(c.transition_triggers, get_trigger(c, trigger))
	data = transitiondata(b)(;text, journal, trigger, action, kwargs...)

	add_transition!(b, source, target, data, position)
end

transition(actor::AbstractString, label, args...; kwargs...) =
	transition_key(key(actor, label), args...; kwargs...)
transition(::typeof(exit), args...; kwargs...) =
	transition_key(key(undef), args...; terminates = true, kwargs...)

transition_key(k, text::AbstractString; kwargs...) =
	transition_key(k; text, kwargs...)
transition_key(k; kwargs...) =
	add_transition!(context, builder, current_state(builder), k; kwargs...)

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
answer(::typeof(exit); kw...) = transition(exit; kw...)
	
#»»2
#»»1

export namespace, actor, trigger, action, journal, language
export state, say, transition, answer
end
D = Dialogs
for f in names(D); f == nameof(D) && continue
	eval(:(@inline $f(args...; kwargs...) = D.$f(args...; kwargs...)))
end

language("en")
namespace("g")
actor("Alice")
trigger("weather is nice")
say(:hello => "weather is nice today", "sunny and all!")
  answer("bye" => exit)
# 		action("i am gone")
# say(:hello2 => "it rains.. again", "but tomorrow it will be sunny"; priority=-1)
# 	answer("let's hope so" => :hello)
# 	answer("what does B say about this?" => ("Bob", :hithere); position=1)
# 	  journal("Today I asked a question to Bob")
# 	answer(exit)
