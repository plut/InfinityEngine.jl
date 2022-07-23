#= TODO:

 - NAMESPACE: could be used for either actor / text / transitions
  - actors should already be globally unique (i.e. pointers to creatures)
	 - if they are not then we have another problem with creatures
	- namespace applies to all text for a given actor
	- transition target: find a way ot indicate either (other_actor, label)
  (with same namespace) or (other namespace, label) (with same actor)
	- case 1: (other actor, same ns, target)
	  we are writing banter between our npcs
		reply(
	- case 2: (same actor, other ns, target)
	  we are inserting into other dialogue for this actor
	
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

    state("Bob", label)

This resets the “current state” global variable to this one.
It is then possible to insert new transitions for this state.

## Transitions

### Creating transitions

Transitions are created relatively to the last created state by the call:

    transition(actor, label, transition data...)
    # or equivalently:
    reply(text => (actor, label))

Here `text` is said by the PC.

TODO: define transition flags.

### Transition targets

The target of a transition is a state. For the `reply` form this is indicated
as either a `(actor, label)` form or as a single label; in this case,
the current actor (see above) is used.

The special target `exit` means that the conversation ends there:

    reply(text => exit)

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
which will be used by the next `say()` or `reply()` call.

    condition("is(ELF)")
    say("Hello, elven friend!")

### Actions

Transitions (not states) may have actions attached.
These are either passed to the `transition()` function via the `action`
keyword argument, or later via the `action()` function.

The `action()` function modifies the action of the last transition defined:

    reply("then fight me!" => exit)
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
     reply("good bye" => exit)
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

using InternedStrings
using UniqueVectors
#««1 Basic types
#««2 Context
mutable struct Context
	language::Int
	namespace::Union{Nothing,String}
	actor::Union{Nothing,String}
	trigger::Union{Nothing,String}

	@inline Context() = new(0, nothing, nothing, nothing)
end

#««2 Context accessors
@inline namespace(c::Context, str) = (c.namespace = str)
@inline actor(c::Context, str::AbstractString) = (c.actor = uppercase(str))
"sets the current trigger value; only allowed if it is not set yet"
trigger!(c::Context, str::AbstractString) =
	(@assert isnothing(c.trigger); c.trigger = str)
trigger!(c::Context, ::Nothing) = nothing
"gets a trigger value, from either supplied x, or current trigger"
@inline function get_trigger(c::Context, x)
	trigger!(c, x) # first update current trigger (no-op if x == `nothing`)
	y = c.trigger  # then get trigger value
	c.trigger = nothing # purge the buffer
	intern_string(y)
end
@inline intern_string(::Nothing) = ""
@inline intern_string(s::AbstractString) = intern(s)

# ««2 String annotated with language
struct LangString
	lang::Int8
	str::String
end
lang(s::LangString) = s.lang
str(s::LangString) = s.string

# our string type is Union{Int32,LangString}; methods for this go here:
isstring(::Int32) = true
isstring(s::LangString) = !iszero(s.lang)

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

# ««2 States
struct State{I}
	transitions::Vector{I}
	priority::Float32
	text::Union{Int32,LangString}
	trigger::String
end
@inline State{I}(c::Context; text = nothing, trigger = nothing,
		priority::Real = -eps(Float32)) where{I} =
	State{I}(I[], priority, get_string(c, text), get_trigger(c, trigger))

# ««2 Transition flags
"module holding syntactic sugar for transition flags"
module Flags
	@inline set(; text = false, trigger = false, action = false,
			terminates = false, journal = false, interrupt = false,
			unsolved = false, journalentry = false, solved = false) =
		UInt32(text<<0 | trigger<<1 | action<<2 | terminates<<3 | journal << 4 |
			interrupt <<5 | unsolved << 6 | journalentry << 7 | solved << 8)
	@inline Base.contains(x, flag::Symbol) =
		!iszero(x & set(; NamedTuple{(flag,),Tuple{Bool}}((true,))...))
	function string(x; default="0") # not importing Base!
		kw = Base.kwarg_decl(first(methods(set).ms))
		iszero(x) ? default :
		join((Base.string.(k) for k in kw if contains(x, k)), '|')
	end
end

# ««2 Transitions
mutable struct Transition{X}
	target::StateKey{X}
	text::Union{Int32,LangString}
	journal::Union{Int32,LangString}
	trigger::String
	action::String
	flags::UInt32
end
function Transition{X}(c::Context, target;
		text = nothing, journal = nothing, action = nothing, trigger = nothing,
		flags = nothing, kwargs...) where{X}
	text = get_string(c, text)::Union{Int32,LangString}
	journal = get_string(c, journal)::Union{Int32,LangString}
	trigger = get_trigger(c, trigger)::String
	action = intern_string(action)::String
	flags = something(flags, Flags.set(;
		text = isstring(text), journal = isstring(journal),
		action = !isempty(action), trigger = !isempty(trigger), kwargs...))
	Transition{X}(target, text, journal, trigger, intern_string(action), flags)
end
# ««1 Dialog structure
# ««2 Type and accessors
"""    Dialog{I,X}

`I` is index type, `X` is label type.

"""
struct Dialog{I,X}
	states::Vector{State{I}}
	transitions::Vector{Transition{X}}
	keys::UniqueVector{StateKey{X}}
	languages::UniqueVector{String}
	# the context structure holds the global data:
	context::Context
	# A few mutable fields useful while building the machine:
	current_state::Base.RefValue{I}
	current_transition::Base.RefValue{I}
	pending_transition::Base.RefValue{Bool}

	Dialog{I,X}() where{I,X} = new{I,X}([], [],
		UniqueVector{StateKey{X}}(), UniqueVector{String}(),
		Context(), Ref(zero(I)), Ref(zero(I)), Ref(false))
end

indextype(::Dialog{I}) where{I} = I
statetype(m::Dialog) = eltype(m.states)
transitiontype(m::Dialog) = eltype(m.transitions)
Base.keytype(m::Dialog) = eltype(m.keys)

@inline findkey(m::Dialog, k) = findfirst(isequal(k), m.keys)
@inline statekey(m::Dialog, args) = keytype(m)(m.context, args...)
language(m::Dialog, s::AbstractString) = findfirst!(isequal(s), m.languages)

#««2 State/transition
"    add_state!(dialog, keydata; kwargs...)

Builds a state with key defined from keydata and content from kwargs.
"
function add_state!(m::Dialog, keydata; kwargs...)
	key = statekey(m, keydata)::keytype(m)
	println("\e[1mINSERT STATE <$key>\e[m")
	if m.pending_transition[]
		println("  \e[32mresolve pending transition to $key\e[m")
		current_transition(m).target = key
		m.pending_transition[] = false
	end
	s = statetype(m)(m.context;kwargs...)
	push!(m.keys, key)
	push!(m.states, s)
	@assert length(m.keys) == length(m.states)
	m.current_state[] = length(m.states)
	return s
end
function set_state!(m::Dialog, keydata)
	key = statekey(m, keydata)::keytype(m)
	println("\e[1mSET STATE <$key>\e[m")
	i = findfirst(isequal(key), m.keys)
	@assert !isnothing(i) "State key not found"
	m.current_state[] = i
end
function add_transition!(m::Dialog, source, ::Nothing, args...; kwargs...)
	println("  \e[31madd pending transition\e[m")
	add_transition!(m, source, first(keys(m.keys)); kwargs...)
	m.pending_transition[] = true
end
function add_transition!(m::Dialog, source, keydata, args...;
		position = 1 + length(source.transitions), kwargs...)
	source::statetype(m)
	target = statekey(m, keydata)::keytype(m)
	println("  add transition to $target")
	@assert !m.pending_transition[] "unsolved pending transition"
	t = transitiontype(m)(m.context, target, args...;
		terminates = target.label == exit, kwargs...)
	push!(m.transitions, t)
	insert!(source.transitions, position, length(m.transitions))
	m.current_transition[] = position
	return t
end

current_state(m::Dialog) =
	(@assert !iszero(m.current_state[]); m.states[m.current_state[]])
if_current_state(f::Function, m::Dialog) =
	!iszero(m.current_state[]) && f(m.states[m.current_state[]])

current_transition(m::Dialog) = let s = current_state(m)
	t = m.current_transition[]
	m.transitions[s.transitions[t]]
end
if_current_transition(f::Function, m::Dialog) =
	if_current_state(m) do s
	t = m.current_transition[]
	!iszero(t) && f(m.transitions[s.transitions[t]])
end

#««2 Action/trigger
function action(m::Dialog, str::AbstractString; override=false)
	t = current_transition(m)
	@assert(override || !contains(t.flags, :action),
		"action already defined for this transition")
	t.flags |= Flags.set(action = true)
	t.action = push!(m.context.actions, str)
end
function journal(m::Dialog, str::AbstractString; override=false)
	t = current_transition(m)
	@assert(override || !contains(t.flags, :journal),
		"journal entry already defined for this transition")
	t.flags |= Flags.set(journal = true)
	t.journal = string_idx(m.context, str)
end

# ««1 Dialog-building API: all code manipulating global data goes here
"    single global variable = the dialog being built."
const top = Dialog{Int32,Any}()
#««2 Misc.: namespace, actor, language, trigger

@inline namespace(str::AbstractString) = namespace(top.context, str)
"""    
    actor(str)
    actor(namespace, str)
"""
@inline actor(str::AbstractString) = actor(top.context, str)
@inline language(str::AbstractString) =
	top.context.language = language(top, str)
@inline trigger(str::AbstractString) = trigger!(top.context, str)
@inline journal(args...; kwargs...) = journal(top, args...; kwargs...)
@inline action(args...; kwargs...) = action(top, args...; kwargs...)

#««2 States
"""
    state(namespace, actor, label)
    state(actor, label) # moves to this label (to edit it)
    state(label)
"""
state(args...) = set_state!(top, args)

"""    say(args...)

where each `say` argument is either:

    (actor, label) => text
    label => text
    text

If more than one state is given then they are linked by implicit
transitions.

The multi-state form is equivalent to consecutive invocations of `state`.
"""
@inline say((key, text)::Pair{<:Any,<:AbstractString}; kw...) =
	say2(text, key; kw...)
@inline say(text::AbstractString; kw...) = say(gensym() => text; kw...)
@inline say(args::Union{AbstractString,Pair{<:Any,<:AbstractString}}...;kw...)=
	say.(args; kw...)
@inline say2(text, key; kw...) = say2(text, (key,); kw...)
say2(text, key::Tuple; kw...) = add_state!(top, key; text, kw...)


#««2 Transitions
"""
    transition(namespace, actor, label; text...)
    transition(actor, label; text...)
    transition(label; text...)
    transition(exit) # special case of the previous
    transition() # not needed: this is the implicit transition!

    reply(text => (namespace, actor, label))
    reply(text => (actor, label))
    reply(text => label)
    reply(text => exit) # special case of previous
    reply(text)

Special considerations:
 - an exit key sets the `terminates` flag.
 - `reply` always has text, transition (in general) does not.
 - if no label is provied then the transition will point to the next state.
"""
@inline transition(args...; kwargs...) =
begin
	# args are repacked as a keydata for add_transition!
	add_transition!(top, current_state(top), args; kwargs...)
end
@inline transition(; kwargs...) =
	add_transition!(top, current_state(top), nothing; kwargs...)

@inline reply((text, k)::Pair{<:AbstractString,<:Tuple}; kw...) =
	transition(k...; text, kw...)
@inline reply((text, label)::Pair{<:AbstractString}; kw...) =
	transition(label; text, kw...)
@inline reply(text::AbstractString; kw...) = transition(; text, kw...)

# ««1 printing
Base.show(io::IO, k::StateKey) =
	print(io, k.actor, ":", k.namespace, "/", k.label|>repr, "")
Base.show(io::IO, s::LangString) = print(io, s.lang, s.str|>repr)

function Base.show(io::IO, mime::MIME"text/plain", m::Dialog)
	keylist = m.keys|>collect
	for (i,k) in keylist|>pairs
		println(io, i, " = ", k)
	end
	println(io)
	for (i, s) in pairs(m.states)
		println(io, "state \e[1m<", keylist[i], '=', i, ">:\e[m with ",
			length(s.transitions), " transitions")
		!isnothing(s.trigger) &&
			println(io, "trigger=\e[33m", s.trigger|>repr, "\e[m")
		!isnothing(s.text) && println(io, "\e[34m", s.text|>repr, "\e[m")
		for j in s.transitions
			t = m.transitions[j]
# 			if hasfield(typeof(t),:target) && getfield(t.target,:.label == exit
# 				print(io, "  \e[7m(final)\e[m")
# 			else
			ti = findkey(m, t.target)
			print(io, " `-> \e[38;5;7m<", t.target, !isnothing(ti) ? "="*string(ti) :
				" \e[7mnot found\e[m", ">\e[m")
# 			end
			!iszero(t.flags) && print(io, "  flags=", Flags.string(t.flags))
			println(io)
			!isnothing(t.trigger) &&
				println(io, "  trigger=\e[33m", t.trigger|>repr, "\e[m")
			!isnothing(t.text) &&
				println(io, "  \e[36m", t.text|>repr, "\e[m")
			!isnothing(t.action) &&
				println(io, "  action=\e[31m", t.action|>repr, "\e[m")
			!isnothing(t.journal) &&
				println(io, "  \e[35m", t.journal|>repr, "\e[m")
		end
	end
end

# »»1
export namespace, actor, trigger, action, journal, language
export state, say, transition, reply
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
#   reply("bye" => exit)
# 		action("QUIT()")
# say(:hello2 => "it rains.. again", "but tomorrow it will be sunny"; priority=-1)
# 	reply("let's hope so" => :hello)
# 	reply("what does B say about this?" => ("Bob", :hello); position=1)
# 	  journal("Today I asked a question to Bob")
# 	transition(exit)
# actor("Bob")
# say(:hello => "I am Bob!!!")
# 	transition(exit)
# 
# D.machine
