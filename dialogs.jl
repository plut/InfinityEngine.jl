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
	if !haskey(c.index, s)
		c.index[s] = length(c.index) # zero-based indexing...
	elseif unique
		error("repeated index: $s")
	end
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
(i.e. either when no state is defined, or just after a `state()` call).

$trigger_warning
"""
function action(text::AbstractString)
	tr = active_transition()
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

"""    state(label, text; [trigger])

Defines a state of the dialog with associated text.

If `trigger()` was called before `state()` then this defines the trigger
conditioning this state; otherwise a trigger may be defined using the
keyword syntax. (Doing both raises an error).
"""
function state(x::X, text::AbstractString; trigger = nothing) where{X}
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

Attaches a transition to the previously defined `state()`.

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

"""    done()

This compiles all previous `state()`, `answer()`, etc. invocations
into a self-contained `Dialogue` object.
"""
function done()
	for (i,s) in pairs(global_status.states)
		println("state \e[1m<$i\e[m> \"$(s.text)\":")
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
				println("  \e[32m$(t.action)\e[m")
			end
		end
	end
	reset(global_status)
	return nothing
end

export answer, state, trigger, action, done

end
D = Dialogs
for f in names(D); f == :Dialogs && continue
	eval(:(@inline $f(args...; kwargs...) = D.$f(args...; kwargs...)))
end
# @inline state(args...; kwargs...) = D.state(args...; kwargs...)
# @inline trigger(args...; kwargs...) = D.trigger(args...; kwargs...)

trigger("at night")
state(:begin, "a beautiful night")
 answer("indeed" => 3)
 answer("again?" => :begin)
 answer("good bye" => exit)
  action("continue script here")

state(3, "the stars are aligned")
done()


# better would be:
# x = state("a beautiful night")
# x: "