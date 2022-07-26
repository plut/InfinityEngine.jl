module DottedEnums
using StaticArrays
#««1 Dotted enums and flags
abstract type EnumOrFlags{T} end
abstract type DottedEnum{T} <: EnumOrFlags{T} end
abstract type DottedFlags{T} <: EnumOrFlags{T} end
@inline basetype(::Type{<:EnumOrFlags{T}}) where{T} = T
(T::Type{<:Integer})(x::EnumOrFlags) = T(x.n)


function namemap end

function showtypename(io::IO, tn::Core.TypeName)
	if !get(io, :compact, false)
		from = get(io, :module, Main)
		if isnothing(from) || !Base.isvisible(tn.name, tn.module, from)
			show(io, tn.module)
			print(io, '.')
		end
	end
	print(io, tn.name)
end
function showenum(io::IO, T::DataType, n::Integer)
	showtypename(io, T.name)
	sym = get(namemap(T), n, nothing)
	if isnothing(sym)
		print(io, '(', repr(n), ')')
	else
		print(io, '.', sym)
	end
end
@inline Base.show(io::IO, x::DottedEnum) = showenum(io, typeof(x), x.n)
function Base.show(io::IO, x::DottedFlags)
	f = one(x.n); b = false
	if iszero(x.n)
		showenum(io, typeof(x), x.n)
		return
	end
	for i in 0:8*sizeof(x)-1
		if !iszero(x.n & f)
			b && print(io, '|'); b = true
			showenum(io, typeof(x), f)
		end
		f<<= 1
	end
end

Base.:|(x::T, y::T) where{T<:DottedFlags} = T(x.n|y.n)
Base.:&(x::T, y::T) where{T<:DottedFlags} = T(x.n&y.n)
Base.:~(x::T) where{T<:DottedFlags} = T(~(x.n))
Base.iszero(x::DottedFlags) = iszero(x.n)
Base.:∈(x::T, y::T) where{T<:DottedFlags} = !iszero(x&y)

function dottedenum(m::Module, T::Union{Symbol,Expr}, args...; flags=false)
	typename, basetype = T, flags ? UInt32 : Int32
	supert = flags ? :DottedFlags : :DottedEnum
	if Meta.isexpr(T, :(::)) && T.args[1] isa Symbol
		typename, basetype = T.args[1], Core.eval(m, T.args[2])
	elseif !(T isa Symbol)
		throw(ArgumentError("invalid type expression for $supert: $T"))
	end
	tn = esc(typename)

	namemap = Dict{basetype, Symbol}()
	namedef = Expr[]
	i = flags ? one(basetype) : zero(basetype)
	isone(length(args)) && Meta.isexpr(args[1], :block) && (args = args[1].args)
	for s in args
		s isa LineNumberNode && continue
		if s isa Symbol # nothing
		elseif Meta.isexpr(s, :(=))
			s, i = s.args[1], convert(basetype, Core.eval(m, s.args[2]))
		else
			throw(ArgumentError("invalid argument for $supert $typename: $s"))
		end
		flags && !iszero(i) && !ispow2(i) &&
			throw(ArgumentError("for $supert, only powers of two (and zero) are allowed"))
		namemap[i] = s
		push!(namedef, :(s == $(QuoteNode(s)) && return $tn($i)))
		if flags
			if iszero(i); i = one(i); else; i<<=1; end
		else
			i+= oneunit(i)
		end
		# a few typeasserts to check:
		@assert Base.isidentifier(s)
		# we don't check for keyword or value unicity
	end
	block = quote
		Base.@__doc__(struct $tn <: $supert{$basetype}
			n::$basetype
			@inline $tn(n::Integer) = new(n)
		end)
		function Base.getproperty(T::Type{$tn}, s::Symbol)
			s ∈ fieldnames(DataType) && return getfield(T, s)
			$(namedef...)
			throw(ArgumentError("unknown value for type "*$(string(typename))*": "*
				string(s)))
		end
		DottedEnums.namemap(::Type{$tn}) = $(esc(namemap))
	end
	block.head = :toplevel
	block
end

"""    @dottedenum EnumName[::BaseType] name1[=value1] ...

Creates a `DottedEnum` subtype with name `EnumName` and enumerated values
`EnumName.name1` etc.

Apart from the syntax, this is similar to `@enum`.
"""
macro DottedEnum(T::Union{Symbol,Expr}, args...)
	dottedenum(__module__, T, args...; flags=false)
end

"""    @DottedFlags EnumName[::BaseType] name1[=value1] ...

Creates a `DottedFlags` subtype with name `EnumName` and individual
bits `EnumName.name1` etc.
"""
macro DottedFlags(T::Union{Symbol,Expr}, args...)
	dottedenum(__module__, T, args...; flags=true)
end
@inline Base.read(io::IO, T::Type{<:EnumOrFlags}) = T(read(io, basetype(T)))

# For serialization, we need extracting a given byte of an enum/flags value,
# and also building one from various flags:
function getbytes(x::EnumOrFlags, r::Union{AbstractUnitRange,Integer})
	@assert first(r) ≥ 1; @assert last(r) ≤ sizeof(x)
	return view(reinterpret(Int8, [x]), r)
end
@inline getbytes(x::EnumOrFlags, r::Union{AbstractUnitRange,Integer},
		T::DataType) = only(reinterpret(T, getbytes(x, r)))
function setbytes(T::Type{<:EnumOrFlags}, vals...)
	t = Vector{UInt8}(undef, sizeof(T))
	i = 0
	for v in vals
		view(t, i+1:i+sizeof(v)) .= reinterpret(eltype(t), [v])
		i+= sizeof(v)
	end
	println("t = $t")
	return T(only(reinterpret(fieldtype(T,:n), t)))
end

#««1 Symbolic flags
# should this be made static?
struct SymbolicFlags; members::Set{Symbol}; end
@inline SymbolicFlags(s::Symbol...) = SymbolicFlags(Set(s))
@inline Base.show(io::IO, f::SymbolicFlags) =
	isempty(f.members) ? print(io, false) : join(io, f.members, '|')
"""    @SymbolicFlags name1 name2 ...
    @SymbolicFlags begin name1 name2 ... end

Declares variables `name1`... to be symbolic names.
Each symbolic name can be converted to a `DottedEnum` type having this
name as a member:

    @DottedEnum Enum1 A B D
    @SymbolicFlags A B C
    Enum1(A) # same as Enum1.A
    Enum1(D) # error

Symbolic names, as well as their boolean combinations under the
operators `|`, `&`, `~` and `⊻`, can be converted to
`DottedFlags` types having these names as members:

    @DottedFlags Flag1 A B C
    Flags(A|B) # same as Flags.A|Flags.B
    Flags(~C)  # FIXME: same as Flags.A|Flags.B
"""
macro SymbolicFlags(names...)
	if length(names) == 1 && Meta.isexpr(names[1], :block)
		names = filter(x-> x isa Symbol, names[1].args)
	end
	symbolicflags(names)
end

"""    SymbolicFlagsFrom type1 type2...

Uses all the declared values from `DottedEnums` and `DottedFlags` types
`type1`, `type2`... as `SymbolicFlags`.
"""
macro SymbolicFlagsFrom(names...)
	vars = Set{Symbol}()
	for name in names
		t = Core.eval(__module__, name)
		push!(vars, values(namemap(t))...)
	end
	symbolicflags(vars)
end

function symbolicflags(names)
	code = [ :($(esc(x)) = $(SymbolicFlags)($(QuoteNode(x)))) for x in names]
	quote $(code...) end
end

struct SymbolicNot; members::Set{Symbol}; end
function Base.show(io::IO, f::SymbolicNot)
	print(io, '~')
	length(f.members) == 0 && print(io, false)
	length(f.members) > 1  &&  print(io, '(')
	join(io, f.members, '|')
	length(f.members) > 1 && print(io, ')')
end

@inline Base.:~(f::SymbolicFlags) = SymbolicNot(f.members)
@inline Base.:~(f::SymbolicNot) = SymbolicFlags(f.members)

@inline Base.:|(f1::SymbolicFlags, f2::SymbolicFlags) =
	SymbolicFlags(f1.members ∪ f2.members)
@inline Base.:|(f1::SymbolicNot, f2::SymbolicNot) =
	SymbolicNot(f1.members ∩ f2.members)
@inline Base.:|(f1::SymbolicFlags, f2::SymbolicNot) =
	SymbolicNot(setdiff(f2.members, f1.members))
@inline Base.:|(f1::SymbolicNot, f2::SymbolicFlags) = f2 | f1

@inline Base.:&(f1::SymbolicFlags, f2::SymbolicFlags) =
	SymbolicFlags(f1.members ∩ f2.members)
@inline Base.:&(f1::SymbolicNot, f2::SymbolicNot) =
	SymbolicNot(f1.members ∪ f2.members)
@inline Base.:&(f1::SymbolicFlags, f2::SymbolicNot) =
	SymbolicFlags(setdiff(f1.members, f2.members))
@inline Base.:&(f1::SymbolicNot, f2::SymbolicFlags) = f2 & f1

@inline Base.:⊻(f1::SymbolicFlags, f2::SymbolicFlags) =
	SymbolicFlags(symdiff(f1.members, f2.members))
@inline Base.:⊻(f1::SymbolicNot, f2::SymbolicNot) =
	SymbolicFlags(symdiff(f1.members, f2.members))
@inline Base.:⊻(f1::SymbolicFlags, f2::SymbolicNot) =
	SymbolicNot(symdiff(f1.members, f2.members))
@inline Base.:⊻(f1::SymbolicNot, f2::SymbolicFlags) = f2 ⊻ f1

Base.convert(T::Type{<:DottedFlags}, f::SymbolicFlags) =
	|(T(0), (getproperty(T, s) for s ∈ f.members)...,)
Base.convert(T::Type{<:DottedFlags}, f::SymbolicNot) =
	~(|(T(0), (getproperty(T, s) for s ∈ f.members)...,))
(T::Type{<:DottedFlags})(f::Union{SymbolicFlags,SymbolicNot}) = convert(T,f)

Base.convert(T::Type{<:DottedEnum}, f::SymbolicFlags) =
	getproperty(T, only(f.members))
(T::Type{<:DottedEnum})(f::SymbolicFlags) = convert(T,f)

Base.:|(f1::DottedFlags, f2::SymbolicFlags) = f1 | typeof(f1)(f2)
Base.:&(f1::DottedFlags, f2::SymbolicFlags) = f1 & typeof(f1)(f2)
Base.:⊻(f1::DottedFlags, f2::SymbolicFlags) = f1 ⊻ typeof(f1)(f2)

Base.:|(f1::SymbolicFlags, f2::DottedFlags) = f2 | f1
Base.:&(f1::SymbolicFlags, f2::DottedFlags) = f2 & f1
Base.:⊻(f1::SymbolicFlags, f2::DottedFlags) = f2 ⊻ f1
#»»1
export @DottedEnum, @DottedFlags, @SymbolicFlags, @SymbolicFlagsFrom
end
