module DottedEnums
using StaticArrays

abstract type EnumOrFlags{T} end

@inline basetype(::Type{<:EnumOrFlags{T}}) where{T} = T
(T::Type{<:Integer})(x::EnumOrFlags) = T(x.n)
@inline Base.read(io::IO, T::Type{<:EnumOrFlags}) = T(read(io, basetype(T)))

function namemap end
function valuemap end
@inline value(T::Type{<:EnumOrFlags}, ::Val) = nothing

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

# #««1 Dotted enums and flags
# abstract type DottedEnum{T} <: EnumOrFlags{T} end
# abstract type DottedFlags{T} <: EnumOrFlags{T} end
# 
# 
# 
# function showenum(io::IO, T::DataType, n::Integer)
# 	sym = get(namemap(T), n, nothing)
# 	get(io, :compact, false) && 
# 		(print(io, isnothing(sym) ? repr(n) : sym); return)
# 	showtypename(io, T.name)
# 	if isnothing(sym)
# 		print(io, '(', repr(n), ')')
# 	else
# 		print(io, '.', sym)
# 	end
# end
# @inline Base.show(io::IO, x::DottedEnum) = showenum(io, typeof(x), x.n)
# function Base.show(io::IO, x::DottedFlags)
# 	f = one(x.n); b = false
# 	if iszero(x.n)
# 		showenum(io, typeof(x), x.n)
# 		return
# 	end
# 	for i in 0:8*sizeof(x)-1
# 		if !iszero(x.n & f)
# 			b && print(io, '|'); b = true
# 			showenum(io, typeof(x), f)
# 		end
# 		f<<= 1
# 	end
# end
# 
# Base.:|(x::T, y::T) where{T<:DottedFlags} = T(x.n|y.n)
# Base.:&(x::T, y::T) where{T<:DottedFlags} = T(x.n&y.n)
# Base.:~(x::T) where{T<:DottedFlags} = T(~(x.n))
# Base.iszero(x::DottedFlags) = iszero(x.n)
# Base.:∈(x::T, y::T) where{T<:DottedFlags} = !iszero(x&y)
# 
# function dottedenum(m::Module, T::Union{Symbol,Expr}, args...; flags=false)
# 	typename, basetype = T, flags ? UInt32 : Int32
# 	supert = flags ? :DottedFlags : :DottedEnum
# 	if Meta.isexpr(T, :(::)) && T.args[1] isa Symbol
# 		typename, basetype = T.args[1], Core.eval(m, T.args[2])
# 	elseif !(T isa Symbol)
# 		throw(ArgumentError("invalid type expression for $supert: $T"))
# 	end
# 	tn = esc(typename)
# 
# 	namemap = Dict{basetype, Symbol}()
# 	namedef = Expr[]
# 	i = flags ? one(basetype) : zero(basetype)
# 	isone(length(args)) && Meta.isexpr(args[1], :block) && (args = args[1].args)
# 	for s in args
# 		s isa LineNumberNode && continue
# 		if s isa Symbol # nothing
# 		elseif Meta.isexpr(s, :(=))
# 			s, i = s.args[1], convert(basetype, Core.eval(m, s.args[2]))
# 		else
# 			throw(ArgumentError("invalid argument for $supert $typename: $s"))
# 		end
# 		flags && !iszero(i) && !ispow2(i) &&
# 			throw(ArgumentError("for $supert, only powers of two (and zero) are allowed"))
# 		namemap[i] = s
# 		push!(namedef, :(s == $(QuoteNode(s)) && return $tn($i)))
# 		if flags
# 			if iszero(i); i = one(i); else; i<<=1; end
# 		else
# 			i+= oneunit(i)
# 		end
# 		# a few typeasserts to check:
# 		@assert Base.isidentifier(s)
# 		# we don't check for keyword or value unicity
# 	end
# 	block = quote
# 		Base.@__doc__(struct $tn <: $supert{$basetype}
# 			n::$basetype
# 			@inline $tn(n::Integer) = new(n)
# 		end)
# 		function Base.getproperty(T::Type{$tn}, s::Symbol)
# 			s ∈ fieldnames(DataType) && return getfield(T, s)
# 			$(namedef...)
# 			throw(ArgumentError("unknown value for type "*$(string(typename))*": "*
# 				string(s)))
# 		end
# 		DottedEnums.namemap(::Type{$tn}) = $(esc(namemap))
# 	end
# 	block.head = :toplevel
# 	block
# end
# 
# """    @dottedenum EnumName[::BaseType] name1[=value1] ...
# 
# Creates a `DottedEnum` subtype with name `EnumName` and enumerated values
# `EnumName.name1` etc.
# 
# Apart from the syntax, this is similar to `@enum`.
# """
# macro DottedEnum(T::Union{Symbol,Expr}, args...)
# 	dottedenum(__module__, T, args...; flags=false)
# end
# 
# """    @DottedFlags EnumName[::BaseType] name1[=value1] ...
# 
# Creates a `DottedFlags` subtype with name `EnumName` and individual
# bits `EnumName.name1` etc.
# """
# macro DottedFlags(T::Union{Symbol,Expr}, args...)
# 	dottedenum(__module__, T, args...; flags=true)
# end
# @inline Base.read(io::IO, T::Type{<:EnumOrFlags}) = T(read(io, basetype(T)))
# 
# # For serialization, we need extracting a given byte of an enum/flags value,
# # and also building one from various flags:
# function getbytes(x::EnumOrFlags, r::Union{AbstractUnitRange,Integer})
# 	@assert first(r) ≥ 1; @assert last(r) ≤ sizeof(x)
# 	return view(reinterpret(Int8, [x]), r)
# end
# @inline getbytes(x::EnumOrFlags, r::Union{AbstractUnitRange,Integer},
# 		T::DataType) = only(reinterpret(T, getbytes(x, r)))
# function setbytes(T::Type{<:EnumOrFlags}, vals...)
# 	t = Vector{UInt8}(undef, sizeof(T))
# 	i = 0
# 	for v in vals
# 		view(t, i+1:i+sizeof(v)) .= reinterpret(eltype(t), [v])
# 		i+= sizeof(v)
# 	end
# 	println("t = $t")
# 	return T(only(reinterpret(fieldtype(T,:n), t)))
# end
# 
#««1 Symbolic flags/enums
#««2 Types and utility functions
abstract type SymbolicEnum{T} <: EnumOrFlags{T} end
abstract type SymbolicFlags{T} <: EnumOrFlags{T} end

Base.:|(x::T, y::T) where{T<:SymbolicFlags} = T(x.n|y.n)
Base.:&(x::T, y::T) where{T<:SymbolicFlags} = T(x.n&y.n)
Base.:~(x::T) where{T<:SymbolicFlags} = T(~(x.n))
Base.iszero(x::SymbolicFlags) = iszero(x.n)
Base.:∈(x::T, y::T) where{T<:SymbolicFlags} = !iszero(x&y)


enumtext(namemap, n) = get(namemap, n, n)
function flagstext(namemap, n)
	s = ""
	f = one(n); b = false; n = n
	if iszero(n)
		return string(get(namemap, zero(n), 0))
	else
		for i in 1:8*sizeof(n)
			t = get(namemap, f, nothing)
			if !iszero(n & f) && !isnothing(t)
				b && (s*= '|'); b = true
				s*= String(t)
				n&=~f
			end
			f<<= 1
		end
		# remainder: unnamed flags
		if !iszero(n)
			b && (s*='|')
			s*= "0x"*string(n, base=16)
		end
	end
	s
end

function Base.show(io::IO, x::T) where{T<:SymbolicEnum}
	long = !get(io, :compact, false)
	long &&  (showtypename(io, T.name); print(io, '('))

	print(io,  enumtext(namemap(T), x.n))
	long && print(io, ')')
end

function Base.show(io::IO, x::T) where{T<:SymbolicFlags}
	long = !get(io, :compact, false)
	long &&  (showtypename(io, T.name); print(io, '('))

	print(io, flagstext(namemap(T), x.n))
	long && print(io, ')')
end

#««2 Constructor macro
function symbolicenum(m::Module, T::Union{Symbol, Expr}, args...; isflags=false)
	typename, basetype = T, isflags ? UInt32 : Int32
	supert = isflags ? :SymbolicFlags : :SymbolicEnum
	if Meta.isexpr(T, :(::)) && T.args[1] isa Symbol
		typename, basetype = T.args[1], Core.eval(m, T.args[2])
	elseif !(T isa Symbol)
		throw(ArgumentError("invalid type expression for $supert: $T"))
	end
	tn = esc(typename)
	namemap = Dict{basetype, Symbol}()
	valuemap = Dict{Symbol, basetype}()
	allbits = zero(basetype)
	namedef = Expr[]
	i = isflags ? one(basetype) : zero(basetype)
	isone(length(args)) && Meta.isexpr(args[1], :block) && (args = args[1].args)
	for s in args
		s isa LineNumberNode && continue
		if s isa Symbol # nothing
		elseif Meta.isexpr(s, :(=))
			s, i = s.args[1], convert(basetype, Core.eval(m, s.args[2]))
		else
			throw(ArgumentError("invalid argument for $supert $typename: $s"))
		end
		isflags && !iszero(i) && !ispow2(i) &&
			throw(ArgumentError("for $supert, only powers of two (and zero) are allowed"))
		namemap[i] = s
		valuemap[s] = i
		i = isflags ? (iszero(i) ? one(i) : i<<1) : i + oneunit(i)
		@assert Base.isidentifier(s)
		# We don't check for keyword or value unicity
	end
	block = quote
		Base.@__doc__(struct $tn <: $supert{$basetype}
			n::$basetype
			@inline $tn(n::Integer) = new(n)
		end)
		DottedEnums.namemap(::Type{$tn}) = $(esc(namemap))
	end
	for (k,v) in pairs(valuemap)
		allbits |= v
		push!(block.args, quote
		$(@__MODULE__).value(::Type{$tn}, ::Val{$(QuoteNode(k))}) = $v
		if isdefined(@__MODULE__, $(QuoteNode(k)))
			isa($(esc(k)), $SymbolicNames) ||
			error("Cannot define SymbolicName ", $(string(k)))
		else
# 			const $(esc(k)) = $SymbolicNames(Set([$(QuoteNode(k))]))
			const $(esc(k)) = $SymbolicNames{($(QuoteNode(k)),)}()
		end
		@inline Base.convert(::Type{$tn}, ::$SymbolicNames{($(QuoteNode(k)),)}) =
			$tn($v)
		end)
	end
	push!(block.args, tn)
	isflags && push!(block.args, quote
		@inline Base.convert(::Type{$tn}, ::$SymbolicNot{()}) = $tn($allbits)
	end)
	block
end
macro SymbolicEnum(T::Union{Symbol,Expr},args...)
	symbolicenum(__module__, T, args...; isflags=false)
end
macro SymbolicFlags(T::Union{Symbol,Expr},args...)
	symbolicenum(__module__, T, args...; isflags=true)
end
#««1 Symbolic names
struct SymbolicNames{X} end
struct SymbolicNot{X} end
@inline SymbolicNames(x::Symbol...) = SymbolicNames{x}()
@inline SymbolicNot(x::Symbol...) = SymbolicNot{x}()
@inline members(::SymbolicNames{X}) where{X} = X
@inline members(::SymbolicNot{X}) where{X} = X
# struct SymbolicNames; members::Set{Symbol}; end
# struct SymbolicNot; members::Set{Symbol}; end
NamesOrNot = Union{SymbolicNames,SymbolicNot}
# @inline SymbolicNames(s::Symbol...) = SymbolicNames(Set(s))
@inline Base.show(io::IO, f::SymbolicNames) =
	isempty(members(f)) ? print(io, false) : join(io, members(f), '|')
# """    @SymbolicNames name1 name2 ...
#     @SymbolicNames begin name1 name2 ... end
# 
# Declares variables `name1`... to be symbolic names.
# Each symbolic name can be converted to a `DottedEnum` type having this
# name as a member:
# 
#     @DottedEnum Enum1 A B D
#     @SymbolicNames A B C
#     Enum1(A) # same as Enum1.A
#     Enum1(D) # error
# 
# Symbolic names, as well as their boolean combinations under the
# operators `|`, `&`, `~` and `⊻`, can be converted to
# `DottedFlags` types having these names as members:
# 
#     @DottedFlags Flag1 A B C
#     Flags(A|B) # same as Flags.A|Flags.B
#     Flags(~C)  # FIXME: same as Flags.A|Flags.B
# """
# macro SymbolicNames(names...)
# 	if length(names) == 1 && Meta.isexpr(names[1], :block)
# 		names = filter(x-> x isa Symbol, names[1].args)
# 	end
# 	symbolicflags(names)
# end
# 
# """    SymbolicNamesFrom type1 type2...
# 
# Uses all the declared values from `DottedEnums` and `DottedFlags` types
# `type1`, `type2`... as `SymbolicNames`.
# """
# macro SymbolicNamesFrom(names...)
# 	vars = Set{Symbol}()
# 	for name in names
# 		t = Core.eval(__module__, name)
# 		push!(vars, values(namemap(t))...)
# 	end
# 	symbolicflags(vars)
# end
# 
# function symbolicflags(names)
# 	code = [ :($(esc(x)) = $(SymbolicNames)($(QuoteNode(x)))) for x in names]
# 	quote $(code...) end
# end
function Base.show(io::IO, f::SymbolicNot)
	print(io, '~')
	length(members(f)) == 0 && print(io, false)
	length(members(f)) > 1  &&  print(io, '(')
	join(io, members(f), '|')
	length(members(f)) > 1 && print(io, ')')
end

@inline Base.:~(f::SymbolicNames) = SymbolicNot(members(f)...)
@inline Base.:~(f::SymbolicNot) = SymbolicNames(members(f)...)

@inline usort(x) = x|>collect|>sort|>unique
@inline Base.:|(f1::SymbolicNames, f2::SymbolicNames) =
	SymbolicNames(usort(members(f1) ∪ members(f2))...)
@inline Base.:|(f1::SymbolicNot, f2::SymbolicNot) =
	SymbolicNot(usort(members(f1) ∩ members(f2))...)
@inline Base.:|(f1::SymbolicNames, f2::SymbolicNot) =
	SymbolicNot(usort(setdiff(members(f2), members(f1)))...)
@inline Base.:|(f1::SymbolicNot, f2::SymbolicNames) = f2 | f1

@inline Base.:&(f1::SymbolicNames, f2::SymbolicNames) =
	SymbolicNames(usort(members(f1) ∩ members(f2))...)
@inline Base.:&(f1::SymbolicNot, f2::SymbolicNot) =
	SymbolicNot(usort(members(f1) ∪ members(f2))...)
@inline Base.:&(f1::SymbolicNames, f2::SymbolicNot) =
	SymbolicNames(usort(setdiff(members(f1), members(f2)))...)
@inline Base.:&(f1::SymbolicNot, f2::SymbolicNames) = f2 & f1

@inline Base.:⊻(f1::SymbolicNames, f2::SymbolicNames) =
	SymbolicNames(usort(symdiff(members(f1), members(f2)))...)
@inline Base.:⊻(f1::SymbolicNot, f2::SymbolicNot) =
	SymbolicNames(usort(symdiff(members(f1), members(f2)))...)
@inline Base.:⊻(f1::SymbolicNames, f2::SymbolicNot) =
	SymbolicNot(usort(symdiff(members(f1), members(f2)))...)
@inline Base.:⊻(f1::SymbolicNot, f2::SymbolicNames) = f2 ⊻ f1

@inline Base.convert(T::Type{<:SymbolicFlags}, f::SymbolicNames) =
	T(|(0, (convert(T, SymbolicNames(s)).n for s ∈ members(f))...))
@inline Base.convert(T::Type{<:SymbolicFlags}, f::SymbolicNot) =
	T(SymbolicNot()) & ~T(SymbolicNames(members(f)...))
# @inline Base.convert(T::Type{<:SymbolicFlags}, f::SymbolicNames) =
# 	T(|(0, (value(T, Val(s)) for s ∈ members(f))...))
# @inline Base.convert(T::Type{<:SymbolicFlags}, f::SymbolicNot) =
# 	~T(|(0, (value(T, Val(s)) for s ∈ members(f))...))
# @inline Base.convert(T::Type{<:SymbolicEnum}, f::SymbolicNames) =
# 	T(value(T, Val(only(members(f)))))
@inline (T::Type{<:EnumOrFlags})(f::NamesOrNot) = convert(T,f)

@inline Base.:|(f1::SymbolicFlags, f2::NamesOrNot) = f1 | typeof(f1)(f2)
@inline Base.:&(f1::SymbolicFlags, f2::NamesOrNot) = f1 & typeof(f1)(f2)
@inline Base.:⊻(f1::SymbolicFlags, f2::NamesOrNot) = f1 ⊻ typeof(f1)(f2)

@inline Base.:|(f1::NamesOrNot, f2::SymbolicFlags) = f2 | f1
@inline Base.:&(f1::NamesOrNot, f2::SymbolicFlags) = f2 & f1
@inline Base.:⊻(f1::NamesOrNot, f2::SymbolicFlags) = f2 ⊻ f1

@inline Base.:∈(f1::SymbolicNames, f2::SymbolicFlags) = oftype(f2, f1) ∈ f2
@inline Base.contains(f1::SymbolicFlags, f2::SymbolicNames) = f2 ∈ f1

@inline Base.:(==)(e::T, f::NamesOrNot) where{T<:EnumOrFlags} = convert(T,f)==e

# Base.convert(T::Type{<:DottedFlags}, f::SymbolicNames) =
# 	|(T(0), (getproperty(T, s) for s ∈ f.members)...,)
# Base.convert(T::Type{<:DottedFlags}, f::SymbolicNot) =
# 	~(|(T(0), (getproperty(T, s) for s ∈ f.members)...,))
# (T::Type{<:DottedFlags})(f::Union{SymbolicNames,SymbolicNot}) = convert(T,f)
# 
# Base.convert(T::Type{<:DottedEnum}, f::SymbolicNames) =
# 	getproperty(T, only(f.members))
# (T::Type{<:DottedEnum})(f::SymbolicNames) = convert(T,f)
# 
# Base.:(==)(e::T, f::Union{SymbolicNames,SymbolicNot}
# 	) where{T<:Union{DottedEnum,DottedFlags}} = convert(T,f) == e

# Base.:|(f1::DottedFlags, f2::SymbolicNames) = f1 | typeof(f1)(f2)
# Base.:&(f1::DottedFlags, f2::SymbolicNames) = f1 & typeof(f1)(f2)
# Base.:⊻(f1::DottedFlags, f2::SymbolicNames) = f1 ⊻ typeof(f1)(f2)
# 
# Base.:|(f1::SymbolicNames, f2::DottedFlags) = f2 | f1
# Base.:&(f1::SymbolicNames, f2::DottedFlags) = f2 & f1
# Base.:⊻(f1::SymbolicNames, f2::DottedFlags) = f2 ⊻ f1
#»»1
# export @DottedEnum, @DottedFlags
export @SymbolicEnum, @SymbolicFlags
# export @SymbolicNames, @SymbolicNamesFrom
end
