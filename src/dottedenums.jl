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

NamesOrNot = Union{SymbolicNames,SymbolicNot}
@inline Base.show(io::IO, f::SymbolicNames) =
	isempty(members(f)) ? print(io, false) : join(io, members(f), '|')
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

#»»1
export @SymbolicEnum, @SymbolicFlags
end
