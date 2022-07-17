module DottedEnums

abstract type DottedEnum end
abstract type DottedFlags end

(T::Type{<:Integer})(x::Union{DottedEnum,DottedFlags}) = T(x.n)

function namemap end

function showtypename(io::IO, tn::Core.TypeName)
	if !get(io, :compact, false)
		from = get(io, :module, Main)
		if isnothing(from) || !Base.isvisible(tn.name, tn.module, from)
			show(io, tn.module)
			print(io, '.', tn.name)
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

function dottedenum(T::Union{Symbol,Expr}, args...; flags=false)
	typename, basetype = T, flags ? UInt32 : Int32
	supert = flags ? :DottedFlags : :DottedEnum
	if Meta.isexpr(T, :(::)) && T.args[1] isa Symbol
		typename, basetype = T.args[1], Core.eval(__module__, T.args[2])
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
			s, i = s.args[1], convert(basetype, Core.eval(__module__, s.args[2]))
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
		Base.@__doc__(struct $tn <: $supert; n::$basetype; end)
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
macro dottedenum(T::Union{Symbol,Expr}, args...)
	dottedenum(T, args...; flags=false)
end

"""    @dottedflags EnumName[::BaseType] name1[=value1] ...

Creates a `DottedFlags` subtype with name `EnumName` and individual
bits `EnumName.name1` etc.
"""
macro dottedflags(T::Union{Symbol,Expr}, args...)
	dottedenum(T, args...; flags=true)
end

end

D=DottedEnums
