"""    Pack

Module defining functions for (TODO) packing/unpacking structures from binary
files in a controlled way (i.e. conforming to a binary format specification).

See `pack`, `unpack`, `packed_layout`, `@pack`.
"""
module Pack
using StaticArrays
using Printf
using Base.Meta: isexpr

#««1 Layout
struct Layout
	sizes::Vector{Int}
	types::Vector{DataType}
	names::Vector{String}
end
"""    packed_layout(object)

Prints the binary layout of the object.

---
    packed_layout(type)

Prints the binary layout of the type. Some data will be variable
(depending on the length of vector fields) and printed as '????'.
"""
function packed_layout(T::DataType)
	# default value for non-`@pack` types
	isstructtype(T) || return Layout([sizeof(T)], [T], [""])
	l = Layout([], [], [])
	for (fn, ft) in zip(fieldnames(T), fieldtypes(T))
		push!(l.sizes, sizeof(ft))
		push!(l.types, ft)
		push!(l.names, string(fn))
	end
	return l
end
function Base.show(io::IO, ::MIME"text/plain", l::Layout)
	offset = 0
	for (fs, ft, fn) in zip(l.sizes, l.types, l.names)
		s1 = fs < 0 ? "????" : @sprintf("%4d", fs)
		o1 = offset < 0 ? "0x????" : @sprintf("0x%04x", offset)
		@printf("\e[1m%s\e[m \e[31m%s\e[m \e[36m%-16s\e[m %s\n",
			o1, s1, string(ft), string(fn))
		offset = (fs < 0 || offset < 0) ? -1 : offset + fs
	end
end

#««1 Pack/unpack: default behaviour
"""    unpack(io, T)

Unpacks an object from a binary IO according to its packed layout.

---
    unpack(io, T, n)

Unpacks `n` objects and returns a vector.
"""
function unpack(io::IO, T::DataType)
	# default value for non-`@pack` types
	isstructtype(T) || return read(io, T)
	fieldvars = [ unpack(io, ft) for ft in fieldtypes(T) ]
	return T <: Tuple ? T((fieldvars...,),) : T(fieldvars...)
end
#««1 @pack: special behaviour for structures
"""    @pack [struct definition]

Defines a struct type together with methods for (TODO) packing to,
and unpacking from, a binary IO representation. Any valid struct definition
can be passed, with the following extra fields:

 - `"constant"`: when unpacking, a string of the given length will be read
   and compared with the constant; an error will be thrown if it does not match.
 - `length(field)::type`, `offset(field)::type`: these fields encode the length
   and (optionally) offset of a field given below, of vector type.

This macro defines the stucture (eliminating the fictious fields listed above)
as well as ad-hoc methods for `unpack` and `packed_layout`.
"""
macro pack(defn)
	@assert isexpr(defn, :struct) && length(defn.args) == 3
	# args[1] is the mutable flag
	# args[2] is the struct header, which is either
	#   structname or <: ( structname, supertype)
	structheader = defn.args[2]
	structname = isexpr(structheader,:(<:)) ? structheader.args[1] : structheader
	if isexpr(structname, :curly)
		typeparams = structname.args[2:end]
		structname = Expr(:curly, esc(structname.args[1]), typeparams...)
	else
		typeparams = ()
		structname = esc(structname)
	end
	
	@assert isexpr(defn.args[3], :block)
	offsets, lengths = (Dict{Symbol,Symbol}() for _ in 1:2)
	structcode = []
	readcode = []
	typelayoutcode = []
	vallayoutcode = []
	fieldvar = fn -> Symbol("field_"*string(fn))
	fieldvars = Symbol[]
	unpack=:($(@__MODULE__).$(:unpack))
	packed_layout=:($(@__MODULE__).$(:packed_layout))

	for f in defn.args[3].args
		# skip this
		f isa LineNumberNode && continue
		# constructor
		(isexpr(f,:(=)) || isexpr(f,:function)) && (push!(structcode, f); continue)
		if isexpr(f, :(::))
# 			f.args[2] = Core.eval(__module__,
# 				:($(f.args[2]) where {$(typeparams...)}))
			fn, ftn = f.args
			ft = Core.eval(__module__, :($ftn where {$(typeparams...)}))
			f.args[2] = ft
			@assert ft isa DataType
			if fn isa Symbol
				fv = fieldvar(fn)
				push!(fieldvars, fv)
				push!(structcode, f)
				ftn ∈ typeparams && (ft = ftn)
				if isexpr(ftn, :curly) && ftn.args[1] == :Vector
					len = get(lengths, fn, nothing)
					@assert !isnothing(len) "length not found for vector field $fn"
					off = get(offsets, fn, nothing)
					seek_io = isnothing(off) ? :(io) : :(seek(io, $(off)))
					elt = ftn.args[2]
					push!(readcode, :($fv = $unpack($seek_io, $(esc(elt)), $(len))))
					push!(typelayoutcode, :(push!(l.sizes, -1)))
					push!(vallayoutcode, :(push!(l.sizes, length(x.$fn)*sizeof($elt))))
				else
					push!(readcode, :($fv = unpack(io, $ft)))
					push!(typelayoutcode, :(push!(l.sizes, sizeof($ft))))
					push!(vallayoutcode, :(push!(l.sizes, sizeof($ft))))
				end
			elseif isexpr(fn, :call)
				ff, fa = fn.args; fv = gensym()
				if ff == :offset
					offsets[fa] = fv
				elseif ff == :length
					lengths[fa] = fv
				else
					error("unknown function: $ff")
				end
				push!(readcode, :($fv = read(io, $ftn)))
				push!(vallayoutcode, :(push!(l.sizes, sizeof($ftn))))
			end
		else
			fn, fs, ft = f, sizeof(f), typeof(f)
			ft = Core.eval(__module__, ft)
			push!(readcode, quote
				tmp = $ft(read(io, $(ft == String ? fs : ft)))
				@assert tmp == $f "expected \""*$f*"\", found instead \""*tmp*"\""
			end)
			push!(vallayoutcode, :(push!(l.sizes, $fs)))
			push!(typelayoutcode, :(push!(l.sizes, $fs)))
			fn = repr(fn)
		end
		layoutline = :(push!(l.types, $(esc(ft))); push!(l.names, $(string(fn))))
		push!(vallayoutcode, layoutline)
		push!(typelayoutcode, layoutline)
	end
	defn.args[3] = Expr(:block, structcode...)
	expr = quote
$defn
$unpack(io::IO, T::DataType, n::Integer) = [ $unpack(io, T) for _ in 1:n ]

function $unpack(io::IO, ::Type{$structname}) where{$(typeparams...)}
	$(readcode...)
	return $structname($(fieldvars...))
end
function $packed_layout(::Type{$structname}) where{$(typeparams...)}
	l = Layout([], [], [])
	$(typelayoutcode...)
	return l
end
function $packed_layout(x::$structname) where{$(typeparams...)}
	l = Layout([], [], [])
	$(vallayoutcode...)
	return l
end
	end
	expr
end
#««1 kwconstructor
"""    kwconstructor(T; keywords...)

Defines a keyword-based constructor according to the field names for this type.

This function tries to provide sensible initialization for fields not covered
by keywords: zero for integers, "" for strings, and `X()` for fields
of type `X` (this works at least for arrays and dictionaries).
Where not useful, this behaviour can be overriden by default keywords.

It is recommended to use this function to define a keyword constructor
with default values in this way:

    T(; kwargs...) = kwconstructor(T; field1=defaultvalue1, kwargs...)

The merge behaviour will then make `kwargs.field1`, if given,
override the supplied `defaultvalue1`. This allows some mimicking
of `Parameters`' `@with_kw` macro.
"""
@generated function kwconstructor(::Type{T}; kwargs...) where{T}
	args = [ :(get(kwargs, $(QuoteNode(fn)), kwconstructor($ft)))
		for (fn, ft) in zip(fieldnames(T), fieldtypes(T)) ]
	# the tuple test is there to cover NamedTuples; this function is quite
	# useless for ordinary Tuples:
	return T <: Tuple ? :($T(($(args...),))) : :($T($(args...)))
end
# sensible default values:
# (the @generated function above already works for arrays)
@inline kwconstructor(::Type{T}) where{T<:String} = T("")
@inline kwconstructor(::Type{T}) where{T<:Integer} = zero(T)

#»»1
export packed_layout, unpack, @pack

end

# Pack.@pack mutable struct Foo{X}
# 	"1234"
# 	header::X
# 	length(a)::Int8
# 	offset(a)::Int8
# 	a::Vector{Int8}
# 	b::Int8
# 	Foo{X}(h, a, b) where{X} = new{X}(h,a,b)
# end
# 
# tmp=open("8") do io; unpack(io,Foo{Int8}); end


