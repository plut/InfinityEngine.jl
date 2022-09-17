"""    Pack

Module defining functions for (TODO) packing/unpacking structures from binary
files in a controlled way (i.e. conforming to a binary format specification).

See `pack`, `unpack`, `packed_layout`, `@pack`.
"""
module Pack
using StaticArrays
using Printf
using Base.Meta: isexpr

# for efficiency we make this generated:
@generated fieldnt(::Type{T}) where{T} = Expr(:tuple,
	(:(($(QuoteNode(n)), $t)) for (n, t) in zip(fieldnames(T), fieldtypes(T)))...)


function readfields end
"""    unpack(io, T)

Extends `Base.read`: unserializes an object of type `T` from IO."""
@inline unpack(io::IO, T::DataType, args...; kwargs...) =
	T(readfields(io, T, args...; kwargs...)...)
# Tuple types do not have a constructor, so we need to explicitly build
# the tuple:
@inline unpack(io::IO, T::Type{<:Tuple}, args...; kwargs...) =
	T((readfields(io, T, args...; kwargs...)...,))
@inline unpack(io::IO, T::Base.BitIntegerType) = read(io, T)

"""    unpack!(io, array, n)

Unpacks `n` objects to the given vector, resizing it in the process."""
@inline function unpack!(io::IO, array::AbstractVector{T}, n::Integer,
		args...; kwargs...) where{T}
	resize!(array, n)
	for i in 1:n; array[i] = unpack(io, T, args...; kwargs...); end
	array
end
@inline vecunpack(io::IO, T::DataType, n::Integer) =
	(v = Vector{T}(undef, n); for i in 1:n; v[i] = unpack(io, T); end; v)

function packfields end
"""    pack(io, x)

Extends `Base.write`: serializes object `x` to IO
and returns number of bytes written."""
@inline pack(io::IO, x::Base.BitInteger) = write(io, x)
@inline pack(io::IO, x::Union{Tuple,AbstractVector}) =
	sum(pack(io, y) for y in x; init=0)
@inline pack(io::IO, x) = pack(io, packfields(x))
# by default, Arrays pack as empty

"""    @donotpack type fields...

Generates `pack` and `unpack` functions for `type` in which the listed
`fields` are neither serialized nor unserialized to IO. Instead those fields
are ignored in `pack` and must be provided in `unpack`, as extra
arguments (in the same order in which they are listed in `@donotpack`).
"""
macro donotpack(type, fields::Symbol...) quote
	@generated function Pack.readfields($(esc(:io))::IO, ::Type{T};
		$(esc.(fields)...)) where{T<:$(esc(type))}
		code = [ fn ∈ ($(QuoteNode.(fields)...),) ? fn : :($unpack(io, $ft))
			for (fn, ft) in zip(fieldnames(T), fieldtypes(T)) ]
		Expr(:tuple, code...)
# 		code = []
# 		fieldvars = [ Symbol(:field, i) for i in 1:fieldcount(T) ]
# 		for (fn, ft, fv) in zip(fieldnames(T), fieldtypes(T), fieldvars)
# 			push!(code, :(println("at position ", position(io), ": ", $(string(fn)))))
# 			push!(code, Expr(:(=), fv, fn ∈ ($(QuoteNode.(fields)...),) ? fn : :($unpack(io, $ft))))
# 		end
# 		:(($(code...); ($(fieldvars...),)))
	end
	@generated function Pack.packfields($(esc(:T))::$(esc(type)))
		vals = [ :(T.$fn) for (fn, ft) in zip(fieldnames(T), fieldtypes(T))
			if fn ∉ ($(QuoteNode.(fields)...),) ]
		Expr(:tuple, vals...)
	end
end end

# default case: no fields are excluded.
@donotpack Any
#««1 Layout/packed_sizeof
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
	for (fn, ft) in fieldnt(T)
		ft <: Array && break
		s = packed_sizeof(T, Val(fn), ft)
		iszero(s) && continue
		push!(l.sizes, s)
		push!(l.types, ft)
		push!(l.names, string(fn))
	end
	return l
end
function Base.show(io::IO, ::MIME"text/plain", l::Layout)
	offset = 0
	for (fs, ft, fn) in zip(l.sizes, l.types, l.names)
		s1 = fs < 0 ? "????" : @sprintf("%4d", fs)
		o1 = offset < 0 ? "????" : @sprintf("0x%04x=%04d ", offset, offset)
		@printf("\e[1m%s\e[m \e[31m%s\e[m \e[36m%-16s\e[m %s\n",
			o1, s1, string(ft), string(fn))
		offset = (fs < 0 || offset < 0) ? -1 : offset + fs
	end
	println("total size = $offset")
end
"""    packed_sizeof(T)

Returns the size of the packed representation of type `T`.
"""
@inline packed_sizeof(st, fn, ft::DataType) = packed_sizeof(ft)
@inline function packed_sizeof(T::DataType)
	isstructtype(T) || return sizeof(T)
	r = 0
	for (fn, ft) in fieldnt(T)
		r+= packed_sizeof(T, Val(fn), ft) #
	end
	return r
end

# #««1 Unpack
# """    unpack(io, T)
# 
# Unpacks an object from a binary IO according to its packed layout.
# 
# ---
#     unpack(io, T, n::Integer)
# 
# Unpacks `n` objects and returns a vector.
# """
# @generated function unpack(io::IO, ::Type{T}) where{T}
# 	isstructtype(T) || return :(read(io, T))
# 	fv = [ Symbol("f$i") for i in 1:fieldcount(T) ]
# 	code = [ :(unpack(io, $T, $(Val(n)), $t))
# 		for (v, n, t) in zip(fv, fieldnames(T), fieldtypes(T)) ]
# 	T <: Tuple ? Expr(:call, T, :(($(code...),))) : Expr(:new, T, code...)
# end
# # @inline function unpack(io::IO, T::DataType)
# # # 	if T <: Main.InfinityEngine.RootedResource
# # # 		println("\e[34m $T: $(position(io)) $(typeof(io))\e[m")
# # # 	end
# # # 	@assert !(T<:Main.InfinityEngine.ITM_hdr && position(io) == 1036)
# # 	# default value for non-`@pack` types
# # # 	println("   unpacking \e[31m$T\e[m at position \e[34m$(position(io))\e[m")
# # 	isstructtype(T) || return read(io, T)
# # # 	fieldvars = []
# # # 	for (ft, fn) in zip(fieldtypes(T), fieldnames(T))
# # # 		println("\e[31;1m$T\e[m: field $ft at positoin \e[32m$(position(io))\e[m")
# # # 		push!(fieldvars, unpack(io, ft))
# # # 	end
# # 	fieldvars = (unpack(io, T, Val(fn), ft) for (fn, ft) in fieldnt(T))
# # # 	fieldvars = [ unpack(io, ft) for ft in fieldtypes(T) ]
# # 	return T <: Tuple ? T((fieldvars...,),) : T(fieldvars...)
# # end
# """    unpack(io, structtype, ::Val{fieldname}, fieldtype)
# 
# Hook allowing the user to override `unpack`'s behaviour for a specific field.
# """
# @inline unpack(io::IO, st, fn, ft) =
# begin
# # 	st <: Main.InfinityEngine.RootedResource &&
# # 	println("general unpack: $st/$fn/$ft @$(position(io))")
# # 	if fn == Val(:root)
# # 		println("\e[31;7m unpack(root) in $st/$fn/$ft\n$(typeof(io))\e[m")
# # 	end
# 	unpack(io, ft)
# # 	println("  now @$(position(io))")
# # 	x
# end
# 
# @inline vecunpack(io::IO, T::DataType, n::Integer) =
# 	(v = Vector{T}(undef, n); for i in 1:n; v[i] = unpack(io, T); end; v)
# unpack(io::IO, T::Type{<:Vector}) = eltype(T)[] # sensible default behavior
# @inline unpack(io::IO, T::Type{<:Dict}) = T()
# @inline unpack(io::IO, ::Type{String}) = ""
# @inline vecunpack(filename::AbstractString, T::DataType, n::Integer...) =
# 	open(filename) do io; unpack(io, T, n...); end
# 
# #««1 Pack
# """    pack(io, x)
# 
# Packs an object to a binary IO according to its packed layout.
# """
# function pack(io::IO, x::T) where{T}
# # 	println("packing type $T")
# # 	@assert length(string(T)) ≤ 120 # prevents deep recursion
# # 	isstructtype(T) || (println("   value is $x::$T"); return write(io,x))
# 	isstructtype(T) || return write(io, x)
# 	s = 0
# 	for i in 1:fieldcount(T)
# 		fn, ft = fieldname(T,i), fieldtype(T,i)
# # 		if contains(string(T), r"TLK_str")
# # 			@printf("\e[1m0x%x=%d %s.%s::%s\e[m\n", position(io), position(io),
# # 				T, fn, ft)
# # # 			println("\e[1mx$(string(position(io), base=16))=$(position(io)) $T.$fn::$ft \e[m = $(getfield(x,i))")
# # # 			dump(getfield(x,i);maxdepth=2)
# #  		end
# 		s+= fieldpack(io, T, Val(fn), getfield(x, i))
# # 		if contains(string(T), r"TLK_str")
# # 		@printf("  -> %x\n", position(io))
# # 		end
# 	end
# 	return s
# # 	sum(fieldpack(io, ft, Val(fn), getfield(x, fn))
# # 		for (fn, ft) in zip(fieldnames(T), fieldtypes(T)))
# end
# 
# """    fieldpack(io, structtype, Val(fieldname), value)
# 
# Hook allowing the user to override `unpack`'s behaviour for a specific field.
# """
# @inline fieldpack(io::IO, st, fn, x) = fieldpack0(io, st, fn, x)
# @inline fieldpack0(io::IO, st, fn, x) =
# begin
# # 	if contains(string(st), "ITM") && contains(string(typeof(x)), "ITM")
# # 		println("\e[35;1m $st.$fn::$(typeof(x))\e[m")
# # 	end
# 	pack(io, x)
# end
# 
# @inline pack(io::IO, x::AbstractVector) =
# begin
# 	for (i,y) in pairs(x)
# # 		if contains(string(eltype(x)), r"TLK_str")
# # 			@printf("@ 0x%x=%d: i=%d\n", position(io), position(io), i)
# # 			@assert i ≤ 10
# # 		end
# 		pack(io, y)
# 	end
# 	0
# # 	sum(pack(io, y) for y in x; init = 0)
# end
# 	
# # function pack(io::IO, x::Vector)
# # 	n = 0
# # 	@assert eltype(x) != UInt8 || length(x) < 63000
# # 	for (i,y) in pairs(x)
# # 		n+= pack(io, y)
# # 	end
# # 	n
# # end
# @inline pack(io::IO, ::Dict) = 0
# pack(io::IO, ::String) = 0
# 
# ««1 Constant fields
"""    Constant{V}

Zero-size field which always `pack`s to the same value."""
struct Constant{V} end
@inline Base.convert(T::Type{<:Constant}, ::Tuple{}) = T()

@inline value(::Type{Constant{V}}) where{V} = V

@inline function unpack(io::IO, ::Type{Constant{V}}) where{V}
	x = read(io, sizeof(V))
	@assert x == V "Bad value \""*String(x)*"\" (should have been \""*String(V)*"\")"
	return Constant{V}()
end
@inline pack(io::IO, ::Constant{V}) where{V} = write(io, V)
@inline packed_sizeof(::Type{<:Constant{V}}) where{V} = sizeof(V)

macro Constant_str(s); :(Constant{SA[$(codeunits(s)...)]}); end

#««1 PositionAssert
"""    PositionAssert{N}

Zero-size type which unpacks and packs to an assertion
that current IO position is `N`."""
struct PositionAssert{N} end
@inline Base.position(::Type{PositionAssert{N}}) where{N} = N
@inline function assert(io::IO, T::Type{<:PositionAssert})
	x, y = position(io), position(T)
	@assert x == y "Bad IO position: "*string(x)*", should be "*string(y)
end
@inline pack(io::IO, a::PositionAssert) = (assert(io, typeof(a)); 0)
@inline unpack(io::IO, T::Type{<:PositionAssert}) = (assert(io, T); T())

#»»1
# Pack debug
struct PackDebug <: IO
	maxdepth::Int
	current_depth::Base.RefValue{Int}
	stack::Vector{NTuple{2,String}}
	io::IOBuffer
end
PackDebug(maxdepth = 2) = PackDebug(maxdepth, Ref(1), [], IOBuffer())

@inline rp(x) = repr(x;context=(:compact=>true))
@inline valname(::Val{N}) where{N} = N

function fieldpack0(debug::PackDebug, stype, fname::Val, x)
	push!(debug.stack, (stype|>rp, fname|>valname|>string))
	flag = false
	if length(debug.stack) ≤ debug.maxdepth
		flag = (length(debug.stack) > debug.current_depth[])
# 		flag && println("\e[32;1m begin $stype\e[m")
		@printf("x%04x=%4d \e[36m%16s\e[m  ",
			position(debug.io), position(debug.io), x|>typeof|>rp)
		debug.current_depth[] = length(debug.stack)
		for (i, (x,y)) in pairs(debug.stack)
			isodd(i) && print("\e[34m")
			isone(i) || print("/")
			print(x, '.', y)
			isodd(i) && print("\e[m")
		end
		println()
	end
	s = pack(debug, x)
# 	flag && println("\e[31;1m end $stype\e[m")
	pop!(debug.stack)
	s
end
Base.write(debug::PackDebug, x::UInt8) = write(debug.io, x)
Base.position(debug::PackDebug) = position(debug.io)
debug(x; maxdepth=3) = pack(PackDebug(maxdepth), x)

export packed_layout, unpack, vecunpack, unpack!, pack, @Constant_str, Constant, packed_sizeof

end
