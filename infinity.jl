#=
# TODO:
# - download packages: StructIO + whatever provides @with_kw
# - TEST 1: define an item
# - make nice flags modules like dialog transitions
# - check translation format
#  - needs xgettext (or at least a very basic version)
#  - languages ?
#   - use one master language (typ. en_US) for determining when to add
#     strings, then compile strings for all languages with same strrefs
#     (obviously!)
# - forbidden NTFS chars to choose a prefix: <>:"/\|?*
# * `.d` => julia syntactic transformation
# - nice display methods for items, creatures, dialog etc.
# - namespace conflict resolution: resources have a symbolic and concrete
# name, use a table for resolving
#  - store the table in a .ids file
# - StructIO: allow invisible fields (i.e. present in the structure
#   but not in the file)
#   + add extra constructors: init(fieldtype, structtype, fieldname)
=#

module InfinityEngine

module StructIO
# ↑ apparently there already exists a registered module with this name,
# but it is listed as “experimental” and its code was last updated in
# June 2018...
# We try to maintain some degree of compatibility, at least in the
# method names (`pack`, `unpack`), but we add more functionality
# (allow user to require specific behaviour for some structure fields, etc.)
using Printf
using StaticArrays
using Markdown

@inline unpack(io::IO, T::DataType, n::Integer) =
	[ unpack(io, T) for _ in Base.OneTo(n) ]

"""    readfield(io, fieldtype, structtype, fieldname)

Hook to allow the user to redefine the behaviour for reading a given field
in a structure.

This function has a default cascade of generic methods allowing the user
to define behaviour in specific cases by overriding one of the following:
 - `$(@__MODULE__).readfield(io, fieldtype, structtype, ::Val{fieldname})`
 - `$(@__MODULE__).readfield(io, fieldtype, structtype)`
 - `$(@__MODULE__).readfield(io fieldtype)`

This last method in turn is by default:
 - `unpack(io, fieldtype)` for bits types;
 - a somewhat sensible default value for other types where possible,
   (currently: empty strings, arrays, dictionaries and sets).
"""
readfield(io::IO, fieldtype::DataType, structtype::DataType, ::Val) =
	readfield(io, fieldtype, structtype)
readfield(io::IO, fieldtype::DataType, structtype::DataType) = 
	readfield(io, fieldtype)
# a few sensible default cases:
readfield(::IO, ::Type{String}) = ""
# note: NOT AbstractArray — we want StaticVectors to be readable:
readfield(::IO, T::Type{<:Array}) = T(undef, zeros(Int, ndims(T))...)
readfield(::IO, T::Type{<:Union{Dict,Set}}) = T()

readfield(io::IO, T::DataType) =
	readfield_traits(Val(isstructtype(T)|| isbitstype(T)), io, T)

# recurse for struct types + leaf case for primitive types:
readfield_traits(::Val{true}, io::IO, T::DataType) = unpack(io, T)

# remaining cases — we don't know what to do:
readfield_traits(::Val{false}, ::IO, T::DataType) =
	error("Impossible to read field \"$T\"")

const field_vector_prop = Dict{Tuple{DataType,Symbol},NTuple{2,Symbol}}()
"""    @field_vector_prop T.f offset count

Declares field `f` of structure `T` to be a vector, with the corresponding
fields `offset` and `count` encoding its offset in the file and the number
of blocks present.
"""
macro field_vector_prop(T_field, offset, count)
	@assert Meta.isexpr(T_field, :(.))
	T, field = T_field.args
	:(field_vector_prop[($(esc(T)),$field)] = $((offset, count)))
end
using InteractiveUtils

"""    unpack(io::IO, T)

Extension of `Base.read`: reads a value of any type from an IO,
reading each field of a struct in turn.

This assumes that the type `T` has constant size.
Leaf types (integers) are read using the `Base.read` methods;
constructed types (structs and tuples) are read by recursive application
of `unpack`.

Some fields behave a bit differently; see `Constant` and `Ignored`.
"""
@generated function unpack(io::IO, ::Type{T}) where{T}
	isstructtype(T) || return :(read(io, T))
	code = Expr[:(start = position(io))]
	vars = Symbol[]
	# Generate the name of the temporary variable holding field `fn`:
	# The following code also works for Tuple, generating read_1, read_2 etc.:
	fvar = fn -> Symbol("read_"*string(fn))
	for i in 1:fieldcount(T)
		fn, ft = fieldname(T, i), fieldtype(T, i)
		v = fvar(fn); push!(vars, v)
		if fn == :transitions
			println("\e[35m**transitions**\e[m")
			println(field_vector_prop)
			println(ft <: AbstractVector)
		end
		if (ft <: AbstractVector) &&
			(x = get(field_vector_prop, (T, fn), nothing); !isnothing(x))
			(offset, count) = x
			push!(code, :($v = unpack(seek(io, start + $(fvar(offset))),
				$(eltype(ft)), $(fvar(count)))))
		else
			push!(code, :($v = readfield(io, $ft, T, Val($(QuoteNode(fn))))))
		end
	end
	expr = if T <: Tuple
		# Tuple{Int,Int}(Int, Int) constructor does not exist...
		# Note: this also catches `NamedTuple` types.
		:($(code...); T(($(vars...),)))
	else
		:($(code...); T($(vars...),))
	end
	return expr
end

"""    packed_sizeof(T)

Returns the size of the packed representation of type `T`.
"""
@inline function packed_sizeof(::Type{T}) where{T}
	# FIXME: allow user hooks to override e.g. String
	isstructtype(T) || return sizeof(T)
	v = [packed_sizeof(fieldtype(T,i)) for i in 1:fieldcount(T) ]
	return sum(v; init=0)
	return sum(packed_sizeof(fieldtype(T,i)) for i in 1:fieldcount(T); init=0)
end

struct Layout
	sizes::Vector{Int}
	types::Vector{String}
	names::Vector{String}
end
function Base.show(io::IO, ::MIME"text/plain", l::Layout)
	offset = 0
	for (s, t, n) in zip(l.sizes, l.types, l.names)
		@printf(io, "\e[1m0x%04x\e[m \e[31m%d\e[m \e[36m%-16s\e[m %s\n", offset, s, t, n)
		offset+= s
	end
	print(io, "Total size: ", offset)
end

"""    packed_layout(T)

Prints the file layout of type `T`.

This may be different from the memory layout:
 - Julia aligns the fields in memory, while on file they are not aligned;
 - any `Constant` or `Ignored` fields don't have a memory representation,
   but have a defined size on file.
"""
function packed_layout(T::DataType)
	isstructtype(T) || return Layout([sizeof(T), repr(T)])
	l = unpack(devnull, Layout) # heh. default constructor. me smart.
	for i in 1:fieldcount(T)
		(type, name) = fieldtype(T,i), fieldname(T, i)
		push!(l.sizes, packed_sizeof(type))
		push!(l.types, repr(type))
		push!(l.names, packed_fieldname(type, name))
	end
	l
end
"""    packed_fieldname(structtype, name)

Returns a string representation of the name of the corresponding field
of the struct `structtype`. By default this is `String(name)` but the
user may customize this for any field."""
packed_fieldname(type, name) = String(name)

"""    kwconstructor(T; fields...)

Defines a keyword-based constructor according to the field names for this type.

When called, any field not defined by a keyword will be initialized to
`undef`. Thus this will work only when the types of these fields accept
conversion (initialization) from `::UndefInitializer`.

It is recommended to use this function to define a keyword constructor
with default value in this way:

    T(; kwargs...) = kwconstructor(T; field1=defaultvalue1, kwargs...)

(The merge behaviour will then make `kwargs.field1`, if given,
override the supplied `defaultvalue1`).
"""
@generated function kwconstructor(::Type{T}; kwargs...) where{T}
	args = [ :(get(kwargs, $(QuoteNode(n)), undef)) for n in fieldnames(T) ]
	return :($T($(args...)))
end

"""    FixedValue{I,V}

A field with a singleton type (zero size in memory),
corresponding to a fixed value `V` in I/O.

The boolean `I` is an ignore field: if it is `false` then the value is
checked on reading, otherwise it is read and ignored.

See `Constant` and `Ignored`.
"""
struct FixedValue{I,V} end
@inline value(::Type{FixedValue{I,V}}) where{I,V} = V
"    Constant{V}: see `FixedValue`."
@inline Base.convert(T::Type{<:FixedValue}, ::Nothing) = T()
function readfield(io::IO, T::Type{<:FixedValue{I}}) where{I}
	x = read(io, sizeof(value(T)))
	@assert I || (x == value(T)) "Bad value \"$x\" (should have been $(value(T)))"
	return T()
end

@inline Base.show(io::IO, ::Type{FixedValue{I,V}}) where{I,V} =
	show_constant_ignored(io, I ? :Ignored : :Constant, V)
@inline show_constant_ignored(io, s, V) = print(io, s, '{', V, '}')
@inline show_constant_ignored(io, s, V::SVector{N,UInt8}) where{N} =
	print(io, s, '"', String(V), '"')
_fieldname(::Type{<:FixedValue}, fieldname) = ""

@inline packed_sizeof(T::Type{FixedValue{I,V}}) where{I,V} = sizeof(V)
using StaticArrays

@inline fixed_str(T, s) = :($(esc(gensym(T)))::$T{SA[$(codeunits(s)...)]})

"""    Constant{V}

Singleton type (zero memory storage) representing a fixed value in a file.
The given value is checked when reading an I/O (with `unpack`).
In case the value is not correct, an error is thrown.

---
    Constant"value"

Defines a field with irrelevant name and `Constant` type.
**This macro must only be called in a `struct` scope** (not at toplevel)."""
const Constant = FixedValue{false}
macro Constant_str(s) fixed_str(:Constant, s) end
"""    Ignored{V}
Singleton type representing a fixed value in a file.
This value is ignored when reading a file (with `unpack`).
The defined value is used when writing a file.

---
    Ignored"value"

Defines a field with irrelevant name and `Ignored` type.
**This macro must only be called in a `struct` scope** (not at toplevel)."""
const Ignored = FixedValue{true}
macro Ignored_str(s) fixed_str(:Ignored, s) end

export Constant, Ignored, @Constant_str, @Ignored_str, unpack, packed_layout

end
module Functors
"""    find_typevar(T, TX)

Given a type expression `T` with (exactly) one free variable `V`
and a concrete subtype `TX`, returns the type with which this variable
is instantiated in `TX`, i.e. `X` such that `TX == T{X}`.
"""
function find_typevar(T::DataType, TX::DataType)
	@assert T.hasfreetypevars
	for (p1, p2) in zip(T.parameters, TX.parameters)
		p1 isa TypeVar && return (p1, p2)
		p1.hasfreetypevars && return find_typevar(p1, p2)
	end
end

"""    replacetypevars(f, T, V, B, x)

Given a type expression `T` with free variable `V`
and a concrete instance `x` of `T{A}`,
as well as a function `f` mapping `A` to `B`,
returns the instance of `T{B}` constructed by mapping `f`
to the appropriate fields of `x`.
"""
replacetypevars(f, ::TypeVar, ::TypeVar, B::DataType, x) = f(x)::B
replacetypevars(f, T::Union, V::TypeVar, B::DataType, x) =
	replacetypevars(f, x isa T.a ? T.a : T.b, V, B, x)
function replacetypevars(f, T::UnionAll, V::TypeVar, B::DataType, x)
	error("not implemented for UnionAll type $T")
	# we don't yet know how to instantiate the remaining type variables of T
end
function replacetypevars(f, T, V::TypeVar, ::Type{B}, x::TX) where{B,TX}
# 	println("replacetypevars: $V => $B for x::$TX from $T")
	!T.hasfreetypevars && return x
	TX <: AbstractArray &&
		return [ replacetypevars(f, T.parameters[1], V, B, y) for y in x ]
	@assert isstructtype(T) "leaf cases should have been eliminated by now..."
# 	println("  UnionAll case: T=$T $(T isa UnionAll)")
	UnionAll(V, T){B}((replacetypevars(f, fieldtype(T,i), V, B, getfield(x,i))
			for i in 1:fieldcount(T))...,)
end

"""    functor(f::Function, T::UnionAll, x, [B::Type])

Given a `UnionAll` type T (defined over a free variable `V`)
and a concrete instance `x` of this type,
maps the function `f` over all fields of `x`
(recursively) defined by the type variable `V`,
returning the resulting object.

If `x` has type `T{A}` and `f` maps type `A` to type `B`
then the returned object will have type `T{B}`.

If the type `B` is not user-supplied then type inference will try
(and sometimes fail!) to guess it.
"""
function functor(f, T::UnionAll, x::TX, B = nothing) where{TX}
	@assert TX <: T
	if isnothing(B)
		(V, A) = find_typevar(T.body, TX)
		@assert TX == T{A}
		isnothing(B) && (B = only(Base.return_types(f, Tuple{A})))
	else
		V = T.var
	end
	replacetypevars(f, T.body, V, B, x)
end
end

import Base.read
using .StructIO
include("dottedenums.jl"); using .DottedEnums
include("dialogs.jl")

using Printf
using StaticArrays
using Parameters

# ««1 Basic types
# ««2 Extract zero-terminated string from IO

@inline function string0(v::AbstractVector{UInt8})
	l = findfirst(iszero, v)
	String(isnothing(l) ? v : view(v, 1:l-1))
end
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))

# ««2 Static length strings
struct StaticString{N} <: AbstractString
	chars::SVector{N,UInt8}
	@inline StaticString{N}(chars::AbstractVector{<:Integer}) where{N} =
		new{N}(chars)
end
@inline Base.sizeof(::StaticString{N}) where{N} = N
@inline Base.ncodeunits(s::StaticString) = sizeof(s)
@inline Base.codeunit(s::StaticString, i::Integer) = s.chars[i]
@inline Base.codeunit(::StaticString) = UInt8
# We handle only ASCII strings...
@inline Base.isvalid(::StaticString, ::Integer) = true
@inline function Base.iterate(s::StaticString, i::Integer = 1)
	i > length(s) && return nothing
	c = s.chars[i]
	iszero(c) && return nothing
	(Char(c), i+1)
end
@inline function StaticString{N}(s::AbstractString) where{N}
	@assert length(s) ≤ N "string must be at most $N characters long: \"$s\""
	return StaticString{N}(codeunits(rpad(uppercase(s), N, '\0')))
end
@inline StaticString{N}(s::StaticString{N}) where{N} = s

@inline read(io::IO, T::Type{StaticString{N}}) where{N} = T(read(io, N))

# ««2 Indices: Strref etc.
"""    Strref

Index (32-bit) referring to a translated string in dialog.tlk/dialogF.tlk.
"""
struct Strref; index::Int32; end

"""    Resref{Type}, Resref"Type"

Index (64-bit, or 8 char string) referring to a resource.
This index carries type information marking the resource type,
and indicated by the string parameter: `Resref"ITM"("BLUN01")`
describes an item, etc.
This allows dispatch to be done correctly at compile-time.

    Resref"value.type"

Shortcut for `Resref"type"("value")`.
"""
struct Resref{T}
	name::StaticString{8}
	Resref{T}(s::AbstractString) where{T} = new{T}(uppercase(first(s,8)))
end
Base.nameof(r::Resref) = r.name

macro Resref_str(s)
	s = uppercase(s)
	i = findlast('.', s)
	isnothing(i) && return Resref{Symbol(s)}
	@assert i ≤ 9
	return Resref{Symbol(s[i+1:end])}(s[begin:i-1])
end
Base.show(io::IO, ::Type{Resref{T}}) where{T} = print(io, "Resref\"", T, "\"")
Base.show(io::IO, r::Resref{T}) where{T} =
	print(io, "Resref\"", nameof(r), '.', T, '"')

"""    BifIndex
32-bit index of resource in bif files.
"""
struct BifIndex; data::UInt32; end
@inline sourcefile(r::BifIndex) = r.data >> 20
@inline tilesetindex(r::BifIndex) = (r.data >> 14) && 0x3f
@inline resourceindex(r::BifIndex) = r.data & 0x3fff

"""    Restype

16-bit value indexing a resource type in key file.
(This is immediately translated to a string value when reading this file).
"""
struct Restype; data::UInt16; end
# ««2 Resource type table

# Static correspondence between UInt16 and strings (actually symbols).
const RESOURCE_TABLE = Dict{UInt16,String}(
	0x0001 => "BMP",
	0x0002 => "MVE",
	0x0004 => "WAV",
	0x0006 => "PLT",
	0x03e8 => "BAM",
	0x03e9 => "WED",
	0x03ea => "CHU",
	0x03eb => "TIS",
	0x03ec => "MOS",
	0x03ed => "ITM",
	0x03ee => "SPL",
	0x03ef => "BCS",
	0x03f0 => "IDS",
	0x03f1 => "CRE",
	0x03f2 => "ARE",
	0x03f3 => "DLG",
	0x03f4 => "2DA",
	0x03f5 => "GAM",
	0x03f6 => "STO",
	0x03f7 => "WMP",
	0x03f8 => "CHR",
	0x03f9 => "BS",
	0x03fa => "CHR2",
	0x03fb => "VVC",
	0x03fc => "VFC",
	0x03fd => "PRO",
	0x03fe => "BIO",
	0x03ff => "WBM",
	0x0400 => "FNT",
	0x0402 => "GUI",
	0x0403 => "SQL",
	0x0404 => "PVRZ",
	0x0405 => "GLSL",
	0x0408 => "MENU",
	0x0409 => "LUA",
	0x040a => "TTF",
	0x040b => "PNG",
	0x044c => "BAH",
	0x0802 => "INI",
	0x0803 => "SRC",
)
@inline String(x::Restype) = get(RESOURCE_TABLE, x.data, x.data|>repr)
@inline Base.Symbol(x::Restype) = Symbol(String(x))

# ««2 Resource type (marked IO object)
"""    Resource{Type}, Resource"TYPE"

A structure describing the physical location of a game resource
(either from filesystem or from a BIF content),
marked with the resource name + type.

 - Since the resource type determines the `read`/`write` methods
   (and only takes a finite set of values) this is a type parameter.
 - The resource name (i.e. basename of the file without extension) is stored
   as a field. The name is canonicalized as upper-case.

Defined methods include:
 - `Resource"EXT"`: macro defining static value for this file type.
 - `read(::Resource{T})` (this opens the IO).

Concrete subtypes (i.e. `ResourceBuf`, `ResourceFile`) must implement:
 - `open(resource)`: returns an IO (this will automatically enable
   `open(f, resource)`; see `"base/io.jl"`).
 - `nameof(resource)`: returns the resource name as a String.

Specializations (i.e. `Resource"DLG"` etc.) must implement:
 - `read(::Resource{T}, io::IO)`.
 - (**TODO**) `write(::Resource{T}, io::IO)`.
"""
abstract type Resource{T} end
macro Resource_str(str)
	if contains(str, '.')
		(_, b) = splitext(str)
		return :(Resource{$(QuoteNode(Symbol(uppercase(b[2:end]))))}($str))
	else
		return :(Resource{$(QuoteNode(Symbol(uppercase(str))))})
	end
end
# macro Resource_str(s) Resource{Symbol(uppercase(s))} end
@inline read(f::Resource; kw...) = open(f) do io; read(f, io; kw...); end

mutable struct ResourceFile{T} <:Resource{T}
	filename::String
end
@inline Resource{T}(f::AbstractString) where{T} = ResourceFile{T}(f)
@inline Resource(f::AbstractString) = 
	Resource{Symbol(uppercase(splitext(basename(f))[2])[2:end])}(f)
@inline Base.open(r::ResourceFile) = open(r.filename)
@inline Base.nameof(r::ResourceFile) =
	uppercase(splitext(basename(r.filename))[1])

mutable struct ResourceBuf{T} <: Resource{T}
	name::String
	buffer::IOBuffer
end
@inline Base.open(r::ResourceBuf) = r.buffer
@inline Base.nameof(r::ResourceBuf) = r.name

Resref(r::Resource{T}) where{T} = Resref{T}(nameof(r))

# ««1 tlk
# TODO: remove type variable
struct TlkStrings{X}
	strings::Vector{X}
	index::Dict{String,Int32}
	# i is the Julia index, not the strref
	@inline TlkStrings(strings::AbstractVector{X}) where{X}  =
		new{X}(strings, Dict(s.string=>i for (i,s) in pairs(strings)))
end

function Base.getindex(f::TlkStrings{X}, s::Strref) where{X}
	i = s.index
	i ∈ eachindex(f.strings) || (i = 0)
	f.strings[i+1]::X
end

struct TLK_hdr
	Constant"TLK V1  "
	lang::UInt16
	nstr::UInt32
	offset::UInt32
end
struct TLK_str
	flags::UInt16
	sound::Resref"WAV"
	volume::UInt32
	pitch::UInt32
	offset::UInt32
	length::UInt32
end

function read(f::Resource"TLK", io::IO)
	header = unpack(io, TLK_hdr)
	strref = unpack(io, TLK_str, header.nstr)
	return TlkStrings([ (string = string0(io, header.offset + s.offset, s.length),
		flags = s.flags, sound = s.sound, volume = s.volume, pitch = s.pitch)
		for s in strref ])
end
Base.findall(r::Union{Regex,AbstractString}, tlk::TlkStrings) =
	[ Strref(i-1) for (i,s) in pairs(tlk.strings) if contains(s.string, r) ]

function Strref(tlk::TlkStrings, s::AbstractString)
	i = get(tlk.index, s, nothing)
	if isnothing(i)
		push!(tlk.strings, s)
		i = tlk.index[s] = length(tlk.strings)
	end
	return Strref(i-1)
end

# ««1 key/bif
struct KEY_hdr
	Constant"KEY V1  "
	nbif::Int32
	nres::Int32
	bifoffset::UInt32
	resoffset::UInt32
end
struct KEY_bif
	filelength::UInt32
	offset::UInt32
	namelength::UInt16
	location::UInt16
end
struct KEY_res
	name::StaticString{8}
	type::Restype
	location::BifIndex
end

"""    KeyIndex

Structure containing the index of BIF resources as described in `"chitin.key"`.
Methods include:

 - `Resource(key, resref)`: converts a `Resref` to a `Resource` of matching type.
 - `read(key, resref)`: opens the resource and reads it.
 - `names(key, Resource"type")`: returns an iterator over all names of resources
   of this type present in the game.
 - `findall(Resource"type", key)`: returns an iterator over all resources.
"""
struct KeyIndex{X}
	# TODO: remove this type parameter, X is always the same tuple type
	directory::String
	bif::Vector{String}
	resources::Vector{X}
	location::Dict{Tuple{StaticString{8},Symbol},BifIndex}
	function KeyIndex(dir, bif, res::AbstractVector{X}) where{X}
		loc = Dict((r.name, Symbol(r.type)) => r.location for r in res)
		new{X}(dir, bif, res, loc)
	end
end
KeyIndex(filename::AbstractString) = open(filename) do io
	dir = dirname(filename)
	header = unpack(io, KEY_hdr)
	seek(io, header.bifoffset)
	bifentries = unpack(io, KEY_bif, header.nbif)
	bifnames = [ string0(io, x.offset, x.namelength) for x in bifentries ]
	seek(io, header.resoffset)
	resentries = unpack(io, KEY_res, header.nres)
	return KeyIndex(dir, bifnames, resentries)
end

const XOR_KEY = "88a88fba8ad3b9f5edb1cfeaaae4b5fbeb82f990cac9b5e7dc8eb7aceef7e0ca8eeaca80cec5adb7c4d08493d5f0ebc8b49dccafa595ba9987d29de391ba90ca"|>hex2bytes

function decrypt(io::IO)
	peek(io) != 0xff && return io
	buf = Vector{UInt8}(read(seek(io,1), String))
	for i in eachindex(buf)
		buf[i] ⊻= XOR_KEY[mod1(i-1, length(XOR_KEY))]
	end
	return IOBuffer(buf)
end
struct BIF_hdr
	Constant"BIFFV1  "
	nres::UInt32
	ntilesets::UInt32
	offset::UInt32
end

struct BIF_resource
	locator::BifIndex
	offset::UInt32
	size::UInt32
	type::Restype
	_1::Ignored{UInt16}
end

struct BIF_tileset
	locator::BifIndex
	offset::UInt32
	ntiles::UInt32
	size::UInt32
	_1::Constant{0x03eb}
	_2::Ignored{UInt16}
end

bifcontent(file::AbstractString, index::Integer) = open(file, "r") do io
	header = unpack(io, BIF_hdr)
	seek(io, header.offset)
	resources = unpack(io, BIF_resource, header.nres)
	IOBuffer(read(seek(io, resources[index+1].offset), resources[index+1].size))
end

function (::Type{<:Resource{T}})(key::KeyIndex, name) where{T}
	name = uppercase(name)
	loc = get(key.location, (StaticString{8}(name), T), nothing)
	isnothing(loc) && return nothing
	bif = joinpath(key.directory, key.bif[1+sourcefile(loc)])
	return ResourceBuf{T}(String(name), bifcontent(bif, resourceindex(loc)))
end
Base.names(key::KeyIndex, ::Type{Resource{T}}) where{T} =
	(r[1] for r in keys(key.location) if r[2] == T)
Base.findall(T::Type{<:Resource}, key::KeyIndex) =
	(T(key, x) for x in names(key,T))

# ««1 ids
# useful ones: PROJECTL SONGLIST ITEMCAT NPC ANISND ?
function read(f::Resource"IDS", io::IO; debug=false)
	io = decrypt(io)
	debug && (mark(io); println("(", read(io, String), ")"); reset(io))
	line = readline(io)
	(!contains(line, " ") || startswith(line, "IDS")) && (line = readline(io))
	!isnothing(match(r"^[0-9]*$", line)) && (line = readline(io))
	list = Pair{Int,String}[]
	while true
		line = replace(line, r"\s*$" => "")
		if !isempty(line)
			s = split(line, r"\s+"; limit=2)
			length(s) ≤ 1 && error("could not split IDS line: $line")
			push!(list, (parse(Int, s[1]) => s[2]))
		end
		eof(io) && break
		line = readline(io)
	end
	return list
end
# ««1 2da
struct MatrixWithHeaders{T} <: AbstractMatrix{T}
	rows::Vector{String}
	cols::Vector{String}
	matrix::Matrix{T}
	@inline MatrixWithHeaders(rows::AbstractVector{<:AbstractString},
		cols::AbstractVector{<:AbstractString}, matrix::AbstractMatrix{T}) where{T}=
		new{T}(rows, cols, matrix)
end

@inline Base.size(m::MatrixWithHeaders) = size(m.matrix)
@inline Base.getindex(m::MatrixWithHeaders, i::Integer...) =
	getindex(m.matrix, i...)
function Base.getindex(m::MatrixWithHeaders,
		s1::AbstractString, s2::AbstractString)
	i1 = findfirst(==(s1), m.rows)
	@assert !isnothing(i1) "Row header not found: '$s1'"
	i2 = findfirst(==(s2), m.cols)
	@assert !isnothing(i2) "Row header not found: '$s2'"
	return m.matrix[i1,i2]
end
function read(f::Resource"2DA", io::IO; debug=false, aligned=false)
	io = decrypt(io)
	debug && (mark(io); println("(", read(io, String), ")"); reset(io))
	line = readline(io)
# 	@assert contains(line, r"^\s*2da\s+v1.0"i) "Bad 2da first line: '$line'"
	defaultvalue = replace(replace(readline(io), r"^\s*" => ""), r"\s*$" => "")
	line = readline(io)
	if !aligned
		cols = split(line)
		lines = readlines(io)
		mat = split.(lines)
		MatrixWithHeaders(first.(mat), cols,
			[m[j+1] for m in mat, j in eachindex(cols)])
	else
		positions = [ i for i in 2:length(line)
			if isspace(line[i-1]) && !isspace(line[i]) ]
		cols = [ match(r"^\S+", line[i:end]).match for i in positions ]
		lines = readlines(io)
		mat = [ match(r"^\S+", line[i:end]).match for line in lines, i in positions]
		MatrixWithHeaders([match(r"^\S+", line).match for line in lines], cols,mat)
	end
end
# ««1 cre
mutable struct CRE_colour
	metal::UInt8
	minor::UInt8
	major::UInt8
	skin::UInt8
	leather::UInt8
	armor::UInt8
	hair::UInt8
end
mutable struct CRE_armor_class
	natural::UInt16
	effective::UInt16
	crushing::UInt16 # 4 modifiers
	missile::UInt16
	piercing::UInt16
	slashing::UInt16
end
mutable struct CRE_saves
	death::UInt8
	wands::UInt8
	polymorph::UInt8
	breath::UInt8
	spells::UInt8
end
mutable struct CRE_resist
	fire::UInt8
	cold::UInt8
	electricity::UInt8
	acid::UInt8
	magic::UInt8
	magic_fire::UInt8
	magic_cold::UInt8
	slashing::UInt8
	crushing::UInt8
	piercing::UInt8
	missile::UInt8
end
mutable struct CRE_abilities
	strength::UInt8
	strength_extra::UInt8
	intelligence::UInt8
	wisdom::UInt8
	dexterity::UInt8
	constitution::UInt8
	charisma::UInt8
end
mutable struct CRE_scripts
	override::Resref"bcs"
	class::Resref"bcs"
	race::Resref"bcs"
	general::Resref"bcs"
	default::Resref"bcs"
end
mutable struct CRE_hdr
	Constant"CRE V1.0"
	long_name::Strref
	short_name::Strref # tooltip
	flags::UInt32
	xp_value::UInt32
	xp_level::UInt32
	gold::UInt32
	status::UInt32 # state.ids
	hp::UInt16
	max_hp::UInt16
	animation::UInt32 # animate.ids
	colour::CRE_colour
	eff_Version::UInt8
	small_portrait::Resref"BMP"
	large_portrait::Resref"BMP"
	reputation::Int8
	hide_in_shadows::UInt8
	armor_class::CRE_armor_class
	thac0::UInt8
	number_attacks::UInt8
	saves::CRE_saves
	resist::CRE_resist
	detect_illusion::UInt8
	set_traps::UInt8
	lore::UInt8
	lockpicking::UInt8
	move_silently::UInt8
	find_traps::UInt8
	pickpockets::UInt8
	fatigue::UInt8
	intoxication::UInt8
	luck::Int8
	proficiency::StaticString{15}
	nightmare_mode::UInt8
	translucency::UInt8
	reputation_killed::Int8
	reputation_join::Int8
	reputation_leave::Int8
	turn_undead::UInt8
	tracking::Int8
	tracking_target::StaticString{32}
	sounds::SVector{100,Strref} # soundoff.ids, sndslot.ids
	level_class::SVector{3,UInt8}
	sex::UInt8
	abilities::CRE_abilities
	morale::UInt8
	morale_break::UInt8
	racial_enemy::UInt8 # race.ids
	morale_recovery::UInt16
	kits::UInt32
	scripts::CRE_scripts
	enemy_ally::UInt8 # ea.ids
	general::UInt8 # general.ids
	race::UInt8 # race.ids
	class::UInt8 # class.ids
	specific::UInt8 # specific.ids
	gender::UInt8 # gender.ids
	object::SVector{5,UInt8} # object.ids
	alignment::UInt8 # alignmen.ids
	global_actor::UInt16
	local_actor::UInt16
	death_variable::StaticString{32}
	known_spells_offset::UInt32
	known_spells_count::UInt32
	spell_memorization_offset::UInt32
	spell_memorization_count::UInt32
	memorized_spells_offset::UInt32
	memorized_spells_count::UInt32
	item_slots_offset::UInt32
	items_offset::UInt32
	items_count::UInt32
	effects_offset::UInt32
	effects_count::UInt32
	dialog::Resref"DLG"
end
function read(f::Resource"CRE", io::IO)
	unpack(io, CRE_hdr)
end
# ««1 itm
# ««2 Enums etc.
@dottedflags ItemFlag::UInt32 begin # ITEMFLAG.IDS
	Indestructible
	TwoHanded
	Droppable
	Displayable
	Cursed
	CannotScribe
	Magical
	LeftHanded
	# byte 2
	Silver
	ColdIron
	OffHanded
	Dialog
	Fake_OffHand
	Disable_Offhand
	UsableInInventory # PSTEE
	Adamantine
	# byte 4
	Undispellable = 0x01000000
	ToggleCriticalHitAversion
end
@dottedenum ItemCat::UInt16 begin # ITEMCAT.IDS
	Misc = 0x0000
	Amulet = 0x0001
	Armor = 0x0002
  Belt = 0x0003
  Boots = 0x0004
  Arrow = 0x0005
  Bracers = 0x0006
  Headgear = 0x0007
  Keys = 0x0008 # not in IWD
  Potion = 0x0009
  Ring = 0x000a
  Scroll = 0x000b
  Shield = 0x000c
  Food = 0x000d
  Bullet = 0x000e
  Bow = 0x000f
  Dagger = 0x0010
  Mace = 0x0011 # includes clubs in BG
  Sling = 0x0012
  SmallSword = 0x0013
  LargeSword = 0x0014
  Hammer = 0x0015
  Morningstar = 0x0016
  Flail = 0x0017
  Dart = 0x0018
  Axe = 0x0019
  Quarterstaff = 0x001a
  Crossbow = 0x001b
  FistWeapon = 0x001c
  Spear = 0x001d
  Halberd = 0x001e # and two-handed axes
  Bolt = 0x001f
  Cloak = 0x0020
  Gold = 0x0021 # for monster-dropped treasure
  Gem = 0x0022
  Wand = 0x0023
  Container = 0x0024 # eye/broken armor
  Books = 0x0025 # broken shield/bracelet
  Familiar = 0x0026 # broken sword/earrings
  Tattoo = 0x0027 # PST
  Lenses = 0x0028 # PST
  Buckler = 0x0029 # teeth (PST)
  Candle = 0x002a
  Club = 0x002c # IWD
  LargeShield = 0x002f # IWD
  MediumShield = 0x0031 # IWD
  Notes = 0x0032
  SmallShield = 0x0035 # IWD
  Telescope = 0x0037 # IWD
  Drink = 0x0038 # IWD
  GreatSword = 0x0039 # IWD
  Container1 = 0x003a
  Fur = 0x003b
  LeatherArmor = 0x003c
  StuddedLeatherArmor = 0x003d
  ChainMail = 0x003e
  SplintMail = 0x003f
  HalfPlate = 0x0040
  FullPlate = 0x0041
  HideArmor = 0x0042
  Robe = 0x0043
  BastardSword = 0x0045
  Scarf = 0x0046
  FoodIWD2 = 0x0047
  Hat = 0x0048
  Gauntlet = 0x0049
end
@dottedflags UsabilityFlags::UInt32 begin # not found
	Chaotic
	Evil
	Good
	Neutral # neither good nor evil
	Lawful
	True # neither lawful nor chaotic
	Bard
	Cleric
	# byte 2
	ClericMage
	ClericThief
	ClericRanger
	Fighter
	FighterDruid
	FighterMage
	FighterCleric
	FighterMageCleric
	# byte 3
	FighterMageThief
	FighterThief
	Mage
	MageThief
	Paladin
	Ranger
	Thief
	Elf
	# byte 4
	Dwarf
	HalfElf
	Halfling
	Human
	Gnome
	Monk
	Druid
	HalfOrc
end
struct ItemAnimation; name::StaticString{2}; end
@dottedenum WProf::UInt8 begin # WPROF.IDS
	None = 0x00
	BastardSword = 0x59
	LongSword
	ShortSword
	TwoHandedSword
	Katana
	Scimitar
	Dagger
	WarHammer
	Spear
	Spear
	Halberd
	FlailMorningstar
	Mace
	Quarterstaff
	Crossbow
	LongBow
	Shortbow
	Dart
	Sling
	Blackjack
	Gun
	MartialArts
	TwoHandedStyle
	SwordShieldStyle
	SingleWeaponStyle
	TwoWeaponStyle
	Club
end
@dottedenum AttackType::UInt8 begin # not found
	None = 0
	Melee
	Ranged
	Magical
	Launcher
end
@dottedenum TargetType::UInt8 begin # not found
	Invalid = 0
	LivingActor = 1
	Inventory = 2
	DeadActor = 3
	AnyPoint = 4
	Caster = 5
	CasterInstant = 7
end
@dottedenum LauncherType::UInt8 begin # not found
	None = 0
	Bow
	Crossbow
	Sling
	Spear = 40
	ThrowingAxe = 100
end
@dottedenum DamageType::UInt8 begin # not found
	None = 0
	Piercing = 1
	Crushing = 2
	Slashing = 3
	Missile = 4
	Fist = 5
	PiercingCrushing = 6
	PiercingSlashing = 7
	CrushingSlashing = 8
	BluntMissile = 9 # bugged
end
# ««2 Data blocks
struct ITM_ability
	attack_type::UInt8
	must_identify::UInt8
	location::UInt8
	alternative_dice_sides::UInt8
	use_icon::Resref"BAM"
	target_type::UInt8
	target_count::UInt8
	range::UInt16
	launcher_required::UInt8
	alternative_dice_thrown::UInt8
	speed_factor::UInt8
	alternative_damage_bonus::UInt8
	thac0_bonus::UInt16
	dice_sides::UInt8
	primary_type::UInt8
	dice_thrown::UInt8
	secondary_type::UInt8
	damage_bonus::UInt16
	damage_type::UInt16
	nfeatures::UInt16
	idxfeatures::UInt16
	max_charges::UInt16
	depletion::UInt16
	flags::UInt32
	projectile_animation::UInt16
	overhand_chance::UInt16
	backhand_chance::UInt16
	thrust_chance::UInt16
	is_arrow::UInt16
	is_bolt::UInt16
	is_bullet::UInt16
end
@with_kw struct ITM_hdr{S}
	resource_name::String
	Constant"ITM V1  "
	unidentified_name::S
	identified_name::S
	replacement::Resref"ITM"
	flags::ItemFlag
	item_type::ItemCat
	usability::UInt32 # UsabilityFlags
	animation::ItemAnimation
	minlevel::UInt16
	minstrength::UInt16
	minstrenghtbonus::UInt8
	kit1::UInt8
	minintelligence::UInt8
	kit2::UInt8
	mindexterity::UInt8
	kit3::UInt8
	minwisdom::UInt8
	kit4::UInt8
	minconstitution::UInt8
	proficiency::WProf
	mincharisma::UInt16
	price::UInt32
	stackamount::UInt16
	inventoryicon::Resref"BAM"
	lore::UInt16
	groundicon::Resref"BAM"
	weight::UInt32
	unidentified_description::S
	identified_description::S
	description_icon::Resref"BAM"
	enchantment::UInt32
	ext_header_offset::UInt32
	ext_header_count::UInt16
	feature_offset::UInt32
	feature_index::UInt16
	feature_count::UInt16
	abilities::Vector{ITM_ability}
end
StructIO.field_vector_prop[(ITM_hdr{Strref}, :abilities)] =
	(:ext_header_offset, :ext_header_count)
@inline searchkey(i::ITM_hdr) = i.identified_name
# ««2 Item function
function read(f::Resource"ITM", io::IO)
	unpack(io, ITM_hdr{Strref})
end
# ««1 dlg
struct DLG_state{S}
	text::S
	first_transition::Int32
	number_transitions::Int32
	trigger::Int32
end
struct DLG_transition{S}
	flags::UInt32
	text::S
	journal::S
	trigger::Int32
	action::Int32
	next_actor::Resref"DLG"
	next_state::Int32
end
# this is the same type for state trigger, transition trigger, actions:
struct DLG_string
	offset::Int32 # of trigger string
	length::Int32 # idem
end
struct DLG_hdr
	Constant"DLG V1.0"
	number_states::Int32
	offset_states::Int32
	number_transitions::Int32
	offset_transitions::Int32
	offset_state_triggers::Int32
	number_state_triggers::Int32
	offset_transition_triggers::Int32
	number_transition_triggers::Int32
	offset_actions::Int32
	number_actions::Int32
	flags::UInt32
	states::Vector{DLG_state{Strref}}
	transitions::Vector{DLG_transition{Strref}}
# 	st_triggers::Vector{DLG_string}
# 	tr_triggers::Vector{DLG_string}
# 	actions::Vector{DLG_string}
end
StructIO.@field_vector_prop DLG_hdr.states offset_states number_states
StructIO.@field_vector_prop DLG_hdr.transitions offset_transitions number_transitions
# StructIO.field_vector_prop(DLG_hdr, :st_triggers, :offset_state_triggers,
# 	:number_state_triggers)
# StructIO.field_vector_prop(DLG_hdr, :tr_triggers, :offset_transition_triggers,
# 	:number_transition_triggers)
# StructIO.field_vector_prop(DLG_hdr, :actions, :offset_actions, :number_actions)

@inline dialog_strings(io::IO, offset, count)= [string0(io, s.offset, s.length)
	for s in unpack(seek(io, offset), DLG_string, count)]

Dialogs.get_string(c::Dialogs.Context, ref::Strref) = ref.index

Dialogs.StateKey{Any}(::Any, n::Resref"dlg", s::Integer) =
	Dialogs.StateKey{Any}("main", nameof(n), s)

function read(f::Resource"DLG", io::IO)
	self = Resref(f)
	header = unpack(io, DLG_hdr)
# 	states = unpack(seek(io,header.offset_states), DLG_state{Strref},
# 		header.number_states)
# 	transitions = unpack(seek(io, header.offset_transitions),
# 		DLG_transition{Strref}, header.number_transitions)
	st_triggers = dialog_strings(io, header.offset_state_triggers,
			header.number_state_triggers)
	tr_triggers = dialog_strings(io, header.offset_transition_triggers,
			header.number_transition_triggers)
	actions = dialog_strings(io, header.offset_actions, header.number_actions)

	global H = header
	dialog = Dialogs.top
	Dialogs.namespace("main")
	getval(list, i) = i+1 ∈ eachindex(list) ? list[i+1] : "bad ref $i"
	for (i, s) in pairs(header.states)
		state = Dialogs.add_state!(dialog, (self|>nameof, i-1);
			text = s.text, trigger = getval(st_triggers, s.trigger))
		for j in s.first_transition+1:s.first_transition+s.number_transitions
			t = header.transitions[j]
			Dialogs.add_transition!(dialog, state, (nameof(t.next_actor), t.next_state);
				t.text, t.journal, trigger = getval(tr_triggers, t.trigger),
				action = getval(actions, t.action), flags = UInt32(t.flags))
		end
	end
	return dialog
	#= patterns observed in Bioware dialogues:
	* normal reply: (text) action=-1 journal=strref(0) trigger=-1
	* (action|terminates) target=("",0) text=strref(0) journal=strref(0)
	* exit transition: (action|terminates), target=("",0),
	  text=strref(-1) journal=strref(0)
	* exit: (action|terminates|journal|solved) target=("",0) trigger=-1
	* (e.g. abazigal transition 8) random solved bit w/o journal entry (nor bit)
	 => don't believe the solved bit, use the journal entry!
	IOW: strrefs are 0 when undefined (=> graceful fail);
		other indices are -1
	* 
	=#
end

#««1 Game
# shorthand: strings*dialog instantiates Strrefs
for T in (:ITM_hdr,)
	@eval @inline Base.:*(str::TlkStrings, x::$T{Strref}) =
		Functors.functor(x->str[x].string::String, $T, x)
end

# Base.:*(str::TlkStrings, x::Dialogs.StateMachine{Int32,Any}) =
# 	Functors.functor(x->str[x].string::String,
# 		Dialogs.StateMachine{Int32,S,String,Any} where{S}, x)

"""    Game

Main structure holding all top-level data for a game installation, including:
 - key/bif archived files,
 - table of override files,
 - tlk strings (TODO).

Methods include:

 - `Resource(game, resref)`: converts a `Resref` to a `Resource`
   (i.e. file descriptor) of the same type.
 - `read(game, resref)`: opens the corresponding resource and returns the
   appropriate Julia structure depending on the `Resref` type.
 - `filetype(game, resref)`: returns either 0 (resource not found),
   1 (resource found in bif), or 2 (resource found in override).
 - `names(game, Resource"type")`: returns a vector of all names of
   existing resources of this type.
 - `findall(Resource"type", game)`: returns an iterator over all existing
   resource descriptors of this type.
"""
struct Game
	directory::String
	key::KeyIndex
	override::Dict{Symbol, Set{String}}
end
"""    Game(directory)

Initializes a `Game` structure from the game directory
(i.e. the directory containing the `"chitin.key"` file).
"""
function Game(directory::AbstractString)
	key = KeyIndex(joinpath(directory, "chitin.key"))
	println("read $(length(key.resources)) key resources")
	override = Dict{Symbol,Set{String}}()
	o_dir = joinpath(directory, "override")
	if !isdir(o_dir)
		println("created override directory: ", o_dir)
		mkdir(o_dir)
	else
		for f in readdir(joinpath(directory, "override"))
			(n, e) = splitext(uppercase(basename(f))); t = Symbol(e[2:end])
			push!(get!(override, t, Set{String}()), n)
		end
		println("read $(sum(length(v) for (_,v) in override; init=0)) override resources")
	end
	return Game(directory, key, override)
end
" returns 2 if override, 1 if key/bif, 0 if not existing."
function filetype(game::Game,::Type{Resource{T}}, name::AbstractString) where{T}
	haskey(game.override, T) && uppercase(String(name)) ∈ game.override[T] &&
		return 2
	haskey(game.key.location, (StaticString{8}(name), T)) && return 1
	return 0
end
function Resource{T}(game::Game, name::AbstractString) where{T}
	t = filetype(game, Resource{T}, name)
	if t == 2
		file = joinpath(game.directory, "override", String(name)*'.'*String(T))
		return ResourceFile{T}(file)
	elseif t == 1
		return Resource{T}(game.key, name)
	else
		error("resource not found: $name.$T")
	end
end
@inline Resource(k::Union{Game,KeyIndex}, resref::Resref{T}) where{T} =
	Resource{T}(k, nameof(resref))

@inline read(k::Union{Game,KeyIndex}, resref::Resref; kw...) =
	read(Resource(k, resref); kw...)
@inline filetype(game::Game, resref::Resref{T}) where{T} =
	filetype(game, Resource{T}, resref)

@inline Base.names(game::Game, ::Type{Resource{T}}) where{T} =
	get(game.override,T, String[]) ∪ names(game.key, Resource{T})
@inline Base.findall(R::Type{<:Resource}, game::Game) =
	(R(game, n) for n in names(game, R))

"""    search(game, strings, Resource"type", text)

Searches for the given text (string or regular expression)
in the names of all resources of the given `type`.
`strings` is the string database used for translation.
"""
function search(game::Game, str::TlkStrings, R::Type{<:Resource}, text)
	for res in all(game, R)
		s = str[searchkey(read(res))].string
		contains(s, text) || continue
		@printf("%c%-8s %s\n", res isa ResourceFile ? '*' : ' ', nameof(res), s)
	end
end
# »»1
end
IE=InfinityEngine; S=IE.StructIO
game = IE.Game("../bg/game")
str=read(IE.Resource"../bg/game/lang/fr_FR/dialog.tlk")
# IE.search(game, str, IE.Resource"ITM", "Varscona")
key = IE.KeyIndex("../bg/game/chitin.key")
# dlg = read(game, IE.Resref"melica.dlg")
dlg = read(game, IE.Resref"zorl.dlg")
# dia = read(IE.Resource"dlg"(key, "abazigal"))
# dia=read(IE.Resource"../bg2/game/override/hull.dlg")
# dia=read(IE.Resource"../bg2/game/override/abazigal.dlg")

D=IE.Dialogs
D.language("en")
D.namespace("test")
D.actor("zorl")

D.trigger("// new dialogue")
D.say(1 => "hello <CHARNAME>")
	D.reply(" hello Zorl" => 1)
	D.reply(" go on...")
D.say(2 => "Yes I go on.")

D.state(1)
	D.reply("I attack!" => exit)

nothing
# dlg;
