#=
# TODO:
# - TEST 1: define an item
# - long references: (namespace, name)
#  - operations needed for references (longref):
#   - map longref -> associated resource
#   - resource -> longref of this: for user
#   - convert(resref, longref)
#   - list of all existing longref: for unicity, conversion
#  - to translate to short references, there needs to exist a global hash
#  + idea 3: use longref = ((interned) namespace, name)
#   - make this hierarchical by searching in all parent namespaces?
#  - create a fresh longref when creating new objects
#   - either from searchkey if available (and iterating suffix as needed
#   when cloning: .1, .2 etc);
#   - or from random string otherwise
#  - convert object to longref: take object.self
#  table (long ref) => (short ref) at some point
#  - during construction, a list of existing long refs is enough
#    (as well as a way to mark the long ref for each object and resref use)
#  - idea 1: use the resref itself as (namespace)/(name), which is invalid
#    (2^24 namespaces, 2^32 names should be enough)
#    this needs a global hash table (long ref) => (pseudo-resref)
#
#  - idea 2: use a Union{} type (less memory-efficient)
#
#  - syntax for long ref of an object? e.g. Item(reference="...")
#   - we now have Item(self="..."); can easily be changed
#  - by default, use the search key (i.e. identified name for an item)
#  - entering a pointer: convert(Item, Resref)
#
# + a type for marked-language strings (pull from dialogs.jl)
# - make nice flags modules like dialog transitions
# - check translation format
#  - needs xgettext (or at least a very basic version)
# * nice display methods for items, creatures, dialog etc.
# - forbidden NTFS chars to choose a prefix: <>:"/\|?*
# - namespace conflict resolution: resources have a symbolic and concrete
# name, use a table for resolving
#  - store the table in a .ids file
# - `.d` => julia syntactic transformation
=#

module InfinityEngine
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
include("pack.jl"); using .Pack
include("dottedenums.jl"); using .DottedEnums

using Printf
using StaticArrays
using Parameters
using UniqueVectors
import Base.read, Base.write

# ««1 Basic types
# Compact representation, useful for Flags and Enums
@inline rp(x) = repr(x;context=(:compact=>true))
# ««2 Extract zero-terminated string from IO
@inline function string0(v::AbstractVector{UInt8})
	l = findfirst(iszero, v)
	String(isnothing(l) ? v : view(v, 1:l-1))
end
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))

# ««2 Static length strings
struct StaticString{N} <: AbstractString
	chars::SVector{N,UInt8}
	# once we encounter a zero character, all that follows is zero:
	@inline StaticString{N}(chars::AbstractVector{UInt8}) where{N} = new{N}(
		SVector{N,UInt8}(any(iszero, view(chars, 1:min(i-1,length(chars)))) ?
			zero(UInt8) : get(chars, i, zero(UInt8)) for i in 1:N))
end
@inline Base.sizeof(::StaticString{N}) where{N} = N
@inline Base.ncodeunits(s::StaticString) = sizeof(s)
@inline Base.codeunit(s::StaticString, i::Integer) = s.chars[i]
@inline Base.codeunit(::StaticString) = UInt8
@inline Base.lowercase(s::StaticString{N}) where{N} =
	StaticString{N}(SVector{N,UInt8}(UInt8(lowercase(Char(c))) for c in s.chars))
# We handle only ASCII strings...
@inline Base.isvalid(::StaticString, ::Integer) = true
@inline function Base.iterate(s::StaticString, i::Integer = 1)
	i > length(s) && return nothing
	c = s.chars[i]
# 	iszero(c) && return nothing
	(Char(c), i+1)
end
@inline function StaticString{N}(s::AbstractString) where{N}
	@assert length(s) ≤ N "string must be at most $N characters long: \"$s\""
	return StaticString{N}(codeunits(lowercase(s)))
end
@inline StaticString{N}(s::StaticString{N}) where{N} = s

@inline Pack.unpack(io::IO, T::Type{StaticString{N}}) where{N} = T(read(io, N))
@inline Pack.pack(io::IO, s::StaticString) = write(io, s.chars)

# ««2 Indices: Resref etc.
"""    Strref

Index (32-bit) referring to a translated string in dialog.tlk or dialogF.tlk.
"""
struct Strref
	index::Int32
end
@inline Strref(x::Strref) = x
@inline Base.isvalid(s::Strref) = (s.index > 0)
@inline Base.show(io::IO, s::Strref) = print(io, "Strref(", s.index, ")")

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
	Resref{T}(s::AbstractString) where{T} = new{T}(lowercase(first(s,8)))
end
@inline Pack.unpack(io::IO, T::Type{<:Resref}) =
	T(lowercase(unpack(io, StaticString{8})))
# @inline Pack.fieldunpack(::IO, ::Val{:self}, T::Type{<:Resource}) = T("", "")
Base.nameof(r::Resref) = r.name

macro Resref_str(s)
	s = lowercase(s)
	i = findlast('.', s)
	isnothing(i) && return Resref{Symbol(s)}
	@assert i ≤ 9
	return Resref{Symbol(s[i+1:end])}(s[begin:i-1])
end
Base.show(io::IO, ::Type{Resref{T}}) where{T} = print(io, "Resref\"", T, "\"")
Base.show(io::IO, r::Resref{T}) where{T} =
	print(io, "Resref\"", nameof(r), '.', T, '"')

"""    Resource{T}

A long resource descriptor: this contains a (static) type and a (dynamic)
namespace and name uniquely identifying the resource.

This is converted to a short `Resref` on compilation.

`Resref` (i.e. short resource identifers) are converted to `Resource`
using the namespace `""` (and keeping their short name).
"""
struct Resource{T}
	namespace::String
	name::String
end
@inline Base.nameof(r::Resource) = r.name
@inline (T::Type{<:Resource})(x::Resource) = T(x.namespace, x.name)

@inline Pack.unpack(io::IO, T::Type{<:Resource}) =
	T("", unpack(io, StaticString{8}))
@inline Pack.packed_sizeof(::Type{<:Resource}) = 8
@inline Pack.pack(io::IO, r::Resource) = write(io, StaticString{8}(r.name))

macro Resource_str(s)
	return Resource{Symbol(lowercase(s))}
end

include("opcodes.jl")
using .Opcodes: Opcode
# ««2 ResIO type (marked IO object)
"""    ResIO{Type}, ResIO"TYPE"

A structure describing the physical location of a game resource
(either from filesystem or from a BIF content),
marked with the resource name + type.

 - Since the resource type determines the `read`/`write` methods
   (and only takes a finite set of values) this is a type parameter.
 - The resource name (i.e. basename of the file without extension) is stored
   as a field. The name is canonicalized as upper-case.

Defined methods include:
 - `ResIO"EXT"`: macro defining static value for this file type.
 - `read(::ResIO{T})` (this opens the IO).

Concrete subtypes (i.e. `ResIOBuf`, `ResIOFile`) must implement:
 - `open(resource)`: returns an IO (this will automatically enable
   `open(f, resource)`; see `"base/io.jl"`).
 - `nameof(resource)`: returns the resource name as a String.

Specializations (i.e. `ResIO"DLG"` etc.) must implement:
 - `read(::ResIO{T}, io::IO)`.
"""
abstract type ResIO{T} end
macro ResIO_str(str)
	if contains(str, '.')
		(_, b) = splitext(str)
		return :(ResIO{$(QuoteNode(Symbol(lowercase(b[2:end]))))}($str))
	else
		return :(ResIO{$(QuoteNode(Symbol(lowercase(str))))})
	end
end
# macro Resource_str(s) ResIO{Symbol(lowercase(s))} end
@inline read(f::ResIO; kw...) = open(f) do io; read(io, f; kw...); end

mutable struct ResIOFile{T} <:ResIO{T}
	filename::String
end
@inline ResIO{T}(f::AbstractString) where{T} = ResIOFile{T}(f)
@inline ResIO(f::AbstractString) = 
	ResIO{Symbol(lowercase(splitext(basename(f))[2])[2:end])}(f)
@inline Base.open(r::ResIOFile) = open(r.filename)
@inline Base.nameof(r::ResIOFile) = lowercase(splitext(basename(r.filename))[1])

mutable struct ResIOBuf{T} <: ResIO{T}
	name::String
	buffer::IOBuffer
end
@inline Base.open(r::ResIOBuf) = r.buffer
@inline Base.nameof(r::ResIOBuf) = r.name

Resref(r::ResIO{T}) where{T} = Resref{T}(nameof(r))

# function write(filename::AbstractString, res::Resource)
# 	open(filename, "w") do io; write(io, res); end
# end

#»»1
include("dialogs.jl") # this needs Strref, so we put it here
# ««1 tlk
# ««2 Type and constructors
mutable struct TLK_str
	flags::UInt16
	sound::Resref"WAV"
	volume::Int32
	pitch::Int32
	offset::Int32
	length::Int32
	string::String
end
mutable struct TlkStrings
	constant::Constant"TLK V1  "
	lang::UInt16
	nstr::Int32
	offset::Int32
	entries::Vector{TLK_str}
	index::Dict{String,Int32}
end
@inline TlkStrings() = TlkStrings((), 0, 0, 0, TLK_str[], Dict{String,Int32}())
@inline Base.show(io::IO, tlk::TlkStrings) =
	print(io, "<TlkStrings with ", length(tlk.entries), " entries>")
@inline Base.isempty(v::TlkStrings) = isempty(v.entries)
@inline Base.length(v::TlkStrings) = length(v.entries)

#««2 I/O from tlk file
function read(io::IO, ::Type{<:TlkStrings})
	f = unpack(io, TlkStrings)
	unpack!(io, f.entries, TLK_str, f.nstr)
	sizehint!(empty!(f.index), f.nstr)
	for (i,s) in pairs(f.entries)
		s.string = string0(io, f.offset + s.offset, s.length)
		f.index[s.string] = i
	end
	return f
end
function write(io::IO, f::TlkStrings)
	f.offset = 18 + 26*length(f.entries)
	f.nstr = length(f.entries)
	offset = 0
	for s in f.entries
		s.length = length(codeunits(s.string)) # zero byte not included
		s.offset = offset
		offset+= s.length
	end
	pack(io, f) # this also writes the TLK_str entries (without the strings)
	@assert position(io) == f.offset
	for s in f.entries
		@assert position(io) == f.offset + s.offset
		write(io, codeunits(s.string)) # zero byte not included
	end
end
#««2 Dictionary interface
function Base.push!(tlk::TlkStrings, s::AbstractString)
	i = get(tlk.index, s, nothing)
	if isnothing(i)
		push!(tlk.entries, s)
		i = tlk.index[s] = length(tlk.entries)
	end
	return Strref(i-1)
end
function Base.get(f::TlkStrings, s::AbstractString, n)
	i = get(f.index, s, nothing)
	isnothing(i) && return n
	return Strref(i-1)
end
function Base.getindex(f::TlkStrings, s::Strref)
	i = s.index
	i ∈ eachindex(f.entries) || (i = 0)
	f.entries[i+1]
end
Base.findall(r::Union{Regex,AbstractString}, tlk::TlkStrings) =
	[ Strref(i-1) for (i,s) in pairs(tlk.entries) if contains(s.string, r) ]

# ««1 key/bif
# ««2 Integer types
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
# ««2 ResIO type table

# Static correspondence between UInt16 and strings (actually symbols).
const RESOURCE_TABLE = Dict{UInt16,String}(#««
	0x0001 => "bmp",
	0x0002 => "mve",
	0x0004 => "wav",
	0x0006 => "plt",
	0x03E8 => "bam",
	0x03E9 => "wed",
	0x03EA => "chu",
	0x03EB => "tis",
	0x03EC => "mos",
	0x03ED => "itm",
	0x03EE => "spl",
	0x03EF => "bcs",
	0x03F0 => "ids",
	0x03F1 => "cre",
	0x03F2 => "are",
	0x03F3 => "dlg",
	0x03F4 => "2da",
	0x03F5 => "gam",
	0x03F6 => "sto",
	0x03F7 => "wmp",
	0x03F8 => "chr",
	0x03F9 => "bs",
	0x03FA => "chr2",
	0x03FB => "vvc",
	0x03FC => "vfc",
	0x03FD => "pro",
	0x03FE => "bio",
	0x03FF => "wbm",
	0x0400 => "fnt",
	0x0402 => "gui",
	0x0403 => "sql",
	0x0404 => "pvrz",
	0x0405 => "glsl",
	0x0408 => "menu",
	0x0409 => "lua",
	0x040A => "ttf",
	0x040B => "png",
	0x044C => "bah",
	0x0802 => "ini",
	0x0803 => "src",
)#»»
@inline String(x::Restype) = get(RESOURCE_TABLE, x.data, x.data|>repr)
@inline Base.Symbol(x::Restype) = Symbol(String(x))

# ««2 File blocks
struct KEY_hdr
	constant::StaticString{8} # "KEY V1  "
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
# ««2 KeyIndex structure and methods
"""    KeyIndex

Structure containing the index of BIF resources as described in `"chitin.key"`.
Methods include:

 - `ResIO(key, resref)`: converts a `Resref` to a `ResIO` of matching type.
 - `read(key, resref)`: opens the resource and reads it.
 - `names(key, ResIO"type")`: returns an iterator over all names of resources
   of this type present in the game.
 - `findall(ResIO"type", key)`: returns an iterator over all resources.
"""
struct KeyIndex
	directory::Base.RefValue{String}
	bif::Vector{String}
# 	resources::Vector{KEY_res}
	location::Dict{Tuple{StaticString{8},Symbol},BifIndex}
	function KeyIndex(dir, bif, res::AbstractVector{KEY_res})
		loc = Dict((lowercase(r.name), Symbol(r.type)) => r.location for r in res)
		new(Ref(dir), bif, res, loc)
	end
	@inline KeyIndex() =
		new(Ref(""), [], Dict{Tuple{StaticString{8},Symbol},BifIndex}())
end
function Base.push!(key::KeyIndex, res::KEY_res)
	k = (lowercase(res.name), Symbol(res.type))
	key.location[k] = res.location
# 	push!(key.resources, res)
end
@inline Base.length(key::KeyIndex) = length(key.location)

init!(key::KeyIndex, filename::AbstractString) = open(filename) do io
	key.directory[] = dirname(filename)
	header = unpack(io, KEY_hdr)
	bifentries = unpack(seek(io, header.bifoffset), KEY_bif, header.nbif)
	push!(key.bif, (string0(io, x.offset, x.namelength) for x in bifentries)...)
	for res in unpack(seek(io, header.resoffset), KEY_res, header.nres)
		push!(key, res)
	end
	return key
end

KeyIndex(filename::AbstractString) = init!(KeyIndex(), filename)
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
	constant::StaticString{8} # "BIFFV1  "
	nres::UInt32
	ntilesets::UInt32
	offset::UInt32
end

struct BIF_resource
	locator::BifIndex
	offset::UInt32
	size::UInt32
	type::Restype
	_1::UInt16
end

struct BIF_tileset
	locator::BifIndex
	offset::UInt32
	ntiles::UInt32
	size::UInt32
	constant::UInt16 # 0x03eb
	_1::UInt16
end

bifcontent(file::AbstractString, index::Integer) = open(file, "r") do io
	header = unpack(io, BIF_hdr)
	seek(io, header.offset)
	resources = unpack(io, BIF_resource, header.nres)
	IOBuffer(read(seek(io, resources[index+1].offset), resources[index+1].size))
end

function (::Type{<:ResIO{T}})(key::KeyIndex, name) where{T}
	name = lowercase(name)
	loc = get(key.location, (StaticString{8}(name), T), nothing)
	isnothing(loc) && return nothing
	bif = joinpath(key.directory[], key.bif[1+sourcefile(loc)])
	return ResIOBuf{T}(String(name), bifcontent(bif, resourceindex(loc)))
end
Base.names(key::KeyIndex, ::Type{ResIO{T}}) where{T} =
	(r[1] for r in keys(key.location) if r[2] == T)
Base.findall(T::Type{<:ResIO}, key::KeyIndex) =
	(T(key, x) for x in names(key,T))

# ««1 ids
# useful ones: PROJECTL SONGLIST ITEMCAT NPC ANISND ?
function read(io::IO, f::ResIO"IDS"; debug=false)
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
function read(io::IO, f::ResIO"2DA"; debug=false, aligned=false)
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
	constant::StaticString{8} # "CRE V1.0"
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
function read(io::IO, f::ResIO"CRE")
	unpack(io, CRE_hdr)
end
# ««1 itm
# ««2 Enums etc.
@SymbolicFlags ItemFlag::UInt32 begin # ITEMFLAG.IDS
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
@SymbolicEnum ItemCat::UInt16 begin # ITEMCAT.IDS
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
@SymbolicFlags UsabilityFlags::UInt64 begin
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
	# byte 5 (kit 1)
	Cleric_Talos
	Cleric_Helm
	Cleric_Lathander
	Totemic_Druid
	Shapeshifter
	Avenger
	Barbarian
	Wildmage
	# byte 6 (kit 2)
	Stalker
	Beastmaster
	Assassin
	Bounty_Hunter
	Swashbuckler
	Blade
	Jester
	Skald
	# byte 7 (kit 3)
	Diviner
	Enchanter
	Illusionist
	Invoker
	Necromancer
	Transmuter
	Generalist
	Archer
	# Byte 8 (kit 4)
	Berserker
	Wizard_Slayer
	Kensai
	Cavalier
	Inquisitor
	Undead_Hunter
	Abjurer
	Conjurer
end
struct ItemAnimation; name::StaticString{2}; end
@SymbolicEnum WProf::UInt8 begin # WPROF.IDS
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
@SymbolicEnum AttackType::UInt8 begin # not found
	None = 0
	Melee
	Ranged
	Magical
	Launcher
end
@SymbolicEnum TargetType::UInt8 begin # not found
	Invalid = 0
	LivingActor = 1
	Inventory = 2
	DeadActor = 3
	AnyPoint = 4
	Caster = 5
	CasterInstant = 7
end
@SymbolicEnum EffectTarget::UInt8 begin
	None = 0
	Self = 1
	Projectile_Target = 2
	Party = 3
	Everyone = 4
	Everyone_except_party = 5
	Caster_group = 6
	Target_group = 7
	Everyone_except_self = 8
	Original_caster = 9
end
@SymbolicEnum LauncherType::UInt8 begin # not found
	None = 0
	Bow
	Crossbow
	Sling
	Spear = 40
	ThrowingAxe = 100
end
@SymbolicEnum DamageType::UInt16 begin # not found
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
@SymbolicEnum TimingMode::UInt8 begin
	Seconds = 0
	Permanent
	While_Equipped
	Delay_Seconds
	Delay_Permanent
	Delay_While_Equipped
	Limited_after_duration
	Permanent_after_duration
	While_Equipped_after_duration
	Never_expires
	Ticks
end
@SymbolicFlags DispelMode::UInt8 begin
	Dispellable
	NotResistable
end
# ««2 Data blocks
@with_kw mutable struct ITM_effect
	opcode::Opcodes.Opcode
	target::EffectTarget
	power::UInt8
	parameters::SVector{2,UInt32}
	timing_mode::TimingMode
	dispel_mode::DispelMode
	duration::UInt32
	probabilities::SVector{2,UInt8}
	resource::Resref"spl"
	dice_thrown::Int32
	dice_sides::Int32
	saving_throw_type::UInt32
	saving_throw_bonus::Int32
	stacking_id::UInt32
end
@with_kw mutable struct ITM_ability
 	attack_type::AttackType
 	must_identify::UInt8
 	location::UInt8
 	alternative_dice_sides::UInt8
	use_icon::Resref"BAM"
	target_type::TargetType
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
	damage_type::DamageType
	effect_count::UInt16
	effect_index::UInt16
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
	effects::Vector{ITM_effect}
end
mutable struct ITM_hdr{S}
	self::Resource"itm"
	modified::Bool
	constant::StaticString{8} # "ITM V1  "
	unidentified_name::S
	identified_name::S
	replacement::Resource"ITM"
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
	inventoryicon::Resource"BAM"
	lore::UInt16
	groundicon::Resource"BAM"
	weight::Int32
	unidentified_description::S
	identified_description::S
	description_icon::Resource"BAM"
	enchantment::Int32
# 	offset(abilities)::UInt32
# 	length(abilities)::UInt16
	abilities_offset::UInt32
	abilities_count::UInt16
# 	offset(effects)::UInt32
	effect_offset::UInt32
	effect_index::UInt16
# 	length(effects)::UInt16
	effect_count::UInt16
	abilities::Vector{ITM_ability}
	effects::Vector{ITM_effect}
end
# ignore "self" and "modified" field on IO
@inline Pack.fieldunpack(::IO, ::Val{:self}, T::Type{<:Resource}) = T("", "")
@inline Pack.fieldpack(::IO, ::Val{:self}, ::Resource) = 0
@inline Pack.packed_sizeof(::Val{:self}, ::Type{<:Resource}) = 0

@inline Pack.fieldunpack(::IO, ::Val{:modified}, ::Type{Bool}) = false
@inline Pack.fieldpack(::IO, ::Val{:modified}, ::Bool) = 0
@inline Pack.packed_sizeof(::Val{:modified}, ::Type{Bool}) = 0

# effects are written “by hand” at the end of the item file
@inline Pack.fieldpack(::IO, ::Val{:effects}, ::Vector{ITM_effect}) = 0

@inline setself!(x::T, res::ResIO) where{T} =
	x.self = fieldtype(T, :self)("", nameof(res))

@inline searchkey(i::ITM_hdr) = i.identified_name
# create a virtual “not_usable_by” item property which groups together
# all the 5 usability fields in the item struct:
@inline function Base.getproperty(i::ITM_hdr, name::Symbol)
	name ∈ fieldnames(typeof(i)) && return getfield(i, name)
	name == :not_usable_by && return UsabilityFlags(
		UInt64(i.usability) | UInt64(i.kit1) << 32 | UInt64(i.kit2) << 40 |
		UInt64(i.kit3) << 48 | UInt64(i.kit4) << 56)
end
@inline function Base.setproperty!(x::ITM_hdr, name::Symbol, value)
	setfield!(x, :modified, true)
	for (fn, ft) in zip(fieldnames(typeof(x)), fieldtypes(typeof(x)))
		ft == Strref && value isa AbstractString && (value = Strref(game, value))
		name == fn && return setfield!(x, name, ft(value))
	end
	if name == :not_usable_by
		x.usability = UInt64(value) % UInt32
		x.kit1 = (UInt64(value) >> 32) % UInt8
		x.kit2 = (UInt64(value) >> 40) % UInt8
		x.kit3 = (UInt64(value) >> 48) % UInt8
		x.kit4 = (UInt64(value) >> 56) % UInt8
		return value
	end
	error("unknown field name \"$name\"")
end
# ««2 Item function
function read(io::IO, res::ResIO"ITM")
	itm = unpack(io, ITM_hdr{Strref})
	setself!(itm, res)
	unpack!(seek(io, itm.abilities_offset), itm.abilities, ITM_ability,
		itm.abilities_count)
	unpack!(seek(io, itm.effect_offset), itm.effects, ITM_effect,
		itm.effect_count)
	for (i, ab) in pairs(itm.abilities)
		unpack!(io, ab.effects, ITM_effect, ab.effect_count)
	end
	return itm
end
function Base.write(io::IO, itm::ITM_hdr)
	itm.abilities_offset = 114
	itm.abilities_count = length(itm.abilities)
	itm.effect_offset = 114 + length(itm.abilities)*packed_sizeof(ITM_ability)
	itm.effect_index = 0 # FIXME
	n = itm.effect_count = length(itm.effects)
	for ab in itm.abilities
		ab.effect_index = n
		ab.effect_count = length(ab.effects)
		n+= ab.effect_count
	end
	pack(io, itm)
	@printf("\e[32mpacking main effects at %d = 0x%x\e[m", position(io), position(io))
	pack(io, itm.effects)
	for ab in itm.abilities;
	@printf("\e[32mpacking ab effects at offset %d = 0x%x\e[m", position(io), position(io))
	pack(io, ab.effects); end
end
function show_effect(io::IO, eff::ITM_effect)
	print(io, """
\e[48;5;13m  $(@sprintf("%63s  ", string(Int16(eff.opcode);base=16)*Opcodes.str(eff.opcode, eff.parameters...)*" on "*rp(eff.target)))\e[m
$(eff.dice_thrown)d$(eff.dice_sides), save $(eff.saving_throw_type)$(@sprintf("%+d", eff.saving_throw_bonus)) parameters $(eff.parameters[1]),$(eff.parameters[2])
Duration $(eff.duration) timing_mode $(eff.timing_mode|>rp) dispel res. $(eff.dispel_mode|>rp) probabilities $(eff.probabilities[1]),$(eff.probabilities[2])
""")
end
function show_ability(io::IO, itm::ITM_hdr, i::Integer)
	ab = itm.abilities[i]
	h = @sprintf("%s %+d %dd%d%+d %s speed %d",
		rp(ab.attack_type), ab.thac0_bonus,
		ab.dice_thrown, ab.dice_sides, ab.damage_bonus, rp(ab.damage_type), ab.speed_factor)
	print(io, """
\e[48;5;12m ($i/$(length(itm.abilities))) $(rpad(h, 60))\e[m
Range $(ab.range), $(ab.target_count)*$(rp(ab.target_type))
Alternative $(ab.alternative_dice_thrown)d$(ab.alternative_dice_sides)
Effects $(ab.effect_index+1):$(ab.effect_index+ab.effect_count) ($(ab.effect_count) total)
""")
	for eff in ab.effects; show_effect(io, eff); end
# 	print(io,"""
# \e[34;7mAbility $i/$(itm.abilities_count)$(' '^40)\e[m
# Attack_type:$(rp(ab.attack_type)) $(rp(ab.damage_type)) $(ab.dice_thrown)d$(ab.dice_sides)+$(ab.damage_bonus), thac0 $(ab.thac0_bonus), speed $(ab.speed_factor)
# Alternative $(ab.alternative_dice_thrown)d$(ab.alternative_dice_sides)
# Range $(ab.range), $(ab.target_count) target(s) of type $(rp(ab.target_type))
# """)
end
function Base.show(io::IO, mime::MIME"text/plain", itm::ITM_hdr)
	header=@sprintf("%26s/%-32s ⚖%-3d ❍%-5d ❝%-3d ",
		str(itm.unidentified_name), str(itm.identified_name),
		itm.weight, itm.price, itm.lore)
	chars=@sprintf("Str:\e[35m%2d/%2d\e[m Dex:\e[35m%2d\e[m Con:\e[35m%2d\e[m Wis:\e[35m%2d\e[m Int:\e[35m%2d\e[m Cha:\e[35m%2d\e[m Level:\e[35m% 3d\e[m",
		itm.minstrength, itm.minstrengthbonus,
		itm.mindexterity, itm.minconstitution, itm.minwisdom, itm.minintelligence,
		itm.mincharisma, itm.minlevel)
	nub = itm.not_usable_by
	if count_ones(nub.n) > count_zeros(nub.n)
		use = "Usable by: \e[32m"*rp(~nub)*"\e[m"
	else
		use = "Not usable by: \e[31m"*rp(nub)*"\e[m"
	end
	print(io, """
\e[7m$header\e[m
Flags: \e[36m$(rp(itm.flags))\e[m
Type: \e[36m$(rp(itm.item_type))\e[m Proficiency: \e[36m$(rp(itm.proficiency))\e[m Ench.\e[36m$(itm.enchantment)\e[m
$use
Requires: $chars
Inventory: \e[34m$(itm.inventoryicon.name)\e[m stack=\e[34m$(itm.stackamount)\e[m groundicon=\e[34m$(itm.groundicon.name)\e[m Animation: \e[34m$(itm.animation.name)\e[m Image=\e[34m$(itm.description_icon.name)\e[m
Casting effects: $(itm.effect_index)
""")
	for eff in itm.effects; show_effect(io, eff); end
	for i in eachindex(itm.abilities); show_ability(io, itm, i); end
# 	print(io,"""
# \e[34;7mAbility $i/$(itm.abilities_count)$(' '^40)\e[m
# Attack_type:$(rp(ab.attack_type)) $(rp(ab.damage_type)) $(ab.dice_thrown)d$(ab.dice_sides)+$(ab.damage_bonus), thac0 $(ab.thac0_bonus), speed $(ab.speed_factor)
# """)
	# TODO: make a nice display method for opcodes, move this to Opcodes
	# (and double that with a constructor)
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
	constant::StaticString{8} # "DLG V1.0"
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
# 	st_triggers::Vector{DLG_string}
# 	tr_triggers::Vector{DLG_string}
# 	actions::Vector{DLG_string}
end

@inline dialog_strings(io::IO, offset, count)= [string0(io, s.offset, s.length)
	for s in unpack(seek(io, offset), DLG_string, count)]


function read(io::IO, f::ResIO"DLG")
	self = Resref(f)
	header = unpack(io, DLG_hdr)
	states = unpack(seek(io,header.offset_states), DLG_state{Strref},
		header.number_states)
	transitions = unpack(seek(io, header.offset_transitions),
		DLG_transition{Strref}, header.number_transitions)
	st_triggers = dialog_strings(io, header.offset_state_triggers,
			header.number_state_triggers)
	tr_triggers = dialog_strings(io, header.offset_transition_triggers,
			header.number_transition_triggers)
	actions = dialog_strings(io, header.offset_actions, header.number_actions)

	dialog = Dialogs.top
	Dialogs.namespace("")
	getval(list, i) = i+1 ∈ eachindex(list) ? list[i+1] : "bad ref $i"
	for (i, s) in pairs(states)
		state = Dialogs.add_state!(dialog, (self|>nameof, i-1);
			text = s.text, trigger = getval(st_triggers, s.trigger))
		for j in s.first_transition+1:s.first_transition+s.number_transitions
			t = transitions[j]
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
"""    Game

Main structure holding all top-level data for a game installation, including:
 - key/bif archived files,
 - table of override files,
 - tlk strings (TODO).

Methods include:

 - `ResIO(game, resref)`: converts a `Resref` to a `ResIO`
   (i.e. file descriptor) of the same type.
 - `read(game, resref)`: opens the corresponding resource and returns the
   appropriate Julia structure depending on the `Resref` type.
 - `filetype(game, resref)`: returns either 0 (resource not found),
   1 (resource found in bif), or 2 (resource found in override).
 - `names(game, ResIO"type")`: returns a vector of all names of
   existing resources of this type.
 - `findall(ResIO"type", game)`: returns an iterator over all existing
   resource descriptors of this type.

# Language data a list of pairs
 (regular expression, language file)
"""
struct Game
	directory::Base.RefValue{String}
	key::KeyIndex
	override::Dict{Symbol, Set{String}}
	language::Base.RefValue{Int}
	namespace::Base.RefValue{String}
	# FIXME: each tlk file is quite heavy (5 Mbytes in a fresh BG1
	# install), we could use a rotation system to not keep more than 4 or 5
	# in memory at the same time
	# (this is already likely during resource building since most mods are
	# written in 3 languages, but should be ensured during final
	# compilation)
	strings::Vector{TlkStrings}
	# we collect all new strings (for any language) in the same structure,
	# so that all the strref numbers advance in sync:
	new_strings::UniqueVector{Tuple{Int8,String}}
	@inline Game() = new(Ref(""), KeyIndex(), Dict{Symbol,Set{String}}(),
		Ref(0), Ref(""), [ TlkStrings() for _ in LANGUAGE_FILES ],
		UniqueVector{Tuple{Int8,String}}())
end

function Base.show(io::IO, game::Game)
	print(io, "<Game: ", length(game.key), " keys, ", length(game.override),
		" overrides, ", count(!isempty, game.strings), " languages>")
end

const LANGUAGE_FILES = (#««
	# We need to put the xxF before xx, because the regexp search goes
	# linearly through this list:
	r"^en.*"i => joinpath("en_US", "dialog.tlk"), # first value is default value
	r"^c[sz].*"i => joinpath("cs_CZ", "dialog.tlk"),
	r"^de.*F"i => joinpath("de_DE", "dialogF.tlk"),
	r"^de.*"i => joinpath("de_DE", "dialog.tlk"),
	r"^es.*F"i => joinpath("es_ES", "dialogF.tlk"),
	r"^es.*"i => joinpath("es_ES", "dialog.tlk"),
	r"^fr.*F"i => joinpath("fr_FR", "dialogF.tlk"),
	r"^fr.*"i => joinpath("fr_FR", "dialog.tlk"),
	r"^hu.*"i => joinpath("hu_HU", "dialog.tlk"),
	r"^it.*F"i => joinpath("it_IT", "dialogF.tlk"),
	r"^it.*"i => joinpath("it_IT", "dialog.tlk"),
	r"^ja.*F"i => joinpath("ja_JP", "dialogF.tlk"),
	r"^ja.*"i => joinpath("ja_JP", "dialog.tlk"),
	r"^ko.*"i => joinpath("ko_KR", "dialog.tlk"),
	r"^pl.*F"i => joinpath("pl_PL", "dialogF.tlk"),
	r"^pl.*"i => joinpath("pl_PL", "dialog.tlk"),
	r"^pt.*F"i => joinpath("pt_BR", "dialogF.tlk"),
	r"^pt.*"i => joinpath("pt_BR", "dialog.tlk"),
	r"^ru.*F"i => joinpath("ru_RU", "dialogF.tlk"),
	r"^ru.*"i => joinpath("ru_RU", "dialog.tlk"),
	r"^tr.*"i => joinpath("tr_TR", "dialog.tlk"),
	r"^uk.*"i => joinpath("uk_UA", "dialog.tlk"),
	r"^zh.*"i => joinpath("zh_CN", "dialog.tlk"),
)#»»
"""    Game(directory)

Initializes a `Game` structure from the game directory
(i.e. the directory containing the `"chitin.key"` file).
"""
Game(directory::AbstractString) = init!(Game(), directory)

function init!(game::Game, directory::AbstractString)
	game.namespace[] = ""
	game.directory[] = directory
	init!(game.key, joinpath(directory, "chitin.key"))
	println("read ", length(game.key), " key resources")
	empty!(game.override)
	o_dir = joinpath(directory, "override")
	if !isdir(o_dir)
		println("created override directory: ", o_dir)
		mkdir(o_dir)
	else
		for f in readdir(o_dir)
			(n, e) = splitext(lowercase(basename(f))); t = Symbol(e[2:end])
			push!(get!(game.override, t, Set{String}()), n)
		end
		println("read $(sum(length(v) for (_,v) in game.override; init=0)) override resources")
	end
	set_language!(game, 1) # default language
	game.namespace[] = "user"
	return game
end
"""    namespace(game, s)

Sets the current namespace for game resources being defined to `s`.
The following default namespaces are used:
 - `"main"` for original game resources;
 - `"user"` is the default namespace for added resources.
"""
@inline namespace(game::Game, s::AbstractString) =
	(game.namespace[] = s; return s)
@inline namespace(game::Game) = game.namespace[]

function set_language!(game::Game, i::Integer)
	game.language[] = i
	isempty(game.strings[i]) || return
	game.strings[i] =
		read(joinpath(game.directory[], "lang", LANGUAGE_FILES[i][2]), TlkStrings)
end
"""    language(game, s)

Sets the current language of the game (i.e. the language in which
strings entered as parameters for functions will be interpreted)
to the one given by string `s`.

Allowed values are:
$(join([replace(replace(repr(x[1]), r"(^r\"\^|\"i$|\.\*)"=>""), "*" => "\\*") for x in LANGUAGE_FILES], ", ")).
"""
function language(game::Game, s::AbstractString)
	for (i, (r, f)) in pairs(LANGUAGE_FILES)
		contains(s, r) || continue
		set_language!(game, i)
		return i
	end
	error("unknown language: "*s)
end
@inline language(game::Game) = game.language[]
@inline strings(game::Game, i = language(game)) = game.strings[i]
function Strref(game::Game, s::AbstractString)
	i = get(strings(game), s, nothing)
	isnothing(i) || return i
	i = findfirst!(isequal((language(game), s),), game.new_strings)
	return Strref(i-1 + length(strings(game)))
end
@inline Base.convert(T::Type{Strref}, s::AbstractString) = T(game, s)

function str(game::Game, s::Strref)
	i = s.index+1
	i ≤ length(strings(game)) && return strings(game).entries[i].string
	return game.new_strings[i - length(strings(game))][2]
end

" returns 2 if override, 1 if key/bif, 0 if not existing."
function filetype(game::Game,::Type{ResIO{T}}, name::AbstractString) where{T}
	haskey(game.override, T) && lowercase(String(name)) ∈ game.override[T] &&
		return 2
	haskey(game.key.location, (StaticString{8}(name), T)) && return 1
	return 0
end
function ResIO{T}(game::Game, name::AbstractString) where{T}
	t = filetype(game, ResIO{T}, name)
	if t == 2
		file = joinpath(game.directory, "override", String(name)*'.'*String(T))
		return ResIOFile{T}(file)
	elseif t == 1
		return ResIO{T}(game.key, name)
	else
		error("resource not found: $name.$T")
	end
end
@inline ResIO(k::Union{Game,KeyIndex}, resref::Resref{T}) where{T} =
	ResIO{T}(k, nameof(resref))

@inline read(k::Union{Game,KeyIndex}, resref::Resref; kw...) =
	read(ResIO(k, resref); kw...)
@inline filetype(game::Game, resref::Resref{T}) where{T} =
	filetype(game, ResIO{T}, resref)

@inline Base.names(game::Game, ::Type{ResIO{T}}) where{T} =
	get(game.override,T, String[]) ∪ names(game.key, ResIO{T})
@inline Base.findall(R::Type{<:ResIO}, game::Game) =
	(R(game, n) for n in names(game, R))

"""    search(game, strings, ResIO"type", text)

Searches for the given text (string or regular expression)
in the names of all resources of the given `type`.
`strings` is the string database used for translation.
"""
function search(game::Game, str::TlkStrings, R::Type{<:ResIO}, text)
	for res in all(game, R)
		s = str[searchkey(read(res))].string
		contains(s, text) || continue
		@printf("%c%-8s %s\n", res isa ResIOFile ? '*' : ' ', nameof(res), s)
	end
end

# Item/etc. generator ««1
# TODO: make this work even for non-mutable types:
# (hard since we are overloading setproperty!)
function clone(x::T; kwargs...) where{T}
	vars = (get(kwargs, fn, getfield(x, fn)) for fn in fieldnames(T))
	y = T(vars...)
	# TODO: add "args..." field and push stuff to kwargs depending
	# on args and T (e.g. Item => identified_name)
	for (k, v) in pairs(kwargs)
		setproperty!(y, k, v)
	end
	y
end

# »»1

# shorthand: strings*dialog instantiates Strrefs
for T in (:ITM_hdr,)
	@eval @inline Base.:*(str::TlkStrings, x::$T{Strref}) =
		Functors.functor(x->str[x].string::String, $T, x)
end

# Base.:*(str::TlkStrings, x::Dialogs.StateMachine{Int32,Any}) =
# 	Functors.functor(x->str[x].string::String,
# 		Dialogs.StateMachine{Int32,S,String,Any} where{S}, x)

const game = Game()
for f in (:language, :init!, :str, :strref)
	@eval function $f(args...; kwargs...)
		!isempty(args) && first(args) isa Game &&
			error("no such method: ", $(string(f)), typeof.(args[2:end]), )
		$f(game, args...; kwargs...)
	end
end

end # module
IE=InfinityEngine; S=IE.Pack

# str = read("../bg/game/lang/en_US/dialog.tlk", IE.TLK_hdr)

# itm = read(IE.ResIO"../ciopfs/bg2/game/override/sw1h06.itm")
# itm = read(IE.ResIO"../ciopfs/bg2/game/override/sw1h08.itm")
IE.init!("../bg/game")
r=IE.Resref"sw1h34.itm"
# game = IE.Game("../bg/game")
itm=read(IE.game, IE.Resref"sw1h34.itm") # Albruin
itm1=IE.clone(itm, unidentified_name="Imoen")
write("../bg/game/override/jp01.itm", itm1)
 
# itm=read(game, IE.Resref"blun01.itm")
# write("/tmp/a.itm", itm)

# using Printf
# function xxd(x::Vector{UInt8}; offset=0)#««
# 	for i in 1:offset
# 		print("..")
# 		iseven(i) && print(" ")
# 	end
# 	for i in 0:length(x)-1
# 		@printf("%02x", x[i+1])
# 		isodd(i+offset) && print(" ")
# 		iszero((i+offset+1)%16) && print("\n")
# 	end
# end#»»

# str=read(IE.ResIO"../bg/game/lang/fr_FR/dialog.tlk")
# # # IE.search(game, str, IE.ResIO"ITM", "Varscona")
# # key = IE.KeyIndex("../bg/game/chitin.key")
# # # dlg = read(game, IE.Resref"melica.dlg")
# dlg = read(game, IE.Resref"zorl.dlg")
# # # dia = read(IE.ResIO"dlg"(key, "abazigal"))
# # 
# D=IE.Dialogs
# D.namespace("test")
# D.actor("zorl")
# 
# D.trigger("// new dialogue")
# D.say(1 => "hello <CHARNAME>")
# 	D.reply(" hello Zorl" => 1)
# 	D.reply(" go on...")
# D.say(2 => "Yes I go on.")
# 
# D.state(1)
# 	D.reply("I attack!" => exit)
# # 
# # nothing
# # dlg;
