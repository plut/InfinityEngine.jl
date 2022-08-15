module InfinityEngine
module DiceThrows
"""    Dice

Allows D&D syntax for dice throws, e.g. 2d4+3.
"""
struct Dice{I<:Integer}
	thrown::I
	sides::I
	bonus::I
end
@inline Dice(a, b) = Dice(promote(a,b)...)
@inline Dice(a::I, b::I, c = zero(I)) where{I<:Integer} = Dice{I}(a,b,c)
function Base.show(io::IO, d::Dice)
	!isone(d.thrown) && print(io, d.thrown)
	print(io, 'd', d.sides)
	d.bonus > 0 && print(io, '+', d.bonus)
	d.bonus < 0 && print(io, d.bonus)
end
@inline Base.:*(a::Integer, d::Dice{I}) where{I} =
	(@assert isone(d.thrown) && iszero(d.bonus); Dice{I}(I(a), d.sides, zero(I)))
@inline Base.:+(d::Dice{I}, b::Integer) where{I} =
	Dice{I}(d.thrown, d.sides, d.bonus + I(b))
@inline Base.:-(d::Dice{I}, b::Integer) where{I} =
	Dice{I}(d.thrown, d.sides, d.bonus - I(b))
@inline Base.iszero(d::Dice) =
	(iszero(d.thrown) || iszero(d.sides)) && iszero(d.bonus)
@inline doubleaverage(d::Dice) = d.thrown * (d.sides+one(d.sides)) + d.bonus<<1
@inline average(d::Dice) = doubleaverage(d)//2

@inline Base.rand(d::Dice) =
	sum(rand(Base.OneTo(d.sides)) for _ in Base.OneTo(d.thrown)) + d.bonus

for i in (2,3,4,5,6,8,10,12,20)
	@eval $(Symbol("d"*string(i))) = Dice{Int}(1,$i,0)
end
export Dice, d2, d3, d4, d5, d6, d8, d10, d12, d20
end

using .DiceThrows
include("pack.jl"); using .Pack
include("dottedenums.jl"); using .DottedEnums
include("opcodes.jl"); using .Opcodes: Opcode

using Printf
using StaticArrays
using Parameters
using UniqueVectors
using Random
import Base.read, Base.write

# ««1 Basic types
struct Auto{A<:Tuple, B}
	args::A
	kwargs::B
	Auto(args...; kwargs...) = new{typeof(args),typeof(kwargs)}(args, kwargs)
end
@inline Base.convert(T::DataType, auto::Auto) = T(auto.args...; auto.kwargs...)
# Compact representation, useful for Flags and Enums
@inline rp(x) = repr(x;context=(:compact=>true))
@inline f75(s::AbstractString) = length(s) ≤ 75 ? s : first(s,75)*'…'
# ««2 Extract zero-terminated string from IO
@inline function string0(v::AbstractVector{UInt8})
	l = findfirst(iszero, v)
	String(isnothing(l) ? v : view(v, 1:l-1))
end
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))
@inline string0(s::AbstractString) = let n = findfirst('\0', s)
	isnothing(n) && (n = length(s)+1)
	view(s, 1:n-1)
end

# ««2 Static length strings
struct StaticString{N} <: AbstractString
	chars::SVector{N,UInt8}
	# once we encounter a zero character, all that follows is zero:
	@inline StaticString{N}(chars::AbstractVector{UInt8}) where{N} =
		(@assert length(chars) ≤ N;
	new{N}(SVector{N,UInt8}(any(iszero, view(chars, 1:min(i-1,length(chars)))) ?
			zero(UInt8) : get(chars, i, zero(UInt8)) for i in 1:N)))
end
@inline Base.sizeof(::StaticString{N}) where{N} = N
@inline Base.ncodeunits(s::StaticString) =
	let l = findfirst(iszero, s.chars); isnothing(l) ? sizeof(s) : l-1; end
@inline Base.codeunit(s::StaticString, i::Integer) = s.chars[i]
@inline Base.codeunit(::StaticString) = UInt8
@inline Base.lowercase(s::StaticString{N}) where{N} =
	StaticString{N}(SVector{N,UInt8}(c|>Char|>lowercase|>UInt8 for c in s.chars))
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
	return StaticString{N}(s|>lowercase|>codeunits)
end
@inline StaticString{N}(s::StaticString{N}) where{N} = s

@inline Pack.unpack(io::IO, T::Type{StaticString{N}}) where{N} = T(read(io, N))
@inline Pack.pack(io::IO, s::StaticString) = write(io, s.chars)

@inline charuc(x::UInt8) = (0x61 ≤ x ≤ 0x7a) ? x-0x20 : x
@inline charlc(x::UInt8) = (0x41 ≤ x ≤ 0x5a) ? x+0x20 : x
@inline Base.uppercase(s::StaticString) = typeof(s)(charuc.(s.chars))
@inline Base.lowercase(s::StaticString) = typeof(s)(charlc.(s.chars))



#««2 Strref
"""    Strref

Index (32-bit) referring to a translated string in dialog.tlk or dialogF.tlk.
"""
struct Strref
	index::Int32
end
@inline Strref(x::Strref) = x
@inline Base.isvalid(s::Strref) = (s.index > 0)
@inline Base.show(io::IO, s::Strref) = print(io, "Strref(", s.index, ")")

#««2 Resource identifiers: Resref
"""    Resref{T}

A (long) resource descriptor: this contains a (static) type and a (dynamic)
namespace and name uniquely identifying the resource.
"""
struct Resref{T}
	name::String # of the form 'namespace/resource'
end
resourcetype(::Type{<:Resref{T}}) where{T} = T
resourcetype(r::Resref) = resourcetype(typeof(r))
@inline resref_join(ns::AbstractString, n::AbstractString) =
	isempty(ns) ? n : ns*'/'*n
@inline (T::Type{<:Resref})(ns::AbstractString, n::AbstractString) =
	T(resref_join(ns, n))
@inline (T::Type{<:Resref})(x::Resref) = T(x.name)

function Base.split(r::Resref)
	j = findlast('/', r.name)
	isnothing(j) ? ("", r.name) : (r.name[1:j-1], r.name[j+1:end])
end
@inline Base.nameof(r::Resref) = split(r)|>last
@inline namespace(r::Resref) = split(r)|>first
# collision happens after Ω(36^2) copies of *the same resource*...
# Base.rand(r::Resref, ns = namespace(r)) =
# 	typeof(r)(ns, replace(nameof(r), r"~.*" => "")*'~'*lowercase(randstring(4)))
Base.show(io::IO, r::Resref) =
	print(io, "Resref\"", r.name, '.', resourcetype(r), '"')

@inline Pack.unpack(io::IO, T::Type{<:Resref}) =
	unpack(io, StaticString{8})|>lowercase|>longref|>T
@inline Pack.packed_sizeof(::Type{<:Resref}) = 8
@inline Pack.pack(io::IO, r::Resref) = pack(io, r.name|>shortref|>uppercase)

macro Resref_str(s)
	s = lowercase(s)
	i = findlast('.', s)
	isnothing(i) ? Resref{Symbol(s)} :
		Resref{Symbol(view(s, i+1:length(s)))}(view(s, 1:i-1))
end

#««2 Rooted resources
"""    RootedResource

A game resource holding a reference to the root resource
(i.e. the one which will eventually be saved in a game file).

The following methods needs to be defined:

 - `root(x)`: returns the root object
 - `root!(x, r)`: sets the root object
 - `register!(root)`: marks this root object as modified.
"""
abstract type RootedResource end
# default field is x.root (actually the only field we use)
@inline root(x::RootedResource) = isdefined(x, :root) ? x.root : nothing
@inline root!(x::RootedResource, r) = (x.root = r)
"""    RootResource

The root of a resource tree, i.e. a resource which will be saved in a game file.
This resource also holds an identifier designating it (and the game file).
"""
abstract type RootResource{T} <: RootedResource end
# Root resource is always its own root
@inline root(x::RootResource) = x
@inline root!(x::RootResource, _) = nothing
@inline resourcetype(::RootResource{T}) where{T} = T
@inline register!(::Nothing) = nothing # no root defined

mutable struct RootedResourceVector{T<:RootedResource,
		R<:RootResource} <: AbstractVector{T}
	v::Vector{T}
	root::R
	# new is *not* a variadic function:
	RootedResourceVector{T,R}(v,r) where{T,R} = new{T,R}(v,r)
	RootedResourceVector{T,R}(v) where{T,R} = new{T,R}(v)
end
@inline RootedResourceVector{T,R}(::UndefInitializer) where{T,R} =
	RootedResourceVector{T,R}()
@inline root(v::RootedResourceVector) = isdefined(v, :root) ? v.root : nothing
@inline root!(v::RootedResourceVector, r) = (v.root = r)
@inline Pack.packed_sizeof(x::RootedResourceVector) = length(x)*packed_sizeof(eltype(x))

#««3 getproperty/setproperty! etc.
@inline rr_skipproperties(::Any) = ()
@inline default_setproperty!(x, f::Symbol, v) =
	# default code; duplicated from Base.jl
	setfield!(x, f, convert(fieldtype(typeof(x), f), v))
@inline function rr_setproperty!(x::RootedResource, f::Symbol, v)
	default_setproperty!(x, f, v)
	# we need to put the register! call last, in case the `ref` property
	# was changed:
	f ∈ rr_skipproperties(x) || register!(root(x))
end
@inline Base.setproperty!(x::RootedResource, f::Symbol, v) =
	rr_setproperty!(x, f, v)

# AbstractArray interface
@inline Base.size(v::RootedResourceVector) = size(v.v)
@inline Base.getindex(v::RootedResourceVector, i::Int) = v.v[i]
@inline Base.setindex!(v::RootedResourceVector, x, i::Int) =
	(register!(root(v)); setindex!(v.v, x, i))
@inline Base.resize!(v::RootedResourceVector, n::Integer) =
	(register!(root(v)); resize!(v.v, n); v)

"""    setroot!(x::RootedResource, newroot = x)

Recursively sets the root object for `x` and all its sub-fields."""
@inline setroot!(x, r) = x
@inline function setroot!(x::RootedResource, r = x)
	root!(x, r)
	for i in 1:nfields(x)
		setroot!(getfield(x, i), r)
	end
	return x
end
@inline function setroot!(x::RootedResourceVector, r)
	root!(x, r)
	for y in x
		setroot!(y, r)
	end
	return x
end
setroot!(x::AbstractVector, r) = for y in x; setroot!(y, r); end

# ««2 ResIO type (marked IO object)
"""    ResIO{Type}, ResIO"TYPE"

An `IO` object marked (statically) with the type of resource being read
(either from filesystem or from a BIF content),
as well as (dynamically) with global properties of the resource
(i.e. resource name, root object).

 - Since the resource type determines the `read`/`write` methods
   (and only takes a finite set of values) this is a type parameter.
 - The resource name (i.e. basename of the file without extension) is stored
   as a field. The name is canonicalized as upper-case.

Defined methods include:
 - `ResIO"EXT"`: macro defining static value for this file type.
"""
mutable struct ResIO{T,R<:RootResource,X<:IO} <: IO
	ref::Resref{T}
	io::X
	root::R
	@inline ResIO{T}(name, io::X) where{T,X<:IO} =
		new{T,ResourceType(ResIO{T}),X}(name, io)
end

@inline Base.read(x::ResIO, T::Type{UInt8}) = read(x.io, T)
@inline Base.seek(x::ResIO, n) = (seek(x.io, n); x)
@inline Base.position(x::ResIO) = position(x.io)
@inline Base.eof(x::ResIO) = eof(x.io)
@inline Base.close(x::ResIO) = close(x.io)

@inline Base.nameof(x::ResIO) = x.name
@inline root!(x::ResIO, r) = (x.root = r)

"`ResourceType{ResIO{T}}` = the root type to attach to this `ResIO` fd"
@inline ResourceType(T::Type{<:ResIO}) = Base.return_types(read,Tuple{T})|>first

macro ResIO_str(str)
	if contains(str, '.')
		(_, b) = splitext(str)
		return :(ResIO{$(b[2:end]|>lowercase|>Symbol|>QuoteNode)}($str))
	else
		return :(ResIO{$(str|>lowercase|>Symbol|>QuoteNode)})
	end
end
#««3 Reading resources from ResIO
# `root` is completely absent from the file:
@inline Pack.fieldpack(::IO, _, ::Val{:root}, ::RootResource) = 0
@inline Pack.fieldunpack(io::ResIO, _, ::Val{:root}, T::Type{<:RootResource}) =
	io.root
@inline Pack.packed_sizeof(_, ::Val{:root}, ::Type{<:RootResource}) = 0
@inline unpack_root(io::ResIO, T::Type{<:RootResource}) =
	(r = unpack(io, T); root!(io, r); r)

# The root resource contains a reference to its own name:
@inline Pack.fieldunpack(io::ResIO, _, ::Val{:ref}, T::Type{<:Resref}) = io.ref
@inline Pack.fieldpack(::IO, _, ::Val{:ref}, ::Resref) = 0
@inline Pack.packed_sizeof(_, ::Val{:ref}, ::Type{<:Resref}) = 0

@inline Pack.fieldunpack(::IO, _, _, T::Type{<:RootedResourceVector}) = T([])

#»»1
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
	unpack!(io, f.entries, f.nstr)
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
	constant::Constant"KEY V1  "
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

 - `open(key, resref)`: converts a `Resref` to a `ResIO` of matching type.
 - `names(key, Resref"type")`: returns an iterator over all names of resources
   of this type present in the game.
"""
struct KeyIndex
	directory::Base.RefValue{String}
	bif::Vector{String}
	location::Dict{Symbol,Dict{StaticString{8},BifIndex}}
# 	location::Dict{Tuple{StaticString{8},Symbol},BifIndex}
	@inline KeyIndex() =
		new(Ref(""), [], Dict{Symbol,Dict{Tuple{StaticString{8},BifIndex}}}())
end
function Base.push!(key::KeyIndex, res::KEY_res)
	d = get!(key.location, Symbol(res.type), Dict{StaticString{8},BifIndex}())
	d[lowercase(res.name)] = res.location
end
@inline Base.length(key::KeyIndex) = key.location|>values.|>length|>sum

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
@inline function Base.get(key::KeyIndex, name::AbstractString, type::Symbol)
	dict = get(key.location, type, nothing); isnothing(dict) && return nothing
	loc = get(dict, StaticString{8}(name), nothing)
	isnothing(loc) && return nothing
	bif = joinpath(key.directory[], key.bif[1+sourcefile(loc)])
	return bifcontent(bif, resourceindex(loc))
end
@inline Base.haskey(key::KeyIndex, name::AbstractString, type::Symbol) =
	haskey(key.location, type) && haskey(key.location[type], name)
@inline Base.names(key::KeyIndex, T::Type{<:Resref}) =
	keys(key.location[resourcetype(T)])

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
# ««1 itm
# ««2 Enums etc.
@SymbolicFlags ItemFlags::UInt32 begin # ITEMFLAG.IDS
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
@SymbolicEnum ItemType::UInt16 begin # ITEMCAT.IDS
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
  Book = 0x0025 # broken shield/bracelet
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
	ShortBow
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
#««2 Item effect
@with_kw mutable struct ItemEffect{R<:RootResource} <: RootedResource
	root::R
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
@inline function Base.getproperty(eff::ItemEffect, f::Symbol)
	f == :damage && return Dice(eff.dice_thrown, eff.dice_sides)
	getfield(eff, f)
end
@inline function Base.setproperty!(eff::ItemEffect, f::Symbol, x)
	if f == :damage
		@assert x isa Dice
		@assert iszero(x.bonus)
		eff.dice_thrown = x.thrown
		eff.dice_sides = x.sides
		return x
	end
	default_setproperty!(eff, f, x)
end
function Base.show(io::IO, ::MIME"text/plain", eff::ItemEffect)
	@printf(io, "\e[48;5;13m%63s\e[m\n", string(Int16(eff.opcode);base=16)*'='
		*Opcodes.str(eff.opcode, eff.parameters...)*'/'*rp(eff.target))
	print(io, """
$(eff.damage), save $(eff.saving_throw_type)$(@sprintf("%+d", eff.saving_throw_bonus)) parameters $(eff.parameters[1]),$(eff.parameters[2])
Duration $(eff.duration) $(eff.timing_mode|>rp); $(eff.dispel_mode|>rp) probabilities $(eff.probabilities[1]),$(eff.probabilities[2])
""")
end
#««2 Item ability
@with_kw mutable struct ItemAbility{R<:RootResource} <: RootedResource
	root::R
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
	effects::RootedResourceVector{ItemEffect{R},R}
end
@inline function Base.getproperty(ab::ItemAbility, f::Symbol)
	f == :damage && return Dice(ab.dice_thrown, ab.dice_sides, ab.damage_bonus)
	getfield(ab, f)
end
@inline function Base.setproperty!(ab::ItemAbility, f::Symbol, x)
	if f == :damage
		@assert x isa Dice
		ab.dice_thrown = x.thrown
		ab.dice_sides = x.sides
		ab.damage_bonus = x.bonus
		return x
	end
	default_setproperty!(ab, f, x)
end
function Base.show(io::IO, mime::MIME"text/plain", ab::ItemAbility)
	h = @sprintf("%s %+d %s %s speed %d", rp(ab.attack_type), ab.thac0_bonus,
		repr(ab.damage), rp(ab.damage_type), ab.speed_factor)
	print(io, """
\e[48;5;12m $(rpad(h, 60))\e[m
Range $(ab.range), $(ab.target_count)*$(rp(ab.target_type))
Alternative $(ab.alternative_dice_thrown)d$(ab.alternative_dice_sides)
Effects $(ab.effect_index+1):$(ab.effect_index+ab.effect_count) ($(ab.effect_count) total)
""")
	for eff in ab.effects; show(io, mime, eff); end
end
#««2 Item structure
mutable struct Item <: RootResource{:itm}
	constant::Constant"ITM V1  "
	unidentified_name::Strref
	name::Strref
	replacement::Resref"ITM"
	flags::ItemFlags
	type::ItemType
	usability::UInt32 # UsabilityFlags
	animation::ItemAnimation
	min_level::UInt16
	min_strength::UInt16
	min_strengthbonus::UInt8
	kit1::UInt8
	min_intelligence::UInt8
	kit2::UInt8
	min_dexterity::UInt8
	kit3::UInt8
	min_wisdom::UInt8
	kit4::UInt8
	min_constitution::UInt8
	proficiency::WProf
	min_charisma::UInt16
	price::UInt32
	stack_amount::UInt16
	inventory_icon::Resref"BAM"
	lore::UInt16
	ground_icon::Resref"BAM"
	weight::Int32
	unidentified_description::Strref
	description::Strref
	description_icon::Resref"BAM"
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
	abilities::RootedResourceVector{ItemAbility{Item},Item}
	effects::RootedResourceVector{ItemEffect{Item},Item}
	ref::Resref"ITM"
end
# disable automatic packing of effects (for main item and abilities) —
# we do it “by hand” by concatenating with item effects
@inline Pack.fieldpack(::IO, _, ::Val{:effects},
	::AbstractVector{<:ItemEffect}) = 0
@inline Base.nameof(i::Item) = i.name
# create a virtual “not_usable_by” item property which groups together
# all the 5 usability fields in the item struct:
@inline function Base.getproperty(i::Item, name::Symbol)
	name == :not_usable_by && return UsabilityFlags(
		UInt64(i.usability) | UInt64(i.kit1) << 32 | UInt64(i.kit2) << 40 |
		UInt64(i.kit3) << 48 | UInt64(i.kit4) << 56)
	getfield(i, name)
end
@inline rr_skipproperties(::Item) = (:abilities_offset, :abilities_count,
		:effect_offset, :effect_index, :effect_count)
@inline rr_skipproperties(::ItemAbility) = (:effect_index, :effect_count)
@inline function Base.setproperty!(x::Item, name::Symbol, value)
	if name == :not_usable_by
		x.usability = UInt64(value) % UInt32
		x.kit1 = (UInt64(value) >> 32) % UInt8
		x.kit2 = (UInt64(value) >> 40) % UInt8
		x.kit3 = (UInt64(value) >> 48) % UInt8
		x.kit4 = (UInt64(value) >> 56) % UInt8
		return value
	end
	rr_setproperty!(x, name, value)
end
function Base.show(io::IO, mime::MIME"text/plain", itm::Item)
	header=@sprintf("%-50s ⚖%-3d ❍%-5d ?%-3d \n%13s:%26s/%-32s",
		nameof(itm.ref), itm.weight, itm.price, itm.lore,
		rp(itm.type), str(itm.unidentified_name), str(itm.name))
	chars=@sprintf("Str:\e[35m%2d/%2d\e[m Dex:\e[35m%2d\e[m Con:\e[35m%2d\e[m Wis:\e[35m%2d\e[m Int:\e[35m%2d\e[m Cha:\e[35m%2d\e[m Level:\e[35m% 3d\e[m",
		itm.min_strength, itm.min_strengthbonus, itm.min_dexterity,
		itm.min_constitution, itm.min_wisdom, itm.min_intelligence,
		itm.min_charisma, itm.min_level)
	nub = itm.not_usable_by
	if count_ones(nub.n) > count_zeros(nub.n)
		use = "Usable by: \e[32m"*rp(~nub)*"\e[m"
	else
		use = "Not usable by: \e[31m"*rp(nub)*"\e[m"
	end
	print(io, """
\e[7m$header\e[m
Flags: \e[36m$(rp(itm.flags))\e[m
Proficiency: \e[36m$(rp(itm.proficiency))\e[m Ench.\e[36m$(itm.enchantment)\e[m Repl.\e[36m$(itm.replacement)\e[m
$use
Requires: $chars
Inventory: \e[34m$(itm.inventory_icon.name)\e[m stack=\e[34m$(itm.stack_amount)\e[m groundicon=\e[34m$(itm.ground_icon.name)\e[m Animation: \e[34m$(itm.animation.name)\e[m Image=\e[34m$(itm.description_icon.name)\e[m
Casting effects: $(itm.effect_index):$(itm.effect_index+itm.effect_count-1)
""")
	for eff in itm.effects; show(io, mime, eff); end
	for ab in itm.abilities; show(io, mime, ab); end
end
# ««2 I/O
function read(io::ResIO"ITM")
	itm = unpack_root(io, Item)
	unpack!(seek(io, itm.abilities_offset), itm.abilities, itm.abilities_count)
	unpack!(seek(io, itm.effect_offset), itm.effects, itm.effect_count)
	for (i, ab) in pairs(itm.abilities)
		unpack!(io, ab.effects, ab.effect_count)
	end
	return itm
end
function Base.write(io::IO, itm::Item)
	itm.abilities_offset = 114
	itm.abilities_count = length(itm.abilities)
	itm.effect_offset = 114 + packed_sizeof(itm.abilities)
	itm.effect_index = 0 # XXX
	n = itm.effect_count = length(itm.effects)
	for ab in itm.abilities
		ab.effect_index = n
		ab.effect_count = length(ab.effects)
		n+= ab.effect_count
	end
	pack(io, itm)
	@assert position(io) == itm.effect_offset
	pack(io, itm.effects)
	for ab in itm.abilities
# 		@printf("\e[32mpacking ab effects at offset %d = 0x%x\e[m\n", position(io), position(io))
		pack(io, ab.effects)
	end
end
# convert user-passed arguments to item properties
# e.g. Longsword("Foobar", +1) will make enchantment +1 and name Foobar:
# XXX make this generic (i.e. name::String, enchantment::Integer etc.)
function args_to_kw(::Item, args...)
	name = nothing
	enchantment = nothing
	for a in args
		a isa AbstractString && isnothing(name) && (name = a; continue)
		a isa Integer && isnothing(enchantment) && (enchantment = a; continue)
		error("bad arguments for Item: $(args...)")
	end
	!isnothing(name) ?
		(!isnothing(enchantment) ? (;name, enchantment) : (;name)) :
		(!isnothing(enchantment) ? (;enchantment) : NamedTuple())
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
# ««1 dlg
# ««2 DLG structures
struct DLG_state
	text::Strref
	first_transition::Int32
	number_transitions::Int32
	trigger::Int32
end
@SymbolicFlags TransitionFlags::UInt32 begin
	HasText
	HasTrigger
	HasAction
	Terminates
	HasJournal
	Interrupt
	Unsolved
	JournalEntry
	Solved
end
struct DLG_transition
	flags::TransitionFlags
	text::Strref
	journal::Strref
	trigger::Int32
	action::Int32
	actor::Resref"DLG"
	state::Int32
end
# this is the same type for state trigger, transition trigger, actions:
struct DLG_string
	offset::Int32 # of trigger string
	length::Int32 # idem
end
# ««2 State, transition
struct StateKey
	# XXX: replace by hashed values
	n::UInt
	@inline StateKey(i::Integer) = new(i)
	@inline StateKey(::typeof(exit)) = new(zero(UInt))
	@inline StateKey(::typeof(!isvalid)) = new(~zero(UInt))
end
@inline Base.isvalid(s::StateKey) = (s ≠ StateKey(!isvalid))
@inline Base.isless(a::StateKey, b::StateKey) = isless(a.n, b.n)

@with_kw mutable struct Transition #{X}
	# XXX both Transition and State can be made immutable (since stored in
	# vectors)
	flags::TransitionFlags
	text::Strref = Strref(0) # graceful failing
	journal::Strref = Strref(0)
	trigger::String = ""
	action::String = ""
	actor::Resref"dlg"
	state::StateKey = StateKey(!isvalid)
end
function Base.show(io::IO, mime::MIME"text/plain", t::Transition)
	print(io, "\e[48;5;3m   ")
	contains(t.flags, Terminates) ||
		print(io, '⇒', t.actor.name, ':', t.state.n, ';')
	println(io, ' ', t.flags|>rp, "\e[m")
	contains(t.flags, HasTrigger) &&
		print(io, "\e[31;1mT: \e[m\e[31m", t.trigger, "\e[m")
	println(io, "\e[34m", t.text|>str|>f75, "\e[m")
	contains(t.flags, HasJournal) &&
		println(io, "\e[36m", t.journal|>str|>f75, "\e[m")
	contains(t.flags, HasAction) &&
		print(io, "\e[33;1mA: \e[m\e[33m", t.action, "\e[m")
end
@with_kw mutable struct State
	text::Strref
	transitions::Vector{Transition} = Auto()
	trigger::String = ""
	priority::Float32 = zero(Float32)
	age::UInt
end
function Base.show(io::IO, mime::MIME"text/plain", s::State)
	!isempty(s.trigger) && print(io, "\e[31m", s.trigger, "\e[m")
	println(io, "\e[35m", s.priority, " age=", s.age, "\e[m")
	println(io, s.text|>str|>f75)
	for t in s.transitions; show(io, mime, t); end
end
# ««2 Actor
@with_kw mutable struct Actor <: RootResource{:dlg}
	constant::Constant"DLG V1.0" = Auto() # "DLG V1.0"
	number_states::Int32 = 0
	offset_states::Int32 = 52
	number_transitions::Int32 = 0
	offset_transitions::Int32 = 0
	offset_state_triggers::Int32 = 0
	number_state_triggers::Int32 = 0
	offset_transition_triggers::Int32 = 0
	number_transition_triggers::Int32 = 0
	offset_actions::Int32 = 0
	number_actions::Int32 = 0
	flags::UInt32 = 0
	# states are keyed by hashes == UInt
	states::Dict{StateKey,State} = Auto()
	ref::Resref"dlg" # must be initialized
end
@inline rr_skipproperties(::Actor) = (:number_states, :offset_states,
	:number_transitions, :offset_transitions,
	:offset_state_triggers, :number_state_triggers,
	:offset_transition_triggers, :number_transition_triggers,
	:number_actions, :offset_actions)
@inline Pack.fieldpack(::IO, ::Type{<:Actor}, ::Val{:states}, _) = 0
@inline firstlabel(a::Actor) =
	first(Iterators.filter(i->!haskey(a.states, StateKey(i)), 0:length(a.states)))

function Base.show(io::IO, mime::MIME"text/plain", a::Actor)
	for (k, v) in sort(pairs(a.states))
		println(io, "\e[7m state $(string(k.n)): $(length(v.transitions)) transitions\e[m")
		show(io, mime, v)
	end
end

#««2 I/O
@inline dialog_strings(io::IO, offset, count)= [string0(io, s.offset, s.length)
	for s in unpack(seek(io, offset), DLG_string, count)]

function read(io::ResIO"DLG")::Actor
	actor = unpack_root(io, Actor)
	st_triggers = dialog_strings(io, actor.offset_state_triggers,
			actor.number_state_triggers)
	tr_triggers = dialog_strings(io, actor.offset_transition_triggers,
			actor.number_transition_triggers)
	actions = dialog_strings(io, actor.offset_actions, actor.number_actions)

	vs = unpack(seek(io,actor.offset_states), DLG_state, actor.number_states)
	vt = unpack(seek(io, actor.offset_transitions),
		DLG_transition, actor.number_transitions)
	@inline getval(list, i) = i+1 ∈ eachindex(list) ? list[i+1] : ""
	global S = vs
	global T = vt

	for (i, s) in pairs(vs)
		state = State(; s.text, trigger = getval(st_triggers, s.trigger),
			age = i-1)
		for j in s.first_transition+1:s.first_transition + s.number_transitions
			t = vt[j]
			push!(state.transitions, Transition(;t.flags, t.text, t.journal,
				t.actor, trigger = getval(tr_triggers, t.trigger),
				state = StateKey(t.state), action = getval(actions, t.action)))
		end
		# TODO: use hashes instead
		actor.states[StateKey(i-1)] = state
	end
	return actor
end
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
function pack_string(io::IO, pos::Ref{<:Integer}, s::AbstractString)
	l = length(s)
	pack(io, DLG_string(pos[], l))
	p = position(io)
	write(seek(io, pos[]), s)
	pos[]+= l
	seek(io, p)
end
@inline pack_string(io::IO, pos::Ref{<:Integer},
	v::AbstractVector{<:AbstractString}) =
	for s ∈ v; pack_string(io, pos, s); end
function Base.write(io::IO, a::Actor)
	vs = sizehint!(DLG_state[], a.number_states)
	vt = sizehint!(DLG_transition[],
		sum(length(s.transitions) for s ∈ values(a.states)))
	vst = UniqueVector{String}()
	vtt = UniqueVector{String}()
	va = UniqueVector{String}()
	ti = 0 # transition index
	for s ∈ sort(a.states|>values|>collect; by=s->(s.priority, s.age))
		nt = length(s.transitions)
		st = isempty(s.trigger) ? 0 : findfirst!(isequal(s.trigger), vst)
		push!(vs, DLG_state(s.text, ti, nt, st - 1))
		ti += nt
		for t in s.transitions
			tt = contains(t.flags, HasTrigger) ?
				findfirst!(isequal(t.trigger), vtt) : 1
			ta = contains(t.flags, HasAction) ?
				findfirst!(isequal(t.action), va) : 1
			push!(vt, DLG_transition(t.flags, t.text, t.journal,
				tt-1, ta-1, t.actor, t.state.n))
		end
	end
	a.offset_states = 52
	a.number_states = length(a.states)
	a.offset_transitions = a.offset_states + 16 * a.number_states
	a.number_transitions = length(vt)
	a.offset_state_triggers = a.offset_transitions + 32 * a.number_transitions
	a.number_state_triggers = length(vst)
	a.offset_transition_triggers =
		a.offset_state_triggers + 8 * a.number_state_triggers
	a.number_transition_triggers = length(vtt)
	a.offset_actions =
		a.offset_transition_triggers + 8 * a.number_transition_triggers
	a.number_actions = length(va)
	# offset for strings
	pack(io, a)
	@assert position(io) == a.offset_states
	pack(io, vs)
	@assert position(io) == a.offset_transitions
	pack(io, vt)
	@assert position(io) == a.offset_state_triggers
	ref = Ref(a.offset_actions + 8 * a.number_actions)
	pack_string(io, ref, vst)
	@assert position(io) == a.offset_transition_triggers
	pack_string(io, ref, vtt)
	@assert position(io) == a.offset_actions
	pack_string(io, ref, va)
	@assert position(io) == a.offset_actions + 8 * a.number_actions
end
#««2 Dialog-building functions
@with_kw mutable struct DialogContext
	current_actor::Actor = Actor(;ref = Resref".dlg")
	current_state_key::StateKey = StateKey(!isvalid)
	current_transition_idx::Int = 0 # index into current_state.transitions
	pending_transition::Bool = false
	trigger::Union{Nothing,String} = nothing
end
@inline current_actor(c::DialogContext) = c.current_actor
@inline current_state(c::DialogContext) =
	(@assert isvalid(c.current_state_key);
	current_actor(c).states[c.current_state_key])
@inline if_current_state(f::Function, c::DialogContext) =
	isvalid(c.current_state_key) && f(current_state(c))
@inline current_transition(c::DialogContext) =
	current_state(c).transitions[c.current_transition_idx]
trigger!(c::DialogContext, s::AbstractString) =
	(@assert isnothing(c.trigger); c.trigger = s)
trigger!(c::DialogContext, ::Nothing) = nothing
"gets a trigger value, from either supplied x, or current trigger"
@inline function get_trigger(c::DialogContext, x)
	trigger!(c, x) # first update current trigger (no-op if x == `nothing`)
	y = c.trigger  # then get trigger value
	c.trigger = nothing # purge the buffer
	return y
end
State(c::DialogContext; trigger = nothing, kwargs...) =
	State(; trigger = get_trigger(c, trigger), kwargs...)
function set_actor!(c::DialogContext, actor::Actor)
	c.current_state_key = StateKey(!isvalid)
	c.current_actor = actor # returns actor
end
function set_state!(c::DialogContext, label)
	key = StateKey(label)
	@assert haskey(current_actor(c).states, key)
	c.current_state_key = key
end
function add_state!(c::DialogContext, label, text::Strref; kwargs...)
	# XXX implicit transition from previous state
	key = StateKey(label)
	@assert !haskey(current_actor(c).states, key) "state $label already exists"
	println("\e[1mINSERT STATE $key\e[m")
	if_current_state(c) do s # insert implicit transition
		isempty(s.transitions) && add_transition!(c, label)
	end
	if c.pending_transition
		println("  \e[32m Resolve pending transition to $key\e[m")
		current_transition(c).state = key
		current_transition(c).actor = a.ref
		c.pending_transition = false
	end
	dict = current_actor(c).states
	register!(current_actor(c))
	s = State(; age=length(dict), text, kwargs...)
	c.current_state_key = key
	dict[key] = s
	return s
end
"""Forms for `add_transition`:

    add_transition!(ctx, actor, state; text, journal)
    add_transition!(ctx, (actor, state); text, journal)
    add_transition!(ctx, state; text, journal)
    add_transition!(ctx, exit; text, journal)

The first form is the main one: all others eventually call it.
This is where structure properties are maintained.
"""
function add_transition!(c::DialogContext, actor::Resref"dlg", state;
		position = nothing, terminates, text = nothing, journal = nothing,
		trigger = nothing, action = nothing)
	key = StateKey(state)
	@assert !(c.pending_transition) "unsolved pending transition"
	# XXX some flags are still missing
	flags = TransitionFlags(0)
	trigger = get_trigger(c, trigger)
	terminates && (flags |= Terminates)
	isnothing(text) ? (text = Strref(-1)) : (flags |= HasText)
	isnothing(journal) ? (journal = Strref(0)) : (flags |= HasJournal)
	isnothing(trigger) ? (trigger = "") : (flags |= HasTrigger)
	isnothing(action) ? (action = "") : (flags |= HasAction)
	t = Transition(;actor = c.current_actor.ref, state = key,
		text, journal, flags)
	position = something(position, 1 + length(current_state(c).transitions))
	register!(current_actor(c))
	c.current_transition_idx = position
	insert!(current_state(c).transitions, position, t)
	return t
end
@inline add_transition!(c::DialogContext,
		(actor, state)::Tuple{<:Resref"dlg",<:Any}; kwargs...) =
	add_transition!(c, actor, state; terminates=false, kwargs...)
@inline add_transition!(c::DialogContext, state; kwargs...) =
	add_transition!(c, (current_actor(c).ref, state); kwargs...)
@inline add_transition!(c::DialogContext, ::typeof(exit); kwargs...) =
	add_transition!(c, Resref".dlg", 0; terminates=true, kwargs...)

function add_transition!(c::DialogContext, ::Nothing; kwargs...)
	println("  \e[31madd pending transition\e[m")
	add_transition!(c, StateKey(!isvalid); kwargs...)
	c.pending_transition = true
end
function add_action!(t::Transition, s::AbstractString; override=false)
	@assert(override || !contains(t.flags, HasAction))
	t.flags |= HasAction
	t.action = s
end
function add_journal!(t::Transition, s::AbstractString; override=false)
	@assert(override || !contains(t.flags, HasJournal))
	t.flags |= HasJournal
	t.journal = s
end

#««1 Game
#««2 Game data structure
"""    Game

Main structure holding all top-level data for a game installation, including:
 - key/bif archived files,
 - table of override files,
 - tlk strings (TODO).

Methods include:

 - `game[resref]`: returns the data structure described by this resource.
 - `get(game, resref) do ... end`
 - `names(game, Resref"type")`: returns a vector of all names of
   existing resources of this type.
"""
@with_kw struct Game
	directory::Base.RefValue{String} = Ref("")
	key::KeyIndex = KeyIndex()
	override::Dict{Symbol, Set{String}} = Auto()
	# Current values (mutable data):
	language::Base.RefValue{Int} = Ref(0)
	namespace::Base.RefValue{String} = Ref("")
	# XXX: each tlk file is quite heavy (5 Mbytes in a fresh BG1
	# install), we could use a rotation system to not keep more than 4 or 5
	# in memory at the same time
	# (this is already likely during resource building since most mods are
	# written in 3 languages, but should be ensured during final
	# compilation)
	strings::Vector{TlkStrings} = [ TlkStrings() for _ in LANGUAGE_FILES ]
	# we collect all new strings (for any language) in the same structure,
	# so that all the strref numbers advance in sync:
	new_strings::UniqueVector{Tuple{Int8,String}} = Auto()
	# longrefs ↔ shortrefs bijection:
	shortref::Dict{String,StaticString{8}} = Auto()
	longref::Dict{StaticString{8},String} = Auto()

	modified_items::Dict{Resref"ITM",Item} = Auto()
	modified_actors::Dict{Resref"dlg",Actor} = Auto()
	dialog_context::DialogContext = Auto()
end
function Base.show(io::IO, g::Game)
	print(io, "<Game: ", length(g.key), " keys, ", length(g.override),
		" overrides, ", count(!isempty, g.strings), " languages>")
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
@inline Game(directory::AbstractString) = init!(Game(), directory)
@inline longref_file(g::Game) = joinpath(game.directory[], "longrefs")
function init!(g::Game, directory::AbstractString)
	g.namespace[] = ""
	g.directory[] = directory
	init!(g.key, joinpath(directory, "chitin.key"))
	println("read ", length(g.key), " key resources")
	empty!(g.override)
	o_dir = joinpath(directory, "override")
	if !isdir(o_dir)
		println("created override directory: ", o_dir)
		mkdir(o_dir)
	else
		for f in readdir(o_dir)
			(n, e) = f|>basename|>lowercase|>splitext; t = Symbol(e[2:end])
			push!(get!(g.override, t, Set{String}()), n)
		end
		println("read $(sum(length(v) for (_,v) in g.override; init=0)) override resources")
	end
	set_language!(g, 1) # default language
	isfile(longref_file(g)) && for line in eachline(longref_file(g))
		# XXX use ; for comments? this is not an allowed file name
		(short, long) = split(line, r"\s", limit=2)
		g.shortref[long] = short
		g.longref[short] = long
	end
	g.namespace[] = "user"
	return g
end
#««2 Resref ←—→ shortrefs
# This layer does all of the work of abstracting Resref (i.e. long
# resource identifiers, possibly including namespace information) to
# short references used as keys by the game.
# There are *three* such conversion methods:
# - longref(game, string): converts (unpacks) a file-read string to a Resref;
# - shortref(game, object): creates (if needed) the shortref from obj's ref;
# - shortref(game, string): packs a Resref to an (existing) shortref.
# Shortref —→ longref when reading a file
"""    longref(game, shortref)

Converts a short reference (i.e. an ≤8 byte string) to a long reference.
 - All “new” shortrefs should be reference in the game's `"longrefs"` file,
   so shortrefs absent from this file belong to "":
 - (the other possible distinction, game key, is less useful, because
   some non-original game files might eventually be biffed).
"""
function longref(g::Game, short::AbstractString)
	# all “new” shortrefs should be referenced in the longrefs file,
	# so shortrefs absent from here belong to "":
	get(g.longref, short, String(short))
end
# Resref —→ shortref when writing a file
"""    shortref(game, resource)

Convert a long reference (already stored in a game object) to a short reference:
 - if the namespace is "" then this is already a short reference;
 - if the (namespace, resource) pair is already indexed, return this;
 - otherwise, create a new index entry for this pair.
"""
function shortref(g::Game, r::Resref)
	str = r.name
	!contains(str, '/') && return StaticString{8}(str|>lowercase)
	get!(g.shortref, str) do
		for s in ShortrefIterator{8}(first(replace(str, r".*/"=>""), 8))
			!haskey(g.longref, s) && !haskey(g.key, s, resourcetype(r)) &&
				(g.longref[s] = str; return s)
		end
	end
end
"""    ShortrefIterator(s)

Iterator producing, in order:  "foobar", "foobar00".."foobar99",
"fooba000".."fooba999" etc.
"""
struct ShortrefIterator{L}
	name::StaticString{L}
	# XXX: allow other bases (e.g. base 16)
end
@inline textlength(::ShortrefIterator{L}) where{L} = L
function Base.iterate(r::ShortrefIterator, i = 0)
	iszero(i) && return (r.name, 10)
	k = ndigits(i)-1; q = 10^k
	(i÷q) > 1 && (i = q*= 10; k+= 1)
	return (eltype(r)(first(r.name, textlength(r)-k)*
		(digits(i, pad=k)[1:k]|>reverse|>join)), i+1)
end
Base.length(r::ShortrefIterator) = 10^textlength(r)+1
Base.eltype(::ShortrefIterator{L}) where{L} = StaticString{L}
"""    shortref(game, name::AbstractString)

Returns the short ref for the given obj ref. The short ref must exist."""
function shortref(g::Game, str::AbstractString)
	!contains(str, '/') && return StaticString{8}(str)
	return g.shortref[str]
end
#««2 Pseudo-dictionary interface
# All the functions used here encapsulate access to the `Game` structure
# as a pseudo-dictionary indexed by `Resref` keys.
# Resources can be stored in three places:
#  - modified resource registry,
#  - override directory,
#  - key/bif archives.

# default case for modified_objs
@inline modified_objs(g::Game, T::Type{<:Resref}) = Nothing
# 	error("no modified objects dictionary found for type $(resourcetype(T))")

# By examing the field types of Game structure, determine the correct
# registry field for each resource type: Item => modified_items, etc.
for (i,T) in pairs(fieldtypes(Game))
	(T <: Dict && keytype(T) <: Resref) || continue
	R = valtype(T); R <: RootResource || continue
	@eval @inline modified_objs(g::Game, ::Type{$(keytype(T))}) = getfield(g, $i)
end

@inline register!(g::Game, x::RootResource) =
begin
	println("\e[34;1m register!($(x.ref))\e[m")
	(modified_objs(g, typeof(x.ref))[x.ref]=x)
end

Base.haskey(g::Game, ref::Resref) = haskey(g, nameof(ref), resourcetype(ref))
function Base.haskey(g::Game, str::AbstractString, type::Symbol)
	s = shortref(g, str)
	haskey2(modified_objs(g, Resref{type}), str) ||
	haskey(game.key, s, type) ||
	(haskey(game.override, type) && haskey(game.override[type], s))
end
@inline haskey2(::Nothing, _) = false
@inline haskey2(d::Dict, k) = haskey(d, k)

function Base.open(g::Game, res::Resref, def::Base.Callable; override = true)
	name, T = shortref(g, res.name), resourcetype(res)
	if override && name|>lowercase ∈ get(g.override, T, ())
		file = joinpath(g.directory[], "override", name*'.'*String(T))
		return ResIO{T}(res, open(file))
	end
	buf = get(g.key, name, T)
	isnothing(buf) && return def()
	ResIO{T}(res, buf)
end
Base.get(f::Base.Callable, g::Game, ref::Resref) =
	getresource(f, g, modified_objs(g, typeof(ref)), ref)
getresource(f::Base.Callable, g::Game, ::Nothing, ref::Resref) =
	open(read, g, ref, f)
getresource(f::Base.Callable, g::Game, dict::AbstractDict, ref::Resref) =
	get(dict, ref) do; open(read, g, ref, f); end
Base.get!(f::Base.Callable, g::Game, ref::Resref) = get(register! ∘ f, g, ref)
Base.get!(g::Game, ref::Resref, x) = get(()->x, g, ref)

@inline Base.getindex(g::Game, ref::Resref) =
	get(g, ref) do; throw(KeyError(ref)); end

@inline Base.names(g::Game, ::Type{<:Resref{T}}) where{T} =
	(longref(g, s) for s in Iterators.flatten(
		(get(g.override,T, Set{String}()), names(g.key, Resref{T}))))

@inline Base.names(g::Game, T::Type{<:Resref}, pat::Union{String,Regex}) =
	(x for x in names(g,T) if contains(x, pat))

"""    Resref{T}(game, string)

Converts a string (entered by the user) to a long reference:
 - if the string is of the form `"/resource"` then use root namespace;
 - if the string is of the form `"namespace/resource"`, parse it;
 - if an object with this reference exists in current namespace, return it;
 - same with root namespace;
 - otherwise, return `(current_namespace, string)` (this is a forward decl.).
"""
function (R::Type{<:Resref})(g::Game, str::AbstractString)
	startswith(str, '/') && return R(str[2:end])
	contains(str, '/') && return R(str)
	ns_str = resref_join(namespace(g), str)
	r = get(g.longref, ns_str, nothing)
	!isnothing(r) && return R(ns_str)
	length(str) ≤ 8 && haskey(g, str, resourcetype(R)) && return R(str)
	return R(ns_str)
end
# #««2 NamedResource
# """    NamedResource{T,R,K}
# 
#  - `T` is the current type of the resource (this changes when taking fields)
#  - `R` is the type of the root resource (i.e. item etc.)
#  - `K` is the key type (i.e. NTuple{2,String} in our case)
#  - `root`: root resource (this is unchanged by taking fields)
#  - `offset`: current pointer offset relative to stored root resource
#  - `key`: global identifier for the resource.
# 
# API:
#   - `getproperty`: also produces a `NamedResource` where useful (i.e.
#     for struct-type fields), otherwise the plain property value.
#   - `setproperty!`: resource.field = value
# 	 * registers `resource` in the modified dict
# 	  [i.e. we need the root resource + its key]
# 	 * computes a pointer to the relevant field of the stored resource
# 	  [i.e. we need current type and offset]
# 	 * stores the value at the pointer location and returns it
#  - likewise array-reading: `getindex`, `iterate` and -modifying operations:
#    `setindex!`, `push!`, `insert!`, `delete!`
#    XXX: deleteat!, pushfirst!, pop!, popfirst!
# 
# XXX possibly cleaner alternative (i.e. no pointers!; use real struct fields,
#   allowing for repl completion):
# append a `root` field in all resources pointing to the root resource
# (including to self for the root resource),
# and append to that resource the `modified` bit and resource ID.
# However, this needs circular types, and everything needs to be mutable
# (and still overwriting all the `setproperty!` etc. methods
# to register the modified parent; also vector-modifying methods, etc.
# Read-only methods are not affected though).
# 
# This would also (reasonably) need to make all resource data types inherit a common types, for which the setproperty! method would be rewritten.
# """
# struct NamedResource{T,R,K}
# 	data::T
# 	root::R
# 	key::K
# 	offset::Int
# end
# MaybeNamed{T} = Union{T,NamedResource{T}}
# 
# function Base.show(io::IO, mime::MIME"text/plain", r::NamedResource)
# 	print(io, "With context ", _key(r), "\n")
# 	show(io, mime, _data(r))
# end
# 
# NamedResource(root::X, key::K) where{K,X} =
# 	NamedResource{X,X,K}(root, root, key, 0)
# @inline _data(r::NamedResource) = getfield(r, :data)
# @inline _key(r::NamedResource) = getfield(r, :key)
# @inline _root(r::NamedResource) = getfield(r, :root)
# @inline _offset(r::NamedResource) = getfield(r, :offset)
# 
# @inline isnamedresource(T::DataType) = isstructtype(T)
# @inline isnamedresource(T::Type{<:DottedEnums.EnumOrFlags}) = false
# 
# function Base.getproperty(r::NamedResource{T,R,K},
# 		f::Symbol) where{T,R,K}
# # 	f ∈ fieldnames(NamedResource) && return getfield(r, f)
# # 	!hasfield(T, f) && return getproperty(r.data, f)
#  	i = findfirst(isequal(f), fieldnames(T))
# # 	println("field index for $f is $i")
# 	Tf, Of, Df = fieldtype(T, i), fieldoffset(T, i), getproperty(_data(r), f)
# # 	println("   field type, offset, data are $Tf, $Of, $Df")
# # 	println("   struct? $(isstructtype(Tf))")
# 	isnamedresource(Tf) || return Df
# 	return NamedResource{Tf, R, K}(Df, _root(r), _key(r),
# 		_offset(r) + Of)
# end
# function Base.setproperty!(r::NamedResource{T}, f::Symbol, x) where{T}
#  	i = findfirst(isequal(f), fieldnames(T))
# 	Tf, Of = fieldtype(T, i), fieldoffset(T, i)
# 	p = Base.unsafe_convert(Ptr{Nothing}, register!(_key(r), _root(r)))
# 	x1 = convert(Tf, x)
# 	unsafe_store!(convert(Ptr{Tf}, p + _offset(r) + Of), x1)
# 	return x1
# end
# # Vector data: getindex, setindex!, push!, insert!
# function Base.getindex(r::NamedResource{T,R,K},
# 		i::Int) where{T<:Vector,R,K}
# 	Tf = eltype(T); Of, Df = (i-1)*sizeof(Tf), _data(r)[i]
# 	isnamedresource(Tf) || return Df
# 	return NamedResource{Tf, R, K}(Df, _root(r), _key(r),
# 		_offset(r) + Of)
# end
# @inline Base.iterate(r::NamedResource{<:Vector}, i=1) =
# 	(i-1 < length(_data(r)) ? (@inbounds r[i], i+1) : nothing)
# @inline Base.length(r::NamedResource{<:Vector}) = length(_data(r))
# @inline Base.eltype(r::NamedResource{T,R,K}) where{T<:Vector,R,K}=
# 	let Tf = eltype(T)
# 	isnamedresource(Tf) ? NamedResource{Tf,R,K} : Tf
# end
# 
# for f in (:setindex!, :push!, :insert!, :delete!)
# 	@eval Base.$f(r::NamedResource{<:Vector}, x...) =
# 		(register!(_key(r), _root(r)); $f(_data(r), x...))
# end
# """    refdict(dict, key)
# 
# Returns a reference to `dict[key]`."""
# function refdict(dict::Dict, key)
# 	i = Base.ht_keyindex(dict, key)
# 	@assert i > 0
# 	return Base.RefArray(dict.vals, i)
# end
# """    refdict!(dict, key, value)
# 
# Returns a reference to `dict[key]`, initializing it to `value` if absent."""
# @inline refdict!(dict::Dict, key, value) =
# 	# if slot exists, return slot index
# 	# otherwise, create slot and return slot index
# 	(get!(dict, key, value); refdict(dict, key))
# 
# ««2 Namespace and language
"""    namespace(game, s)

Sets the current namespace for game resources being defined to `s`.
The following default namespaces are used:
 - `""` for original game resources;
 - `"user"` is the default namespace for added resources.
"""
@inline namespace(g::Game, s::AbstractString) = (g.namespace[] = s; s)
@inline namespace(g::Game) = g.namespace[]

function set_language!(g::Game, i::Integer)
	g.language[] = i
	isempty(g.strings[i]) || return
	g.strings[i] =
		read(joinpath(g.directory[], "lang", LANGUAGE_FILES[i][2]), TlkStrings)
end
"""    language(game, s)

Sets the current language of the game (i.e. the language in which
strings entered as parameters for functions will be interpreted)
to the one given by string `s`.

Allowed values are:
$(join([replace(replace(repr(x[1]), r"(^r\"\^|\"i$|\.\*)"=>""), "*" => "\\*") for x in LANGUAGE_FILES], ", ")).
"""
function language(g::Game, s::AbstractString)
	for (i, (r, f)) in pairs(LANGUAGE_FILES)
		contains(s, r) || continue
		set_language!(g, i)
		return i
	end
	error("unknown language: "*s)
end
@inline language(g::Game) = g.language[]
# ««2 Strings
@inline strings(g::Game, i = language(g)) = g.strings[i]
function Strref(g::Game, s::AbstractString)
	i = get(strings(g), s, nothing)
	isnothing(i) || return i
	i = findfirst!(isequal((language(g), s),), g.new_strings)
	return Strref(i-1 + length(strings(g)))
end

function str(g::Game, s::Strref)
	i = s.index+1; i = max(i, one(i))
	i ≤ length(strings(g)) && return strings(g).entries[i].string
	return g.new_strings[i - length(strings(g))][2]
end

"""    search(game, strings, ResIO"type", text)

Searches for the given text (string or regular expression)
in the names of all resources of the given `type`.
`strings` is the string database used for translation.
"""
function search(g::Game, str::TlkStrings, R::Type{<:ResIO}, text)
	for res in all(g, R)
		s = res|>read|>nameof
		contains(s, text) || continue
		@printf("%c%-8s %s\n", res isa ResIOFile ? '*' : ' ', nameof(res), s)
	end
end
#««2 Accessors: `item` etc.
@inline item(g::Game, name::AbstractString) = g[Resref"itm"(g, name)]
@inline items(g::Game, r::Union{String,Regex}...) =
	(item(g, n) for n in names(g, Resref"itm", r...))

#««2 Dialog-building functions
get_actor(g::Game, s::AbstractString) = get_actor(g, Resref"dlg"(s))
get_actor(g::Game, ref::Resref"dlg") = get!(g, ref, Actor(;ref))

"""
    actor("name")

Loads the named actor from game files, or creates an empty actor if
none exists with this name.
"""
actor(g::Game, s::AbstractString) = set_actor!(g.dialog_context, get_actor(g,s))

"""
    say({text | label => text}*; priority, trigger)

Introduces states of dialog for the current actor.
A single `say` call is equivalent to several successive `say` calls
for the same current actor.
Implicit transitions will be inserted between those states.
"""
say(g::Game, text::AbstractString; kw...) =
	say2(g, firstlabel(g.dialog_context|>current_actor), text; kw...)
say(g::Game, pair::Pair{<:Any,<:AbstractString}; kw...) =
	say2(g, pair[1], pair[2]; kw...)
say(g::Game, args...; kw...) = for a in args; say(g, a; kw...); end

say2(g::Game, label, text::AbstractString; kw...) =
	add_state!(g.dialog_context, label, Strref(g, text); kw...)
state(g::Game, label) = set_state!(g.dialog_context, label)
"""
    reply(text => label)

Introduces a state transition (player reply) pointing to the given label.
The label may be one of:
 - ("actor", state)
 - state  (uses current actor)
 - `exit` (creates a final transition)
"""
reply(g::Game, (text, label)::Pair{<:AbstractString}; kw...) =
	add_transition!(g.dialog_context, label; text, kw...)
"""
    trigger(string)

Attaches a trigger to the **next** transition or state."""
@inline trigger(g::Game, s::AbstractString) = trigger!(g.dialog_context, s)
"""
    action(string)

Attaches an action to the latest transition."""
@inline action(g::Game, s::AbstractString; kw...) =
	add_action!(current_transition(g.dialog_context), s; kw...)
"""
    journal(string)

Attaches a journal entry to the latest transition."""
@inline journal(g::Game, s::AbstractString; kw...) =
	add_journal!(current_transition(g.dialog_context), s; kw...)

#««2 Saving game data
function save(g::Game)
	# Modified items
	# XXX: iterate over dict fields, etc.
	println("\e[1mWriting $(length(game.modified_items)) modified items\e[m")
	for itm in values(game.modified_items)
		save(game, itm)
	end
	open(longref_file(g), "w") do io
		for (k, v) in pairs(g.longref)
			println(io, k, '\t', v)
		end
	end
end
function save(g::Game, x::RootResource)
	ref = x.ref
	T, s = resourcetype(ref), shortref(g, ref)
	println("  \e[32m$ref => $s.$T\e[m")
	write(joinpath(game.directory[], "override", uppercase(s)*'.'*string(T)), x)
	push!(get!(g.override, T, Set{String}()), s)
end

#««1 Item/etc. factory
args_to_kw(x, args...) =
	error("no conversion from args to properties for type $(typeof(x))")
	# return a NamedTuple
"""    getkey(key, dict1, dict2, ..., default)

Chained version of `get`."""
@inline getkey(k, x1, default) = get(x1, k, default)
@inline getkey(k, x1, y, z...) = get(x1, k, getkey(k, y, z...))
@inline getkey(f::Function, k, x1) = get(f, x1, k)
@inline getkey(f::Function, k, x1, y...) = get(x1, k, getkey(f, k, y...))

function Base.copy(g::Game, x::T, args...; kwargs...) where{T<:RootResource}
	kw2 = args_to_kw(x, args...)::NamedTuple
	println("got kw2=$kw2")
	vars = (getkey(fn, kwargs, kw2, getfield(x, fn)) for fn in fieldnames(T))
	y = T(vars...)
	# this triggers a register! call:
	y.ref = getkey(:ref, kwargs, kw2) do
		r = namespace(g)*'/'* replace(str(g, y.name), r"[^a-zA-Z0-9]" => "")
		i = 1
		while haskey(g.shortref, r) ||
				haskey(modified_objs(g, typeof(x.ref)), oftype(x.ref, r))
			r = replace(r, r"~\d*$" => "")*'~'*string(i)
			i+= 1
		end
		r
	end
	return y
end
@inline Base.copy(g::Game, r::Resref, args...; kwargs...) =
	copy(g, read(g, r), args...; kwargs...)

const _resource_template = Dict{DottedEnums.SymbolicNames,Resref}(
	Amulet => Resref"amul02.itm",
	Belt => Resref"belt01.itm",
	Boots => Resref"boot06.itm",
	Arrow => Resref"arow01.itm",
	Bracers => Resref"brac05.itm",
	Ring => Resref"ring01.itm",
	Scroll => Resref"scrl02.itm",
# 	Shield => Resref"shld01.itm",
	Bullet => Resref"bull01.itm",
# 	Bow => Resref"bow05.itm",
	Dagger => Resref"dagg01.itm",
	Mace => Resref"blun04.itm",
	Sling => Resref"slng01.itm",
# 	SmallSword => Resref"sw1h07.itm",
# 	LargeSword => Resref"sw1h04.itm",
	Hammer => Resref"hamm01.itm",
	Morningstar => Resref"blun06.itm",
	Flail => Resref"blun02.itm",
	Dart => Resref"dart01.itm",
	Axe => Resref"ax1h01.itm",
	Quarterstaff => Resref"staf01.itm",
	Crossbow => Resref"xbow04.itm",
	Spear => Resref"sper01.itm",
	Halberd => Resref"halb01.itm",
	Bolt => Resref"bolt01.itm",
# 	Cloak => Resref" # no trivial cloak
	Gem => Resref"misc20.itm", # bloodstone
	Buckler => Resref"shld08.itm",
# 	Club => Resref"blun01.itm", # IWD
# 	LargeShield => Resref"shld15.itm", # IWD
# 	MediumShield => Resref"shld13.itm", # IWD
# 	SmallShield => Resref"shld11.itm", # IWD
# 	GreatSword => Resref"sw2h01.itm", # IWD
	LeatherArmor => Resref"leat01.itm",
	StuddedLeatherArmor => Resref"leat04.itm",
	ChainMail => Resref"chan01.itm",
	SplintMail => Resref"chan04.itm",
	HalfPlate => Resref"plat01.itm",
	FullPlate => Resref"plat04.itm",
	HideArmor => Resref"leat10.itm",
# 	Robe => Resref" # no trivial Robe
	BastardSword => Resref"sw1h01.itm",
	LongSword => Resref"sw1h04.itm",
	ShortSword => Resref"sw1h07.itm",
	TwoHandedSword => Resref"sw2h01.itm",
	Katana => Resref"sw1h43.itm",
	Scimitar => Resref"sw1h56.itm",
	LongBow => Resref"bow03.itm",
	ShortBow => Resref"bow05.itm",
)
const Longbow = LongBow
const Shortbow = ShortBow
const Longsword = LongSword
const PlateMail = HalfPlate

(T::DottedEnums.SymbolicNames)(args...; kwargs...) =
	copy(_resource_template[T], args...; kwargs...)
#««1 Global `game` object

const game = Game()

# For all methods of those functions starting with a `::Game` argument:
# define a corresponding method where the global `game` is used.
for f in (init!, str, shortref, longref, register!, save,
		language, namespace,
		item, items, actor, say, reply, action, trigger), m in methods(f)
	argt = m.sig.parameters
	(length(argt) ≥ 2 && argt[2] == Game) || continue
	argn = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), m.slot_syms)
	z = zip(argn[3:end], argt[3:end])
	lhs = (:($n::$t) for (n,t) in z)
	rhs = (n for (n,t) in z)
	eval(:($(nameof(f))($(lhs...)) = $f(game, $(rhs...))))
end
@inline Base.convert(T::Type{Strref}, s::AbstractString) = T(game, s)
@inline Base.convert(T::Type{Strref}, ::Nothing) = T(0) # <NO TEXT>
@inline Base.convert(T::Type{<:Resref}, s::AbstractString) = T(game, s)
@inline Base.convert(T::Type{<:Resref}, x::RootResource) = x.ref
@inline Base.copy(x::Union{Resref,RootResource}, args...; kwargs...) =
	copy(game, x, args...; kwargs...)

# debug
function modified_objs()
	println(length(game.modified_items), " modified items:")
	for (k,v) in pairs(game.modified_items)
		println("  $k\t$(v.ref)")
	end
end
function blob(x)
	f = TransitionFlags(0)
	iszero(x) || (f|= HasTrigger)
	f
end
function extract(r::Resref, fn::AbstractString = nameof(r)*'.'*string(resourcetype(r)))
	buf = get(game.key, nameof(r), resourcetype(r))
	write(fn, buf)
end
#»»1

end # module
