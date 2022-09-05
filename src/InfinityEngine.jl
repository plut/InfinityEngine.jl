"""    InfinityEngine

An interface for Infinity Engine games' databases.

Currently available: item editor and (basic) dialog editor.
"""
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
Dice(a, b) = Dice(promote(a,b)...)
Dice(a::I, b::I, c = zero(I)) where{I<:Integer} = Dice{I}(a,b,c)
function Base.show(io::IO, d::Dice)
	!isone(d.thrown) && print(io, d.thrown)
	print(io, 'd', d.sides)
	d.bonus > 0 && print(io, '+', d.bonus)
	d.bonus < 0 && print(io, d.bonus)
end
Base.:*(a::Integer, d::Dice{I}) where{I} =
	(@assert isone(d.thrown) && iszero(d.bonus); Dice{I}(I(a), d.sides, I|>zero))
Base.:+(d::Dice{I}, b::Integer) where{I} =
	Dice{I}(d.thrown, d.sides, d.bonus + I(b))
Base.:-(d::Dice{I}, b::Integer) where{I} =
	Dice{I}(d.thrown, d.sides, d.bonus - I(b))
Base.iszero(d::Dice) =
	(iszero(d.thrown) || iszero(d.sides)) && iszero(d.bonus)
doubleaverage(d::Dice) = d.thrown * (d.sides+one(d.sides)) + d.bonus<<1
average(d::Dice) = doubleaverage(d)//2

using Random
Base.rand(rng::AbstractRNG, d::Random.SamplerTrivial{<:Dice}) =
	sum(rand(rng, Base.OneTo(d[].sides)) for _ in Base.OneTo(d[].thrown)) + d[].bonus

for i in (2,3,4,5,6,8,10,12,20)
	s = Symbol("d$i"); @eval ($s = Dice{Int}(1,$i,0); export $s)
end
export Dice
end

using .DiceThrows
include("pack.jl"); using .Pack
include("symbolicenums.jl"); using .SymbolicEnums
include("opcodes.jl"); using .Opcodes
include("markedstrings.jl"); using .MarkedStrings

using Printf
using StaticArrays
using Parameters
using UniqueVectors
using Random
using TOML

# ««1 Basic types
# ««2 Misc.
struct EmptyDict; end
Base.get(::EmptyDict, _, x) = x
Base.isempty(::EmptyDict) = true
Base.iterate(::EmptyDict) = nothing
Base.copy!(d::AbstractDict, ::EmptyDict) = empty!(d)
struct Auto{A<:Tuple, B}
	args::A
	kwargs::B
	Auto(args...; kwargs...) = new{typeof(args),typeof(kwargs)}(args, kwargs)
end
Base.convert(T::DataType, auto::Auto) = T(auto.args...; auto.kwargs...)
# Compact representation, useful for Flags and Enums
rp(x) = repr(x;context=(:compact=>true))
f75(s::AbstractString) = length(s) ≤ 72 ? s : first(s,72)*'…'
function if_haskey(f::Base.Callable, dict::Dict, key)
	index = Base.ht_keyindex(dict, key)
	index > 0 && f(@inbounds dict.vals[index])
end
#««2 Read call context

const SELF_DIR = dirname(String(@__FILE__))
# This returns the first stacktrace falling outside this module's scope
function call_frame()
	for frame in stacktrace()
		dir = dirname(String(frame.file))
		!startswith(dir, SELF_DIR) && return frame
	end
end
call_directory() = dirname(String(call_frame().file))

# ««2 Extract zero-terminated string from IO
@inline string0(v::AbstractVector{UInt8}) =
	isempty(v) ? "" : String(iszero(last(v)) ? view(v, 1:length(v)-1) : v)
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))
@inline function substring0(s::AbstractString, a::Integer, n::Integer)
	iszero(n) && return NO_SUBSTRING
	p = prevind(s,a+n+1)
	view(s, a+1:(iszero(codeunit(s, p)) ? prevind(a, p) : p))
end
	
@inline string0(s::AbstractString) = let n = findfirst('\0', s)
	isnothing(n) && (n = length(s)+1)
	view(s, 1:n-1)
end

# ««2 Static length strings
struct StaticString{N} <: AbstractString
	chars::SVector{N,UInt8}
	# once we encounter a zero character, all that follows is zero:
	@inline StaticString{N}(chars::AbstractVector{UInt8}) where{N} =
		(@assert length(chars) ≤ N; c = Ref(0x1); new{N}(SVector{N}(
		iszero(c[]) ? c[] : (c[] = get(chars, i, 0x0)) for i in 1:N)))
end
Base.sizeof(::StaticString{N}) where{N} = N
@inline Base.ncodeunits(s::StaticString) =
	let l = findfirst(iszero, s.chars); isnothing(l) ? sizeof(s) : l-1; end
@inline function Base.:(==)(x::StaticString{N}, y::StaticString{N}) where{N}
	for i in 1:N
		@inbounds x.chars[i] == y.chars[i] || return false
		@inbounds iszero(x.chars[i]) && break
	end
	return true
end

Base.codeunit(s::StaticString, i::Integer) = s.chars[i]
Base.codeunit(::StaticString) = UInt8
# We handle only ASCII strings...
Base.isvalid(::StaticString, ::Integer) = true
function Base.iterate(s::StaticString, i::Integer = 1)
	i > length(s) && return nothing
	c = @inbounds s.chars[i]
	iszero(c) && return nothing
	(Char(c), i+1)
end
@inline function StaticString{N}(s::AbstractString) where{N}
	@assert length(s) ≤ N "string must be at most $N characters long: \"$s\""
	return StaticString{N}(s|>codeunits)
end
StaticString{N}(s::StaticString{N}) where{N} = s

@inline Pack.unpack(io::IO, T::Type{StaticString{N}}) where{N} =
	T(SVector{N,UInt8}(read(io, UInt8) for _ in SOneTo(N)))
Pack.pack(io::IO, s::StaticString) = write(io, s.chars)

charuc(x::UInt8) = (0x61 ≤ x ≤ 0x7a) ? x-0x20 : x
charlc(x::UInt8) = (0x41 ≤ x ≤ 0x5a) ? x+0x20 : x
Base.uppercase(s::StaticString) = typeof(s)(charuc.(s.chars))
Base.lowercase(s::StaticString) = typeof(s)(charlc.(s.chars))
@inline function Base.hash(s::StaticString{8})
	# this operation is time-critical; we completely unroll the loop
	n1 = @inbounds Int64(s.chars[1])
	n2 = @inbounds Int64(s.chars[2])
	n3 = @inbounds Int64(s.chars[3])
	n4 = @inbounds Int64(s.chars[4])
	n5 = @inbounds Int64(s.chars[5])
	n6 = @inbounds Int64(s.chars[6])
	n7 = @inbounds Int64(s.chars[7])
	n8 = @inbounds Int64(s.chars[8])
	hash(n1 | (n2<<8) | (n3 << 16) | (n4 << 24) | (n5 << 32) |
		(n6 << 40) | (n7 << 48) | (n8 << 56))
end

#««2 Strref
"""    Strref

Index (32-bit) referring to a translated string in a `"dialog.tlk"` file.
"""
struct Strref
	index::Int32
end
# Strref(x::Strref) = x
# Index 0 corresponds to '<NO TEXT>', which is technically a valid string,
# but should not actually appear in-game: we use this value as a marker
# for invalid strings.
Base.isvalid(s::Strref) = (s.index > 0)
Base.show(io::IO, s::Strref) = print(io, "Strref(", s.index, ")")

#««2 Resource identifiers: Resref
"""    Resref{T}

A (long) resource descriptor: this contains a (static) type and a (dynamic)
namespace and name uniquely identifying the resource.
"""
struct Resref{T}
	name::StaticString{8}
	(::Type{Resref{T}})(s::AbstractString) where{T} = new{T}(lowercase(s))
# 	name::String # of the form 'namespace/resource'
end
resourcetype(::Type{<:Resref{T}}) where{T} = T
resourcetype(r::Resref) = resourcetype(typeof(r))
# @inline resref_join(ns::AbstractString, n::AbstractString) =
# 	isempty(ns) ? n : ns*'/'*n
Base.isempty(r::Resref) = isempty(r.name)
(T::Type{<:Resref})(x::Resref) = T(x.name)

Base.show(io::IO, r::Resref) =
	print(io, "Resref\"", r.name, '.', resourcetype(r), '"')

Pack.unpack(io::IO, T::Type{<:Resref}) = unpack(io, StaticString{8})|>T
Pack.packed_sizeof(::Type{<:Resref}) = 8
Pack.pack(io::IO, r::Resref) = pack(io, r.name|>uppercase)

macro Resref_str(s)
	s = lowercase(s)
	i = findlast('.', s)
	isnothing(i) ? Resref{Symbol(s)} :
		view(s,1:i-1) |> Resref{view(s,i+1:length(s))|>Symbol}
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
root(x::RootedResource) = isdefined(x, :root) ? x.root : nothing
root!(x::RootedResource, r) = (x.root = r)
"""    RootResource

The root of a resource tree, i.e. a resource which will be saved in a game file.
This resource also holds an identifier designating it (and the game file).
"""
abstract type RootResource{T} <: RootedResource end
# Root resource is always its own root
root(x::RootResource) = x
root!(x::RootResource, _) = nothing
resourcetype(::RootResource{T}) where{T} = T
register!(::Nothing) = nothing # no root defined

mutable struct RootedResourceVector{T<:RootedResource,
		R<:RootResource} <: AbstractVector{T}
	v::Vector{T}
	root::R
	# new is *not* a variadic function:
	RootedResourceVector{T,R}(v,r) where{T,R} = new{T,R}(v,r)
	RootedResourceVector{T,R}(v) where{T,R} = new{T,R}(v)
end
# RootedResourceVector{T,R}(::UndefInitializer) where{T,R} =
# 	RootedResourceVector{T,R}()
root(v::RootedResourceVector) = isdefined(v, :root) ? v.root : nothing
root!(v::RootedResourceVector, r) = (v.root = r)
Pack.packed_sizeof(x::RootedResourceVector) = length(x)*packed_sizeof(eltype(x))

#««3 getproperty/setproperty! etc.
rr_skipproperties(::Any) = ()
@inline default_setproperty!(x, f::Symbol, v) =
	# default code; duplicated from Base.jl
	setfield!(x, f, convert(fieldtype(typeof(x), f), v))
@inline function rr_setproperty!(x::RootedResource, f::Symbol, v)
	default_setproperty!(x, f, v)
	# we need to put the register! call last, in case the `ref` property
	# was changed:
	f ∈ rr_skipproperties(x) || register!(root(x))
end
Base.setproperty!(x::RootedResource, f::Symbol, v) = rr_setproperty!(x, f, v)

# AbstractArray interface
Base.size(v::RootedResourceVector) = size(v.v)
Base.getindex(v::RootedResourceVector, i::Int) = v.v[i]
Base.setindex!(v::RootedResourceVector, x, i::Int) =
	(register!(root(v)); setindex!(v.v, x, i))
Base.resize!(v::RootedResourceVector, n::Integer) =
	(register!(root(v)); resize!(v.v, n); v)

"""    setroot!(x::RootedResource, newroot = x)

Recursively sets the root object for `x` and all its sub-fields."""
setroot!(x, r) = x
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
	@inline ResIO{T}(ref, io::X) where{T,X<:IO} =
		new{T,ResourceType(ResIO{T}),X}(ref, io)
end

Base.read(x::ResIO, T::Type{UInt8}) = read(x.io, T)
Base.seek(x::ResIO, n) = (seek(x.io, n); x)
Base.position(x::ResIO) = position(x.io)
Base.eof(x::ResIO) = eof(x.io)
Base.close(x::ResIO) = close(x.io)

root!(x::ResIO, r) = (x.root = r)

"`ResourceType{ResIO{T}}` = the root type to attach to this `ResIO` fd"
ResourceType(T::Type{<:ResIO})= Base.return_types(read,Tuple{T})|>only

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
Pack.fieldpack(::IO, _, ::Val{:root}, ::RootResource) = 0
Pack.unpack(io::ResIO, _, ::Val{:root}, T::Type{<:RootResource}) = io.root
Pack.packed_sizeof(_, ::Val{:root}, ::Type{<:RootResource}) = 0
@inline unpack_root(io::ResIO, T::Type{<:RootResource}) =
	(r = unpack(io, T); root!(io, r); r)

# The root resource contains a reference to its own name:
Pack.unpack(io::ResIO, _, ::Val{:ref}, T::Type{<:Resref}) = io.ref
Pack.fieldpack(::IO, _, ::Val{:ref}, ::Resref) = 0
Pack.packed_sizeof(_, ::Val{:ref}, ::Type{<:Resref}) = 0

Pack.unpack(::IO, _, _, T::Type{<:RootedResourceVector}) = T([])

#»»1
# ««1 tlk
# ««2 Type and constructors
@SymbolicEnum TextFlags::UInt16 begin
	IsEmpty = 0
	HasText
	HasSound
	AmbientMessage
	HasTokens
end
@with_kw mutable struct TLK_str
	flags::TextFlags = HasText
	sound::Resref"WAV" = Resref".wav"
	volume::Int32 = 0
	pitch::Int32 = 0
	offset::Int32 = 0
	length::Int32 = 0
	# (since game strings are constants, we only read/allocate a single string
	# and use substrings for all strings)
	# We can convert any string to a SubString: SubString(s);
	# this gives us a canonical constructor: TLK_str(; string::String).
	string::SubString{String}
end
const NO_SUBSTRING = view("", 1:0)
Pack.unpack(::IO, ::Type{SubString{String}}) = NO_SUBSTRING
Pack.pack(::IO, ::SubString{String}) = 0
@with_kw mutable struct TlkStrings
	constant::Constant"TLK V1  " = Auto()
	lang::UInt16 = 0
	nstr::Int32 = 0
	offset::Int32 = 0
	entries::Vector{TLK_str} = Auto()
end
Base.show(io::IO, tlk::TlkStrings) =
	print(io, "<TlkStrings with ", length(tlk.entries), " entries>")
Base.isempty(v::TlkStrings) = isempty(v.entries)
Base.empty!(v::TlkStrings) = empty!(v.entries)
Base.length(v::TlkStrings) = length(v.entries)

#««2 I/O from tlk file
function Base.read(io::IO, ::Type{<:TlkStrings})
	buf = read(io, String) # helps with speed
	io = IOBuffer(buf) # helps (a lot) with speed
	f = unpack(io, TlkStrings)
	unpack!(io, f.entries, f.nstr)
	for (i,s) in pairs(f.entries)
# 		s.string = string0(io, f.offset + s.offset, s.length)
		s.string = substring0(buf, f.offset + s.offset, s.length)
# 		k = (string0(io, f.offset + s.offset, s.length))
# 		@assert s.string == k
# 		get!(f.firstindex, s.string, i) # set it only if it is not already set
	end
	close(io)
	return f
end
TlkStrings(file::AbstractString) = read(file, TlkStrings)
function Base.write(io::IO, tlk::TlkStrings)
	tlk.offset = 18 + 26*length(tlk.entries)
	tlk.nstr = length(tlk.entries)
	offset = 0
	for s in tlk.entries
		s.length = length(codeunits(s.string)) # zero byte not included
		s.offset = offset
		offset+= s.length
	end
	pack(io, tlk) # this also writes the TLK_str entries (without the strings)
	@assert position(io) == tlk.offset "(position,offset) = $((position(io),tlk.offset))"
	for s in tlk.entries
		@assert position(io) == tlk.offset + s.offset
		write(io, codeunits(s.string)) # zero byte not included
	end
end
#««2 Dictionary interface
# push!: always appends a new string
# tlk[strref]: returns text for this strref
Base.push!(tlk::TlkStrings, s::AbstractString) =
	push!(tlk.entries, TLK_str(; string=s))
function Base.getindex(tlk::TlkStrings, s::Strref)
	i = s.index
	i ∈ eachindex(tlk.entries) || (i = 0)
	@inbounds tlk.entries[i+1]
end
Base.findall(f::Function, tlk::TlkStrings) =
	[ Strref(i-1) for (i,s) in pairs(tlk.entries) if f(s.string) ]
Base.findall(r, tlk::TlkStrings) = findall(s->contains(s, r), tlk)
Base.only(tlk::TlkStrings, s::AbstractString) = findall(isequal(s), tlk)|>only

# ««1 key/bif
# ««2 Integer types
"""    BifIndex
32-bit index of resource in bif files.
"""
struct BifIndex; data::UInt32; end
sourcefile(r::BifIndex) = r.data >> 20
tilesetindex(r::BifIndex) = (r.data >> 14) && 0x3f
resourceindex(r::BifIndex) = r.data & 0x3fff

"""    Restype

16-bit value indexing a resource type in key file.
(This is immediately translated to a string value when reading this file).
"""
struct Restype; data::UInt16; end
# ««2 ResIO type table

# Static correspondence between UInt16 and symbols
# This dictionary is quite crucial for (loading) performance;
# we use a custom hash function to improve speed:
# (This hash functions produces unique keys modulo 64, which is what is
# used by the hash table)
Base.hash(x::Restype) = UInt(7*(x.data>>6)+(x.data&0x3f))
# (and yes, we checked that this indeed faster than Base.ImmutableDict).
const RESOURCE_TABLE = Dict(Restype(a) => b for (a,b) in (#««
	0x0001 => :bmp,
	0x0002 => :mve,
	0x0004 => :wav,
	0x0005 => :wfx,
	0x0006 => :plt,
	0x03E8 => :bam,
	0x03E9 => :wed,
	0x03EA => :chu,
	0x03EB => :tis,
	0x03EC => :mos,
	0x03ED => :itm,
	0x03EE => :spl,
	0x03EF => :bcs,
	0x03F0 => :ids,
	0x03F1 => :cre,
	0x03F2 => :are,
	0x03F3 => :dlg,
	0x03F4 => Symbol("2da"),
	0x03F5 => :gam,
	0x03F6 => :sto,
	0x03F7 => :wmp,
	0x03F8 => :chr,
	0x03F9 => :bs,
	0x03FA => :chr2,
	0x03FB => :vvc,
	0x03FC => :vfc,
	0x03FD => :pro,
	0x03FE => :bio,
	0x03FF => :wbm,
	0x0400 => :fnt,
	0x0402 => :gui,
	0x0403 => :sql,
	0x0404 => :pvrz,
	0x0405 => :glsl,
	0x0408 => :menu,
	0x0409 => :lua,
	0x040A => :ttf,
	0x040B => :png,
	0x044C => :bah,
	0x0802 => :ini,
	0x0803 => :src,
))#»»
Symbol(x::Restype) = get(RESOURCE_TABLE, x) do
	Symbol(:Restype_, @sprintf("%d", x.data)) end

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

 - `open(key, name, type)`: returns the `IOBuffer` for this resource.
 - `keys(key, type)`: returns an iterator over all names of resources
   of this type present in the game.
"""
@with_kw struct KeyIndex
	directory::Base.RefValue{String} = Ref("")
	bif::Vector{String} = Auto()
	location::Dict{Symbol,Dict{StaticString{8},BifIndex}} =
		Auto(s => Dict{StaticString{8},BifIndex}() for s in values(RESOURCE_TABLE))
end
function Base.push!(key::KeyIndex, ref::KEY_res)
	d = get!(key.location, Symbol(ref.type)) do; valtype(key.location)() end
	d[lowercase(ref.name)] = ref.location
end
Base.length(key::KeyIndex) = key.location|>values.|>length|>sum
Pack.unpack(io::IO, ::Type{KEY_res}) = # this helps with speed...
	KEY_res(unpack(io,StaticString{8}), unpack(io,Restype), unpack(io,BifIndex))

init!(key::KeyIndex, filename::AbstractString) = open(filename) do io
	io = IOBuffer(read(io, String)) # helps (a lot) with speed
	# These numbers come from a heavily modded BGT game — by directly
	# allocating correctly-sized hash tables we avoid spending too much
	# time enlarging them:
	for (x,y) in ( :bam => 14, :wav => 13, :cre => 12, :bmp => 12,
		:pvrz => 12, :spl => 11, :bcs => 11, :itm => 11, :dlg => 11,
		:chr => 10, Symbol(2,:da) => 10, :mos => 10, :are => 10, :wed => 9,
		:ini => 9, :tis => 9, :sto => 8, :pro => 8, :vvc => 8, :plt => 8)
		sizehint!(key.location[x], 1<<y)
	end
	key.directory[] = dirname(filename)
	header = unpack(io, KEY_hdr)
	bifentries = unpack(seek(io, header.bifoffset), KEY_bif, header.nbif)
	push!(key.bif, (string0(io, x.offset, x.namelength) for x in bifentries)...)
	v = unpack(seek(io, header.resoffset), KEY_res, header.nres)
	for res in v
		push!(key, res)
	end
	return key
	close(io)
end

KeyIndex(filename::AbstractString) = init!(KeyIndex(), filename)
const XOR_KEY = "88a88fba8ad3b9f5edb1cfeaaae4b5fbeb82f990cac9b5e7dc8eb7aceef7e0ca8eeaca80cec5adb7c4d08493d5f0ebc8b49dccafa595ba9987d29de391ba90ca"|>hex2bytes

function decrypt(io::IO)
	peek(io) != 0xff && return io
	buf = read(seek(io,1), String)|>codeunits # used to be Vector{UInt8}
	for i in eachindex(buf)
		buf[i] ⊻= @inbounds XOR_KEY[mod1(i-1, length(XOR_KEY))]
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
	r = resources[index+1]; IOBuffer(read(seek(io, r.offset), r.size))
end
@inline function Base.open(key::KeyIndex, name::AbstractString, type::Symbol)
	dict = get(key.location, type, nothing); isnothing(dict) && return nothing
	loc = get(dict, StaticString{8}(name), nothing)
	isnothing(loc) && return nothing
	bif = joinpath(key.directory[], key.bif[1+sourcefile(loc)])
	return bifcontent(bif, resourceindex(loc))
end
@inline Base.haskey(key::KeyIndex, name::AbstractString, type::Symbol) =
	haskey(key.location, type) &&
		haskey(key.location[type], name|>StaticString{8})
Base.keys(key::KeyIndex, type::Symbol) = keys(key.location[type])

# ««1 ids
# useful ones: PROJECTL SONGLIST ITEMCAT NPC ANISND ?
function Base.read(io::IO, f::ResIO"IDS"; debug=false)
	io = decrypt(io)
	debug && (mark(io); println("(", read(io, String), ")"); reset(io))
	line = readline(io)
	(!contains(line, " ") || startswith(line, "IDS")) && (line = readline(io))
	!isnothing(match(r"^[0-9]*$", line)) && (line = readline(io))
	list = Pair{Int,String}[]
	while !eof(io)
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
	MatrixWithHeaders(rows::AbstractVector{<:AbstractString},
		cols::AbstractVector{<:AbstractString}, matrix::AbstractMatrix{T}) where{T}=
		new{T}(rows, cols, matrix)
end

Base.size(m::MatrixWithHeaders) = size(m.matrix)
Base.getindex(m::MatrixWithHeaders, i::Integer...) = getindex(m.matrix, i...)
function Base.getindex(m::MatrixWithHeaders,
		s1::AbstractString, s2::AbstractString)
	i1 = findfirst(==(s1), m.rows)
	@assert !isnothing(i1) "Row header not found: '$s1'"
	i2 = findfirst(==(s2), m.cols)
	@assert !isnothing(i2) "Row header not found: '$s2'"
	return m.matrix[i1,i2]
end
function Base.read(io::IO, f::ResIO"2DA"; debug=false, aligned=false)
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
Pack.fieldpack(::IO, _, ::Val{:effects}, ::AbstractVector{<:ItemEffect}) = 0
# create a virtual “not_usable_by” item property which groups together
# all the 5 usability fields in the item struct:
@inline function Base.getproperty(i::Item, name::Symbol)
	name == :not_usable_by && return UsabilityFlags(
		UInt64(i.usability) | UInt64(i.kit1) << 32 | UInt64(i.kit2) << 40 |
		UInt64(i.kit3) << 48 | UInt64(i.kit4) << 56)
	getfield(i, name)
end
rr_skipproperties(::Item) = (:abilities_offset, :abilities_count,
		:effect_offset, :effect_index, :effect_count)
rr_skipproperties(::ItemAbility) = (:effect_index, :effect_count)
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
		itm.ref.name, itm.weight, itm.price, itm.lore,
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
function Base.read(io::ResIO"ITM")
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
	allegiance::UInt8 # ea.ids
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
function Base.read(io::IO, f::ResIO"CRE")
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
	StateKey(i::Integer) = new(i)
	StateKey(::typeof(!isvalid)) = new(~zero(UInt))
end
Base.isvalid(s::StateKey) = (s ≠ StateKey(!isvalid))
Base.isless(a::StateKey, b::StateKey) = isless(a.n, b.n)

@with_kw mutable struct Transition #{X}
	# XXX both Transition and State can be made immutable (stored in vectors)
	flags::TransitionFlags = Auto(0)
	text::Strref = Strref(0)
	journal::Strref = Strref(0)
	trigger::String = ""
	action::String = ""
	target::Tuple{Resref"dlg",StateKey}
# 	actor::Resref"dlg"
# 	state::StateKey
end
function Base.show(io::IO, mime::MIME"text/plain", t::Transition)
	print(io, "\e[48;5;3m   ")
	contains(t.flags, Terminates) ||
		print(io, '⇒', t.target[1].name, ':', t.target[2].n, ';')
	println(io, ' ', t.flags|>rp, "\e[m")
	contains(t.flags, HasTrigger) &&
		println(io, "  \e[31;1mT: \e[m\e[31m", chomp(t.trigger), "\e[m")
	println(io, "  \e[34m", t.text|>str|>f75, "\e[m")
	contains(t.flags, HasJournal) &&
		println(io, "  \e[36m", t.journal|>str|>f75, "\e[m")
	contains(t.flags, HasAction) &&
		println(io, "  \e[33;1mA: \e[m\e[33m", chomp(t.action), "\e[m")
end
function Base.show(io::IO, mime::MIME"text/julia", t::Transition;
		name, prefix="")
	a, k = t.target[1].name, t.target[2].n
	contains(t.flags, HasTrigger) &&
		println(io, "\t", prefix, "trigger(", repr(t.trigger), ")")
	target = contains(t.flags, Terminates) ? exit : a == name ? k : (a, k)
	text = contains(t.flags, HasText) ? str(t.text) : nothing
	ttarget = (text => target)
	(ttarget == (nothing => exit)) && (ttarget = exit)
	println(io, "\t", prefix, "reply(", ttarget, ")")
	contains(t.flags, HasJournal) &&
		println(io, "\t", prefix, "journal(", repr(str(t.journal)), ")")
	contains(t.flags, HasAction) &&
		println(io, "\t", prefix, "journal(", repr(t.action), ")")
	println(io)
end
@with_kw mutable struct State
	text::Strref
	transitions::Vector{Transition} = Auto()
	trigger::String = ""
	priority::Float32 = zero(Float32)
	age::UInt # secondary sorting key
	position::UInt = 0 # index in outputted file
end
function Base.show(io::IO, mime::MIME"text/plain", s::State)
	!isempty(s.trigger) && println(io, "\e[31m", chomp(s.trigger), "\e[m")
	println(io, "\e[35m", s.priority, " age=", s.age, "\e[m")
	println(io, s.text|>str|>f75)
	for t in s.transitions; show(io, mime, t); end
end
function Base.show(io::IO, mime::MIME"text/julia", s::State;
		name, key, prefix = "")
	!isempty(s.trigger) &&
		println(io, prefix, "trigger(", repr(s.trigger), ")")
	println(io, prefix, "say(", repr(key => str(s.text)),
		iszero(s.priority) ? "" : "; priority = $(s.priority)", ") # age = ", s.age)
	println(io, "# ", length(s.transitions), " transitions: ")
	for t in s.transitions
		show(io, mime, t; prefix, name)
	end
end
sortkey(s::State) = (s.priority, s.age)
Pack.fieldpack(::IO, ::Type{<:State}, ::Val{:position}, _) = 0
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
	# sorted list of states when writing the file
	sorted_keys::Vector{StateKey} = Auto()
end
const NO_ACTOR = Actor(;ref = Resref".dlg")
rr_skipproperties(::Actor) = (:number_states, :offset_states,
	:number_transitions, :offset_transitions,
	:offset_state_triggers, :number_state_triggers,
	:offset_transition_triggers, :number_transition_triggers,
	:number_actions, :offset_actions, :sorted_keys)
Pack.fieldpack(::IO, ::Type{<:Actor}, ::Val{:states}, _) = 0
Pack.fieldpack(::IO, ::Type{<:Actor}, ::Val{:sorted_keys}, _) = 0
firstlabel(a::Actor) =
	first(i for i in 0:length(a.states) if !haskey(a.states, StateKey(i)))
# 	Iterators.filter(i->!haskey(a.states, StateKey(i)), 0:length(a.states))|>first

function Base.show(io::IO, mime::MIME"text/plain", a::Actor)
	for (k, v) in sort(pairs(a.states))
		println(io, "\e[7m state $(string(k.n)): $(length(v.transitions)) transitions\e[m")
		show(io, mime, v)
	end
end
function Base.show(io::IO, mime::MIME"text/julia", a::Actor; prefix="")
	println(io, "# actor '$(a.ref.name)' with $(length(a.states)) states:")
	for k in sort(collect(keys(a.states)); by=k->sortkey(a.states[k]))
		show(io, mime, a.states[k]; key = Int(k.n), prefix, a.ref.name)
	end
end

#««2 I/O
@inline dialog_strings(io::IO, offset, count)= [string0(io, s.offset, s.length)
	for s in unpack(seek(io, offset), DLG_string, count)]
function Base.read(io::ResIO"DLG")::Actor
	actor = unpack_root(io, Actor)
	st_triggers = dialog_strings(io, actor.offset_state_triggers,
			actor.number_state_triggers)
	tr_triggers = dialog_strings(io, actor.offset_transition_triggers,
			actor.number_transition_triggers)
	actions = dialog_strings(io, actor.offset_actions, actor.number_actions)

	vs = unpack(seek(io,actor.offset_states), DLG_state, actor.number_states)
	vt = unpack(seek(io, actor.offset_transitions),
		DLG_transition, actor.number_transitions)
	@inline getval(list, i) = i+1 ∈ eachindex(list) ? (@inbounds list[i+1]) : ""

	for (i, s) in pairs(vs)
		state = State(; s.text, trigger = getval(st_triggers, s.trigger),
			age = i-1)
		for j in s.first_transition+1:s.first_transition + s.number_transitions
			t = vt[j]
			push!(state.transitions, Transition(;t.flags, t.text, t.journal,
				trigger= contains(t.flags, HasTrigger) ? tr_triggers[t.trigger+1] : "",
				action= contains(t.flags, HasAction) ? actions[t.action+1] : "",
				target = (t.actor, t.state|>StateKey)))
		end
		actor.states[StateKey(i-1)] = state
	end
	return actor
end
	#= patterns observed in Bioware dialogs:
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
pack_string(io::IO, pos::Ref{<:Integer}, v::AbstractVector{<:AbstractString}) =
	for s ∈ v; pack_string(io, pos, s); end
"""
    reindex!(actor)

Sorts all states by increasing (priority, age), and reindexes them
in this order (starting at 0) for determining written transition keys.
Returns the sorted list of states.
"""
function reindex!(a::Actor)
	# We need:
	#  - sorted list of states (or state keys)
	#  - map (symbolic key) => (index in this list)
	#    (or an
	# XXX todo: add a `issorted` flag?
	a.sorted_keys = keys(a.states)|>collect
	sort!(a.sorted_keys; by = k->sortkey(a.states[k]))
	for (i, k) in pairs(a.sorted_keys)
		a.states[k].position = i-1
	end
end
function Base.write(io::IO, a::Actor)
	# XXX we must assume that all actors are reindexed prior to writing
	reindex!(a)
	vs = sizehint!(DLG_state[], a.number_states)
	vt = sizehint!(DLG_transition[],
		sum(length(s.transitions) for s ∈ values(a.states)))
	vst = UniqueVector{String}()
	vtt = UniqueVector{String}()
	va = UniqueVector{String}()
	ti = 0 # transition index
	for k ∈ a.sorted_keys; s = a.states[k]
		nt = length(s.transitions)
		st = isempty(s.trigger) ? 0 : findfirst!(isequal(s.trigger), vst)
		push!(vs, DLG_state(s.text, ti, nt, st - 1))
		ti += nt
		for t in s.transitions
			tt = contains(t.flags, HasTrigger) ?
				findfirst!(isequal(t.trigger), vtt) : 1
			ta = contains(t.flags, HasAction) ?
				findfirst!(isequal(t.action), va) : 1
			push!(vt, DLG_transition(t.flags, t.text, t.journal, tt-1, ta-1,
				t.target[1],contains(t.flags,Terminates) ? 0 : stateindex(t.target...)))
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
function decompile(a::Actor, filename = a.ref.name*".jl"; prefix="")
	open(filename, "w") do io
		show(io, MIME"text/julia"(), a; prefix)
	end
end
#««2 Context structure
"""    DialogContext

Holds current (mutable) data for building dialog:
 - (actor, key) of last inserted state,
 - next actor for which text will be inserted,
 - current PC reply."""
@with_kw mutable struct DialogContext
	# Note that the mutating functions below only use (and set)
	# source_actor and source_key; target_actor is stored only as a
	# convenience for the `Game`-level functions.
	source_actor::Actor = NO_ACTOR
	source_key::StateKey = Auto(!isvalid)
	target_actor::Actor = NO_ACTOR
	current_transition_idx::Int = 0 # index into source_state.transitions
	trigger::Union{Nothing,String} = nothing
end

source_actor(c::DialogContext) =
	(@assert !isempty(c.source_actor.ref); c.source_actor)
source_state(c::DialogContext) =
	(@assert isvalid(c.source_key); source_actor(c).states[c.source_key])

function set_source!(c::DialogContext, key)
	@assert haskey(c.source_actor.states, key);
	c.source_key = key
	c.current_transition_idx = 0
end
set_source!(c::DialogContext, actor, key) =
	(@assert !isempty(actor.ref); c.source_actor=actor; set_source!(c,key))

if_current_state(f::Function, c::DialogContext) =
	if_haskey(f, c.source_actor.states, c.source_key)

current_transition(c::DialogContext) =
	source_state(c).transitions[c.current_transition_idx]
@inline has_pending_transition(c::DialogContext) =
	!iszero(c.current_transition_idx) &&
	if_current_state(c) do s; !isempty(s.transitions) &&
		!isvalid(s.transitions[c.current_transition_idx].target[2])
	end

target_actor(c::DialogContext) =
	(@assert !isempty(c.target_actor.ref.name); c.target_actor)
target_actor!(c::DialogContext, actor::Actor) =
	(@assert !isempty(actor.ref); c.target_actor = actor)

implicit_transition(actor, key::StateKey)= Transition(; target=(actor.ref, key))

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

#««2 Dialog-building functions
# These are the lowest-level functions handling `Dialog` structures.
# They use the low-level types: `StateKey`, `Strref`.
# Conversion is performed by higher-level functions (`say`, `reply` etc.)
function add_state!(c::DialogContext, actor, key, text::Strref;
		trigger=nothing, kwargs...)
	dict = actor.states
	@assert !haskey(dict, key) "state $key already exists"
	println("\e[1mINSERT STATE $(key.n): $(first(text|>str,40))…\e[m")
	if_current_state(c) do source # insert implicit transition
		isempty(source.transitions) && begin
			println("   \e[35m implicit transition from $(c.source_actor.ref.name):$(c.source_key.n)")
			push!(source.transitions, implicit_transition(actor, key))
		end
	end
	if has_pending_transition(c)
		println("  \e[32m Resolve pending transition to $key\e[m")
		current_transition(c).target = (actor.ref, key)
	end
	register!(actor)
	trigger = something(get_trigger(c, trigger), "")
	target = State(; age=length(dict), text, trigger, kwargs...) # returns it
	dict[key] = target
	set_source!(c, actor, key)
end

"""    tail_insert!(c, actor, key, text; trigger)
Given the current state A and transitions A—aᵢ—→ Bᵢ:
 * create a new state X(key, text);
 * if there is any pending transition from A, connect it to X,
   otherwise create an implicit transition from A to X;
 * move all non-pending transitions aᵢ to X;
 * declare X to now be the current state.

The end result should be A ——→ X —aᵢ—→ Bᵢ.
"""
function tail_insert!(c::DialogContext, actor::Actor, key::StateKey,
		text::Strref; trigger=nothing, kwargs...)
	t_states = actor.states
	@assert !haskey(t_states, key) "state $key already exists"
	println("\e[31;1mTAIL INSERT: new state=($(actor.ref),$key)\e[m")
	register!(actor)
	c|>source_actor ≠ actor && register!(c|>source_actor)
	trigger = something(get_trigger(c, trigger), "")
	source = source_state(c)
	target = State(; age=length(t_states), text, trigger, kwargs...)
	if has_pending_transition(c)
		println("  \e[32m Resolve pending transition to $key\e[m")
		trans = current_transition(c)
		trans.target = (actor.ref, key)
		target.transitions = deleteat!(source.transitions, c.current_transition_idx)
		source.transitions = [ trans ]
	else
		println("  \e[33m Add implicit transition to $key\e[m")
		target.transitions = source.transitions
		source.transitions = [ implicit_transition(actor, key) ]
	end
	t_states[key] = target
	set_source!(c, actor, key)
end

"""    add_transition!(c, text, actor, key)

Creates a transition from last state to (actor, key).
"""
function add_transition!(c::DialogContext, text::Strref, actorref::Resref"dlg",
		key::StateKey; position = nothing, terminates = false, journal = nothing,
		trigger = nothing, action = nothing)
	@assert !has_pending_transition(c) "unsolved pending transition"
	!isvalid(key) && println("  \e[31madd pending transition\e[m")
	println("  \e[34m", terminates ? "FINAL TRANSITION" :
		"TRANSITION TO $actorref/$key", "\e[m")
	# XXX check if some flags are still missing
	flags = TransitionFlags(0)
	trigger = get_trigger(c, trigger)
	terminates && (flags |= Terminates)
	isvalid(text) && (flags |= HasText)
	isnothing(journal) ? (journal = Strref(0)) : (flags |= HasJournal)
	isnothing(trigger) ? (trigger = "") : (flags |= HasTrigger)
	isnothing(action) ? (action = "") : (flags |= HasAction)
	t = Transition(;target=(actorref,key), text, journal, trigger, action, flags)
	position = something(position, 1 + length(source_state(c).transitions))
	register!(c|>source_actor)
	c.current_transition_idx = position
	insert!(source_state(c).transitions, position, t)
	return t
end

function add_action!(t::Transition, s::AbstractString; override=false)
	@assert(override || !contains(t.flags, HasAction))
	t.flags |= HasAction
	t.action = s
end
function add_journal!(t::Transition, s::AbstractString; override=false)
	# XXX journal flags
	@assert(override || !contains(t.flags, HasJournal))
	t.flags |= HasJournal
	t.journal = s
end

#««1 GameStrings
# Game strings (in any language) exposed as a dictionary.
# The keys are either:
#  - for original game strings: integers;
#  - for strings added by this module: the string as present in the
#  source file (including any "?{}" comments still present).
# Strings are internally saved as Strrefs in game objects, so we have two maps:
#  string keys -> Strrefs -> game strings.

# `Nothing` is inserted manually where it makes sense
# `AbstractString` is a decoy to catch those non-prefixed strings with
# the correct error message
const StringKey = Union{Integer,MarkedString,AbstractString}

#««2 Data structure
# This holds (language path) => (has F version?); default language is first
const LANGUAGES = (
	"en_US" => false,
	"cs_CZ" => false,
	"de_DE" => true,
	"es_ES" => true,
	"fr_FR" => true,
# 	"hu_HU" => false,
# 	"it_IT" => true,
# 	"ja_JP" => true,
# 	"ko_KR" => false,
# 	"pl_PL" => true,
# 	"pt_BR" => true,
# 	"ru_RU" => true,
# 	"tr_TR" => false,
# 	"uk_UA" => false,
# 	"zh_CN" => false,
)
"""    LANGUAGE_FILES

Iterator of (language regex, language .tlk file, language dict id).

    LANGUAGE_DICT

Map of (language dictionary id) -> (language id1, language id2|0)."""
const LANGUAGE_FILES, LANGUAGE_DICT = begin
	data = Tuple{Regex,String,Int}[]; dict=NTuple{2,Int}[]
	for (lang, flag) in LANGUAGES
		l1, l2 = split(lang, '_'); l2 = lowercase(l2)
		r = l1 == l2 ? "^"*l1*".*" : "^("*l1*'|'*l2*").*"
		flag && push!(data, (Regex(r*'F',"i"), joinpath(lang,"dialogF.tlk"), 0))
		push!(data, (Regex(r,"i"), joinpath(lang, "dialog.tlk"), 1+length(dict)))
		n = length(data)
		push!(dict, flag ? (n, n-1) : (n, 0))
	end
	(data...,), (dict...,)
end
"""    GameStrings

Collection of game strings, indexed by string and language.

### Interface

 - `Strref(gamestrings, x)`: string key to `Strref` conversion.
 - `gamestrings[language, strref]`: `Strref` to `String` conversion.
 - `commit(gamestrings)`: saves state to filesystem.
 - `init!(directory)`: loads state from filesystem.

### Fields

 - let N0 = number of base-game strings (34000)
       N1 = number of new tlk strings
       N2 = number of memory tlk strings

Then Strref(0..N0-1) are base-game strings
     Strref(N0..N0+N1+N2-1) are new strings = indexed by strings

   Strref(i + N0-1) ⇔ new_string[i]

Stored value is N0-1 == 33999 for BG2; constant even when modifying tlk

Displaying a string for Strref(i): if i ≤ #tlk-1 then tlk[i+1]
  else new_string[i-N0+1] (<- only the tail end is used)
"""
@with_kw_noshow struct GameStrings
	lang_dir::String
	language::Base.RefValue{Int} = Ref(0)
	# right now tlk is only used for displaying; thus
	# we don't need the whole db in memory, only the current language
	tlk::TlkStrings
# 	tlk::Vector{TlkStrings} = [ TlkStrings() for _ in LANGUAGE_FILES ]
	# we collect all new strings (for any language) in the same structure,
	# so that all the strref numbers advance in sync; Int8 indexes the
	# language:
	new_strings::UniqueVector{String} = Auto()
	offset::Int
	translations::Vector{Dict{String,String}} =
		[ Dict{String,String}() for _ in LANGUAGE_DICT ]
end
Base.show(io::IO, g::GameStrings) = print(io, "GameStrings<",
# 	count(!isempty, g.tlk), " languages, ",
	length(g.tlk), " strings, ", length(g.new_strings), " keyed strings>")

# function load_tlk!(g::GameStrings, i)
# 	# Loads the strings for this language if not already done:
# 	!isempty(g.tlk[i]) && return
# 	g.tlk[i] = read(joinpath(g.lang_dir, LANGUAGE_FILES[i][2]), TlkStrings)
# end
# set_language!(g::GameStrings, i) = (g.language[] = i; load_tlk!(g, i))
function GameStrings(directory::AbstractString, state)
	tlk = read(joinpath(directory, LANGUAGE_FILES[1][2]), TlkStrings)
	return GameStrings(; lang_dir = directory, tlk,
		new_strings = Auto(get(state, "keys", String[])),
		offset = get(state, "offset", length(tlk)-1))
end
# ««2 Pseudo-dictionary interface
# gamestrings[strref]: returns the string according to current language
# (or its only language for new strings)
function Base.getindex(g::GameStrings, s::Strref)
	i = s.index; i = max(i, zero(i)) #; i+= oneunit(i) # ensure ≥ 1
	i < length(g.tlk) ? g.tlk.entries[i+1].string : g.new_strings[i - g.offset]
# 	n = length(g.tlk)
# 	i ≤ n ? g.tlk.entries[i].string : g.new_strings[i - n]
end
"""    Strref(gamestrings, s)

Returns the `Strref` for the key `s`, which may be either `nothing`,
an integer, or a string. In the last case, the reference is created
if it does not yet exist.
"""
Strref(::GameStrings, i::Integer) = Strref(i)
Strref(::GameStrings, ::Nothing) = Strref(0) # graceful fail
Strref(g::GameStrings, s::MarkedString) =
	Strref(g.offset + findfirst!(isequal(s.str), g.new_strings))
Strref(::GameStrings, ::AbstractString) =
	error("In-game strings must be marked for translation by prefixing with `_`")

# """    search(game, strings, ResIO"type", text)
# 
# Searches for the given text (string or regular expression)
# in the names of all resources of the given `type`.
# `strings` is the string database used for translation.
# """
# function search(g::Game, str::TlkStrings, R::Type{<:ResIO}, text)
# 	for res in all(g, R)
# 		s = res|>read
# 		contains(s, text) || continue
# 		@printf("%c%-8s %s\n", res isa ResIOFile ? '*' : ' ', res.name, s)
# 	end
# end
# ««2 Translations
function load_translations!(g::GameStrings,
		directory::AbstractString = call_directory())
	empty!.(g.translations)
	for filename in readdir(directory)
		(base, ext) = splitext(filename)
		lowercase(ext) == ".po" || continue
		for (lang, _, dict_id) in LANGUAGE_FILES
			!iszero(dict_id) && contains(base, lang) && 
				MarkedStrings.read_po!(g.translations[dict_id], filename)
		end
	end
end

"""    translate(gamestrings, dict_id)

Returns an iterator of (language_id => translated strings into this language).
"""
@inline function translate(g::GameStrings, dict_id)
	(l1, l2) = LANGUAGE_DICT[dict_id]
	dict = g.translations[dict_id]
	process= f->[MarkedStrings.remove_comments(f(s)) for s ∈ g.new_strings]
	sM, sF = "?{F}" => "?{M}", "?{M}" => "?{F}"
	if iszero(l2) # simple case
		(l1=>process() do s; get(dict, s, s) end,)
	else
		(l2=>process() do s; get(dict, replace(s,sF)) do; get(dict, s, s) end; end,
		 l1=>process() do s; get(dict, replace(s,sM)) do; get(dict, s, s) end; end)
	end
end
@inline function commit(g::GameStrings)
	# Save game strings for all translations
	for j in eachindex(LANGUAGE_DICT), (lang_id, tr_strings) in translate(g, j)
		file = joinpath(g.lang_dir, LANGUAGE_FILES[lang_id][2])
		tlk = read(file, TlkStrings)
		println(tlk)
		# append those strings to language file
		for s in tr_strings
			push!(tlk, s)
		end
		println(tlk)
		println("writing strings to $file: ")
		for s in tr_strings
			println("   ", s)
		end
		mktemp(g.lang_dir) do tmp, io
			println("using temporary file: ", tmp)
			write(io, tlk)
			mv(tmp, file; force=true)
		end
	end
end
state(g::GameStrings) = Dict("offset" => g.offset, "keys" => g.new_strings)

#««1 GameResources
#««2 Auxiliary structure: GameChanges
struct GameChanges
	itm::Dict{Resref"itm",Item}
	dlg::Dict{Resref"dlg",Actor}
	GameChanges() = new((Auto() for _ in 1:fieldcount(GameChanges))...,)
end
alldicts(c::GameChanges) = (getfield(c,i) for i in 1:nfields(c))
Base.empty!(c::GameChanges) = c|>alldicts.|>empty!
Base.length(c::GameChanges) = c|>alldicts.|>length|>sum
for T in fieldnames(GameChanges)
	@eval changes(g::GameChanges,::Type{<:Resref{$(QuoteNode(T))}})=g.$T
end
changes(g::GameChanges, r::Resref) = changes(g, typeof(r))
Base.haskey(g::GameChanges, r::Resref) = haskey(changes(g, r), r)
Base.get(f::Function, g::GameChanges, r::Resref)= get(f, changes(g,r),r)
Base.get!(f::Function,g::GameChanges, r::Resref)= get!(f,changes(g,r),r)
Base.get(g::GameChanges, r::Resref, x)= get(changes(g,r), r, x)
Base.get!(g::GameChanges, r::Resref, x)= get!(changes(g,r), r, x)
Base.delete!(g::GameChanges, r::Resref) = delete!(changes(g,r), r)
Base.keys(g::GameChanges, x...) = keys(changes(g, x...))

Base.show(io::IO, ::MIME"text/plain", c::GameChanges) = for dict in alldicts(c)
	println("\e[33mmodified $(valtype(dict)|>nameof): $(length(dict))\e[m")
	for (k,v) in pairs(dict)
		println("  $(k.name)\t$(v.ref.name)")
	end
end
#««2 Data type
@with_kw_noshow struct GameResources
	override_directory::String
	# Resources data:
	key::KeyIndex = KeyIndex()
	override::Dict{Symbol, Dict{StaticString{8},StaticString{8}}} =
		Auto(s => Dict{StaticString{8},StaticString{8}}()
			for s in fieldnames(GameChanges))
	namespace::Base.RefValue{String} = Ref("")
	shortref::Dict{String,StaticString{8}} = Auto()
	changes::GameChanges = Auto()
end
Base.show(io::IO, g::GameResources) = print(io, "GameResources<",
	length(g.key), " keys, ", g.override|>values.|>length|>sum, " overrides, ",
	length(changes(g)), " changes>")
function GameResources(directory, state)
	g = GameResources(; override_directory = joinpath(directory, "override"))
	g.namespace[] = ""
	init!(g.key, joinpath(directory, "chitin.key"))
# 	empty!.(values(g.override))
	println("read ", length(g.key), " key resources")
	if !isdir(g.override_directory)
		println("created override directory: ", g.override_directory)
		mkdir(g.override_directory)
	else
		for f in readdir(g.override_directory)
			(name, ext) = f|>basename|>splitext
			length(name) > 8 && continue
			T = Symbol(ext[2:end]|>lowercase)
			if_haskey(g.override, T) do state
				key = lowercase(name)
				v = get(state, key, nothing)
				if isnothing(v)
					state[key] = name
				else
					error("file conflict: $(state[key]), $name")
				end
			end
		end
		println("read $(g.override|>values.|>length|>sum) override resources")
	end

	copy!(g.shortref, state)
	g.namespace[] = "user"
	return g
end
#««2 String → Resref
"""    ShortrefIterator(s)

Iterator producing, in order:  "foobar", "foobar00".."foobar99",
"fooba000".."fooba999" etc.
"""
struct ShortrefIterator{L}
	name::StaticString{L}
	# XXX: allow other bases (e.g. base 16)
end
textlength(::ShortrefIterator{L}) where{L} = L
function Base.iterate(r::ShortrefIterator, i = 0)
	iszero(i) && return (r.name, 10)
	k = ndigits(i)-1; q = 10^k
	(i÷q) > 1 && (i = q*= 10; k+= 1)
	return (eltype(r)(first(r.name, textlength(r)-k)*
		(digits(i, pad=k)[1:k]|>reverse|>join)), i+1)
end
Base.length(r::ShortrefIterator) = 10^textlength(r)+1
Base.eltype(::ShortrefIterator{L}) where{L} = StaticString{L}

"""    available_resref

Given a string `s` (only used as a seed), returns a `StaticString{8}`
which is guaranteed to designate a fresh `Resref{T}`."""
function available_resref(g::GameResources, R::Type{Resref{T}}, str) where{T}
	# XXX invent a better way to shorten (by words?)
	# remove non-filename characters:
	fname = replace(first(str|>basename|>lowercase, 8),
		r"[^-!#$%&0-9@a-zA-Z^_`]" => "")
	return first(s for s in ShortrefIterator{8}(fname) if !haskey(g, R(s)))
end


"""    Resref{T}(game.resources, string)

Converts a string (entered by the user) to a long reference:
 - if the string is of the form `"/resource"` then use root namespace;
 - if the string is of the form `"namespace/resource"`, parse it;
 - if an object with this reference exists in current namespace, return it;
 - same with root namespace;
 - otherwise, create a fresh reference, associate it with the string, and return it.
"""
function (R::Type{Resref{T}})(g::GameResources, str::AbstractString) where{T}
# function makeref(g::GameResources, ::Type{Resref{T}}, str) where{T}
# end
	isempty(str) && return str|>R
	startswith(str, '/') && return str[2:end]|>lowercase|>R
	# Search for an existing original game resource with this name
	if length(str) ≤ 8
		s1 = lowercase(str)
		haskey(g.key, s1, T) && return R(s1)
	end
	fullpath = lowercase(contains(str, '/') ? str : g.namespace[]*'/'*str)
	ref = get!(g.shortref, fullpath) do
		s = available_resref(g, R, str)
		return s
	end
	return ref|>R
end

#««2 Pseudo-dictionary interface
# All the functions used here encapsulate access to the `GameResources`
# structure as a pseudo-dictionary indexed by `Resref` keys.
# Resources can be stored in three places:
#  - modified resource registry,
#  - override directory,
#  - key/bif archives.

changes(g::GameResources) = g.changes

function Base.haskey(g::GameResources, ref::Resref{T}) where{T}
	haskey(changes(g), ref) ||
	haskey(g.key, ref.name, T) ||
	(haskey(g.override, T) && haskey(g.override[T], ref.name))
end

function open_override(g::GameResources, s::AbstractString, T::Symbol)
	dict = get(g.override, T, nothing); isnothing(dict) && return nothing
	name = get(dict, s, nothing); isnothing(name) && return nothing
	return open(joinpath(g.override_directory, name*'.'*String(T)))
end
function Base.open(g::GameResources, s::AbstractString, T::Symbol;override=true)
	# this returns a plain IO (IOStream or IOBuffer):
	if override
		io = open_override(g, s, T)
		!isnothing(io) && return io
	end
	return open(g.key, s, T)
end

function Base.get(f::Base.Callable, g::GameResources, ref::Resref{T};
		override = true) where{T}
	get(changes(g), ref) do
		io = open(g, ref.name, T)
		isnothing(io) && return f()
		data = read(ResIO{T}(ref, io))
		close(io)
		return data
	end
end
Base.get(g::GameResources, ref, x) = get(()->x, g, ref)
Base.get!(f::Base.Callable, g::GameResources, ref) = get(register! ∘ f, g, ref)
Base.get!(g::GameResources, ref, x) = get!(()->x, g, ref)
Base.getindex(g::GameResources, ref) = get(()->throw(KeyError(ref)), g, ref)

# This performs both conversion of (long) string -> (short) Resref
# and reading of the resource:
Base.getindex(g::GameResources, R::Type{<:Resref}, s::AbstractString)= g[R(g,s)]

function Base.keys(g::GameResources, ::Type{<:Resref{T}}) where{T}
	k = Set{StaticString{8}}()
	for s in keys(changes(g), Resref{T}); push!(k, s.name); end
	for s in keys(get(g.override, T, ())); push!(k, s); end
	for s in keys(g.key, T); push!(k, s); end
	k
end
Base.keys(g::GameResources, T, pat) = (x for x in keys(g, T) if contains(x,pat))

#««2 Commit changes to filesystem
@inline function commit(g::GameResources)
	# Commit modified objects:
	# This takes care of moving all relevant data from `changes` to `override`.
	for dict in alldicts(changes(g))
		println("\e[1mWriting $(length(dict)) modified $(valtype(dict))(s)\e[m")
		for x in values(dict)
			commit(g, x)
		end
	end
end
@inline function commit(g::GameResources, x::RootResource)
	ref = x.ref; T, s = resourcetype(ref), ref.name
	println("  \e[32m$ref => $s.$T\e[m")
	name = uppercase(s)
	write(joinpath(g.override_directory, name*'.'*string(T)), x)
	get!(g.override, T, Auto())[s] = name
	delete!(changes(g), ref)
end
state(g::GameResources) = g.shortref

#««1 Game
#««2 Game structure
"""    Game

Main structure holding all top-level data for a game installation, including:
 - resource repository (three kinds: key/bif, override, memory database of
   modified resources);
 - tlk strings,
 - conversion of resource references to 8-byte short references;
 - dialog-building context.

This structure works as a pseudo-dictionary:
indexing it with keys of a given `Resref` type
returns data structures of the corresponding resource type.

 - `game[resref]`: returns the data structure described by this resource.
 - `get` and `get!` methods, e.g. `get(game, resref) do ... end`
 - `keys(game, type)`: returns a vector of all names of
   existing resources of this type.
"""
@with_kw_noshow struct Game
	directory::String
	strings::GameStrings
	resources::GameResources
	dialog_context::DialogContext = Auto()
end
state_file(::Type{Game}, directory) = joinpath(directory,"state.toml")
"""    Game(directory)

Initializes a `Game` structure from the game directory
(i.e. the directory containing the `"chitin.key"` file).
"""
function Game(directory::AbstractString)
	f = state_file(Game, directory)
	state = f|>isfile ? TOML.parsefile(f) : EmptyDict()
	return Game(; directory,
		strings = GameStrings(joinpath(directory, "lang"),
			get(state, "strings", EmptyDict())),
		resources = GameResources(directory,
			get(state, "resources", EmptyDict())))
end
Base.show(io::IO, g::Game) =
	print(io, "Game<", repr(g.strings), ", ", repr(g.resources), ">")
@inline register!(g::Game, x::RootResource) =
begin
	println("\e[34;1m register!($(x.ref))\e[m")
	# this prevents overwriting if the key already exists:
	get!(changes(g.resources), x.ref, x)
end

# ««2 Namespace and language accessor functions
"""    namespace([game], s)

Sets the current namespace for game resources being defined to `s`.
The following default namespaces are used:
 - `""` for original game resources;
 - `"user"` is the default namespace for added resources.
"""
namespace(g::Game, s::AbstractString) = (g.resources.namespace[] = s; s)
namespace(g::Game) = g.resources.namespace[]

"""    language([game], s)

Sets the current language of the game (i.e. the language in which
strings entered as parameters for functions will be interpreted)
to the one given by string `s`.

Allowed values are:
$(join(["`\""*replace(replace(repr(x[1]), r"(^r\"\^|\"i$|\.\*)"=>""), "*" => "\\*")*"\"`" for x in LANGUAGE_FILES], ", ")).

(Some languages exist in both a male and female version; for simplicity,
these are (at least for now) considered as two independent languages).
"""
function language(g::Game, s::AbstractString)
	for (i, (r, f, _)) in pairs(LANGUAGE_FILES)
		contains(s, r) || continue
		set_language!(g.strings, g.directory[], i)
		return i
	end
	error("unknown language: "*s)
end
language(g::Game) = g.strings.language[]
#««2 Accessors: `item` etc.

# Special case: get only if modified
# modified -> return modified object; existing -> return reference;
# non-existing -> return `nothing`.
# FIXME: see if this could be made un-global by adding a Game field in
# Actor structure?
# Special case: the behavior depends on the `modified` status of the
# object
#  - modified: state is indexed
#  - unmodified: use original state key
#  - not-existing: return 0
function stateindex(g::Game, r::Resref"dlg", key::StateKey)
	a = get(changes(g.resources), r, nothing)
	!isnothing(a) && return a.states[key].position
	@assert haskey(g.resources, r)
	return fieldtype(State, :position)(key.n)
end

"""    item([game], ref)

Returns the item with given reference."""
item(g::Game, name::AbstractString) = g.resources[Resref"itm", name]
"""    items([game], [regex])

Returns an iterator over all items. If a `regex` is provided then
only those items with matching reference are included."""
items(g::Game, r...) = (item(g, n) for n in keys(g, Resref"itm", r...))

#««2 Dialog-building functions
get_actor(g::Game, s::AbstractString) = get_actor(g, Resref"dlg"(s))
get_actor(g::Game, ref::Resref"dlg")= get!(g.resources,ref) do; Actor(;ref) end
get_actor(::Game, a::Actor) = a

"""
    actor([game], "name")

Loads the named actor from game files, or creates an empty actor if
none exists with this name.
"""
actor(g::Game, s::AbstractString) =
	target_actor!(g.dialog_context, get_actor(g,s))
actor(g::Game, s::AbstractString, l::Union{AbstractString,Integer}) =
	actor(g,s).states[StateKey(l)]
actor(g::Game) = target_actor(g.dialog_context)
actor(::Game, a::Actor) = a

StateKey(::Game, s::Integer) = StateKey(s)
StateKey(g::Game, s::AbstractString) =
	(q = contains(s, '/') ? s : namespace(g)*'/'*s; q|>hash|>StateKey)
StateKey(::Game, ::typeof(exit)) = StateKey(zero(UInt))
StateKey(::Game, ::typeof(!isvalid)) = StateKey(!isvalid)
# say: unpacks args into (label, text)
# XXX allow a way to say(actor, label, text)
"""    say({text | (label => text)}*; priority, trigger)

Introduces states of dialog for the current actor.
If the label is omitted, a default (numeric, increasing) label
will be inserted
(although inserting an explicit label makes the state easier to reach).

A single `say` call is equivalent to several successive `say` calls
for the same current actor.

## Special cases

 - implicit, text-less transitions: `say(text1) say(text2)`;
 - multi-say: `say(text1, text2, ...)` — actually equivalent to
   the previous form;
 - chain with actor change: `actor(name1) say(text1) actor(name2) say(text2)...`;
"""
say(g::Game, text::StringKey; kw...) =
	say2(g, g.dialog_context|>target_actor|>firstlabel, text; kw...)
say(g::Game, (label, text)::Pair{<:Any,<:StringKey}; kw...) =
	say2(g, label, text; kw...)
SayText = Union{StringKey,Pair{<:Any,<:StringKey},Pair{<:Any,<:Pair}}
say(g::Game, args::SayText...; kw...) = for a in args; say(g, a; kw...); end

# say2(game, label, text): calls low-level
say2(g::Game, label, text::StringKey; kw...) =
	say2(g, (g.dialog_context|>target_actor, label), text; kw...)
say2(g::Game, (a, label)::Tuple{<:Any,<:Any}, text::StringKey; kw...) =
	add_state!(g.dialog_context, actor(g, a),
		StateKey(g, label), Strref(g.strings, text); kw...)
"""    from([game], [actor], label)

Sets current state to `actor`, `label`. The state must exist.
"""
function from(g::Game, label) 
	isempty(g.dialog_context.source_actor.ref) &&
		error("from(label) when no actor is loaded; use from(actor, label)")
	set_source!(g.dialog_context, StateKey(g, label))
end
from(g::Game, actor, label) =
	set_source!(g.dialog_context, get_actor(g, actor), StateKey(g, label))
# interject: unpack args into (actor, text)
"""
    interject({text | (label => text)}*; priority, trigger)

Inserts text inside existing dialog. The new state(s) are inserted just after
the current state, using tail insertions. """
interject(g::Game, text::StringKey; kw...) =
	interject2(g, g.dialog_context|>target_actor, text; kw...)
interject(g::Game, (actor, text)::Pair{<:AbstractString,<:StringKey}; kw...) =
	interject2(g, actor, text; kw...)
InterjText = Union{StringKey,Pair{<:AbstractString,<:StringKey}}
# XXX insert here the variable guard if needed
# XXX what do do with the trigger? does it apply to only the first state,
# or to all of them? (Cf. WeiDU)
# XXX see whether to move actions (should be easy: this is transitive)
# XXX enable giving an actor label
interject(g::Game, args::InterjText...; kw...) =
	for a in args; interject(g, a; kw...); end

# interject2(actor, text): calls low-level
function interject2(g::Game, actor, text; kw...)
	t_actor = get_actor(g, actor)
	t_key = firstlabel(actor)|>StateKey
	tail_insert!(g.dialog_context, t_actor, t_key, Strref(g.strings, text); kw...)
end

# reply: unpacks args into (text, target)
"""
    reply(text => label)

Introduces a state transition (player reply) pointing to the given label.
The label may be one of:
 - `("actor", state)` (equivalently `"actor" => state`);
 - `state`  (uses current target actor);
 - `exit` (creates a final transition).

State may be either numeric (referring to the base game's states) or string.
In the latter case, if it does not contain a slash,
it will be prefixed by the current namespace : `"namespace/state"`.
This prevents states from different namespaces from interfering.


## Special forms:

 - `reply(exit)` creates a text-less, final transition;
 - `reply(text)` creates a pending transition: this will be connected to the
   next state inserted (via `say`).

## Examples:
    # chain to other actor:
    reply("Say Hi to Hull" => "hull" => 0)
    # connect pending transition:
    say("How do you do?") reply("Fine!") say("Let'sa go!") reply(exit)
"""
reply(g::Game, (text, target)::Pair; kw...) = reply2(g, text, target; kw...)
reply(g::Game, ::typeof(exit); kw...) = reply2(g, nothing, exit; kw...)
# Special case: reply("text") inserts a pending transition
reply(g::Game, text::StringKey; kw...) = reply3(g, text, "", !isvalid;kw...)

# reply2(game, text, target): unpacks target into (actor, label)
reply2(g::Game, text, target::Union{Tuple,Pair}; kw...) =
	reply3(g, text, get_actor(g, target[1]), target[2]; kw...)
reply2(g::Game, text, label; kw...) =
	reply3(g, text, target_actor(g.dialog_context), label; kw...)
reply2(g::Game, text, ::typeof(exit); kw...) =
	reply3(g, text, NO_ACTOR, 0; terminates = true, kw...)

# reply3(game, text, target_actor, target_label): calls low-level
reply3(g::Game, text, actor, label; kw...) = add_transition!(g.dialog_context,
	Strref(g.strings,text), get_actor(g, actor).ref, StateKey(g, label); kw...)
"""
    trigger(string)

Attaches a trigger to the **next** transition or state."""
trigger(g::Game, s::AbstractString) = trigger!(g.dialog_context, s)
"""
    action(string)

Attaches an action to the latest transition."""
action(g::Game, s::AbstractString; kw...) =
	add_action!(current_transition(g.dialog_context), s; kw...)
"""
    journal(string)

Attaches a journal entry to the latest transition."""
journal(g::Game, s::AbstractString; kw...) =
	add_journal!(current_transition(g.dialog_context), s; kw...)

#««2 Saving game data
"""    commit([game])

Saves all changed game data to the disk installation."""
function commit(g::Game)
	commit(g.resources)
	commit(g.strings)
	# XXX try to make this atomic by saving to temporary files and then
	# moving all the files at the last minute
	open(state_file(Game, g.directory), "w") do io
		TOML.print(io, Dict(
			"resources" => state(g.resources),
			"strings" => state(g.strings),
		))
	end
	print("\e[31m"); run(`cat $(state_file(Game, g.directory))`)
	print("\e[32m"); run(`ls $(g.resources.override_directory)`); print("\e[m")
end
#««1 Item/etc. copying
args_to_kw(x, args...) =
	error("no conversion from args to properties for type $(typeof(x))")
	# return a NamedTuple
"""    getkey(key, dict1, dict2, ..., default)

Chained version of `get`."""
@inline getkey(k, x1, default) = get(x1, k, default)
@inline getkey(k, x1, y, z...) = get(x1, k) do; getkey(k, y, z...) end
@inline getkey(f::Function, k, x1) = get(f, x1, k)
@inline getkey(f::Function, k, x1, y...) = get(x1, k) do; getkey(f, k, y...) end

function Base.copy(g::Game, x::T, args...; kwargs...) where{T<:RootResource}
	kw2 = args_to_kw(x, args...)::NamedTuple
	println("got kw2=$kw2")
	vars = (getkey(fn, kwargs, kw2, getfield(x, fn)) for fn in fieldnames(T))
	y = T(vars...)
	y.ref = getkey(:ref, kwargs, kw2) do
		# Make a new reference for the copied object by using the name,
		# possibly appending a suffix to make it unique. This operation also
		# 1. needs `Game` access (to check unicity);
		# 2. triggers the needed `register!` call:
		r = namespace(g)*'/'* replace(g.strings[y.name], r"[^a-zA-Z0-9]" => "")
		i = 1
		while haskey(g.resources, oftype(x.ref, r))
			r = replace(r, r"~\d*$" => "")*'~'*string(i)
			i+= 1
		end
		r
	end
	return y
end
Base.copy(g::Game, r::Resref, args...; kwargs...) =
	copy(g, g[r], args...; kwargs...)

const _resource_template = Dict{SymbolicEnums.SymbolicNames,Resref}(
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
# wakizashi, ninjato XXX

(T::SymbolicEnums.SymbolicNames)(args...; kwargs...) =
	copy(_resource_template[T], args...; kwargs...)
#««1 Global `game` object
const global_game = Ref{Game}()
game() = global_game[]
init!(directory::AbstractString) = global_game[] = Game(directory)

# XXX fix this: include a Game field in ResIO?
str(s::Strref) = game().strings[s]
changes() = changes(game().resources)
load_translations!(dir::AbstractString...) =
	load_translations!(game().strings, dir...)

# For all methods of those functions starting with a `::Game` argument:
# define a corresponding method where the global `game` is used.
for f in (register!, commit,
		language, namespace,
		item, items,
		actor, say, reply, action, trigger, journal, from, stateindex, interject),
		m in methods(f)
	argt = m.sig.parameters
	(length(argt) ≥ 2 && argt[2] == Game) || continue
	argn = ccall(:jl_uncompress_argnames, Vector{Symbol}, (Any,), m.slot_syms)
	z = zip(argn[3:end], argt[3:end])
	lhs = (:($n::$t) for (n,t) in z)
	rhs = Any[n for (n,t) in z]
	m.isva && (rhs[end] = Expr(:(...), rhs[end]))
	@eval $(nameof(f))($(lhs...)) = $f(game(), $(rhs...))
end
Base.convert(::Type{Strref}, s) = Strref(game().strings, s)
Base.convert(::Type{Strref}, s::Strref) = s # method conflict fix
Base.convert(T::Type{<:Resref}, s::AbstractString) = T(game().resources, s)
Base.convert(T::Type{<:Resref}, x::RootResource) = x.ref
Base.copy(x::Union{Resref,RootResource}, args...; kwargs...) =
	copy(game(), x, args...; kwargs...)

# debug
function extract(f::AbstractString, g::AbstractString = f)
	name, type = rsplit(f, '.'; limit=2)
	isdir(g) && (g = joinpath(g, f))
	buf = get(game().key, name, Symbol(type))
	write(g, buf)
end
#»»1

export commit, namespace, language
export item, items
export actor, say, reply, journal, action, trigger, from, interject
export @__str # re-export from MarkedStrings
@eval export $(names(DiceThrows)...) # re-export d6 etc.
end # module
