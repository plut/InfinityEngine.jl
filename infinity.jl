#=
# TODO:
# - check translation format
#  - needs xgettext (or at least a very basic version)
# + define `Game` structure holding everything at top level
#  + override (map type -> Set of ids)
#  - languages ?
#   - use one master language (typ. en_US) for determining when to add
#     strings, then compile strings for all languages with same strrefs
#     (obviously!)
# - check forbidden NTFS chars to choose a prefix: <>:"/\|?*
# - `.d` => julia syntactic transformation
# - nice display methods for items, creatures, dialog etc.
=#

module InfinityEngine

module Blocks
using Printf
using StaticArrays
abstract type AbstractBlock end

reprb(x) = repr(x)
reprb(x::Base.CodeUnits) = 'b'*repr(String(x))

"""    @block BlockName begin
      field1::type1 # ...
      b"constant1" # ...
    end

Defines a data type `BlockName` as well as methods for `Base.read` and `Base.write` for binary I/O of this type respecting the layout.
The fields may be either:
 - `field::type`: (with defined length)
 - `b"constant"`: a marker with defined size, this is checked on reading, but does not appear as a field in the data type
 - **(TODO)** `field[pos]::type`: only a part of the field is read/written here; the next time this field appears the type should not be given.

"""
macro block(name, fields)
# TODO:
	type_code = Expr[]
	read_vars = Symbol[]
	read_code = Expr[]
	show_code = Expr[]
	field_frag = Dict{Symbol,BitVector}()
	size_code = Expr[]
	instantiate_code = Expr[]
# 	description = ""
	if Meta.isexpr(name, :curly)
		typeparams = name.args[2:end]
		escname = :($(esc(name.args[1])){$(typeparams...)})
	elseif Meta.isexpr(name, :where)
		typeparams = name.args[2:end]
		escname = :($(esc(name.args[1].args[1])){$(name.args[1].args[2:end]...)})
	else
		typeparams = ()
		escname = esc(name)
	end
# 	typeparams = Meta.isexpr(name, :curly) ? name.args[2:end] : ()
# 	escname = Meta.isexpr(name, :curly) ?
# 		Expr(:curly, esc(name.args[1]), typeparams...) : esc(name)
	for expr in fields.args
		expr isa LineNumberNode && continue
		if Meta.isexpr(expr, :ref)
			error("not implemented")
# 			field, r = expr.args[1], Core.eval(__module__, expr.args[2])
# 			if Meta.isexpr(field, :(::))
# 				field, content = expr.args[1], Core.eval(__module__, field.args[2])
# 				@assert !haskey(field_frag, field)
# 				field_frag[field] = falses(sizeof(content))
# 				push!(type_code, :($field::$(esc(content))))
# 				push!(read_vars, v)
# 			end
# 			r1, r2 = first(r), last(r)
# 			@assert r1 ≥ 1; @assert r2 ≤ sizeof(content)
# 			@assert !any(view(field_frag[field], r1:r2))
# 			field_frag[field][r1:r2].= true
# 			v = Symbol(string(field)*string(r1))
# 			if all(field_frag)
# 			end
		elseif Meta.isexpr(expr, :(::))
			field, content = expr.args[1], expr.args[2]
			if content ∈ typeparams
				push!(size_code, :(sizeof($content)))
			else
				content = esc(content)
				push!(size_code, :(sizeof($content)))
			end
			push!(type_code, :($field::$content))
			v = field; push!(read_vars, v)
			push!(read_code, :($v = read(io, $content)),
				:(debug && println($(string(name)), "/",  $(string(field)), ": read ",$(sizeof(content)),": ",$v)))
		else
			field, content = reprb(expr), eval(expr)
			push!(read_code, :(@assert read(io, $(sizeof(content))) == $content))
		end
		push!(show_code, quote
		s = try sizeof($content); catch; end
		if !isnothing(s)
		@printf(io, "0x%04x %4d %s\n", offset, s, $(string(field)))
		offset+= s
		else
		@printf(io, "<-- type %s::%s does not have a definite type\n",
			$(string(field)), string($content))
		end
		end)
	end
	block = quote
		Base.@__doc__(struct $escname <: AbstractBlock
			$(type_code...)
		end)
		# this hook allows overriding the kwargs method:
		@inline $escname(;kwargs...) where{$(typeparams...)} =
			$escname(AbstractBlock; kwargs...)
		function Base.show(io::IO, ::MIME"text/plain", ::Type{<:$escname}
				) where{$(typeparams...)}
			offset = 0
			$(show_code...)
			print(io, "total size is ", offset)
			return offset
		end
		$escname(::Type{AbstractBlock};$(read_vars...)) where{$(typeparams...)} =
			$escname($(read_vars...))
		# We extend filesize() to hold the size of the file representation of
		# this struct:
		@inline Base.filesize(::Type{$escname}) where{$(typeparams...)} =
			+($(size_code...))
		function Base.read(io::IO, ::Type{$escname}; debug=false) where{$(typeparams...)}
			$(read_code...)
			return $escname(;$(read_vars...))
		end
	end
	block.head = :toplevel
	return block
end


# @inline readt(io::IO, T::DataType) = only(reinterpret(T,read(io, sizeof(T))))
@inline Base.read(io::IO, T::Type{<:AbstractBlock}, n::Integer) =
	[ read(io, T) for _ in Base.OneTo(n) ]
end
using .Blocks: @block

module Functors
"""    find_typevar(T, TX)

Given a type expression `T` with (exactly) one free variable `V`
and a concrete subtype `TX`, returns the type with with this variable
is instantiated in `TX`, i.e. `X` such that `TX == T{X}`.
"""
function find_typevar(T::DataType, TX::DataType)
	@assert T.hasfreetypevars
	for (p1, p2) in zip(T.parameters, TX.parameters)
		p1 isa TypeVar && return (p1, p2)
		p1.hasfreetypevars && return find_typevar(p1, p2)
	end
end

"""    replacetypevars(f, T, V, A, B, x)

Given a type expression `T` with free variable `V`
and a concrete instance `x` of `T{A}`,
as well as a function `f` mapping `A` to `B`,
returns the instance of `T{B}` constructed by mapping `f`
to the appropriate fields of `x`.
"""
function replacetypevars(f, T, V, A, B, x::TX) where{TX}
# 	println("replacetypevars: $T, $TX;  $V = $A => $B")
	# two leaf cases:
	T isa TypeVar && return f(x)::B
	!T.hasfreetypevars && return x
	TX <: AbstractArray &&
	begin
		newt = UnionAll(V,T){B}
# 		println("  V=$V T=$T UnionAll=$(UnionAll(V,T))")
# 		println("new type should be ", newt)
		z = [ replacetypevars(f, T.parameters[1], V, A, B, y) for y in x ]
# 		println("changing to: ", (typeof(z), z))
		return [ replacetypevars(f, T.parameters[1], V, A, B, y) for y in x ]
	end
# 	if isstructtype(T)
# 		TB = UnionAll(V,T){B}
# 		vals = ((replacetypevars(f, fieldtype(T,i), V, A, B, getfield(x,i))
# 			for i in 1:fieldcount(T))...,)
# 		Expr(:new, :TB, Expr(Symbol("..."), :vals))
# 	end
	isstructtype(T) && return UnionAll(V, T){B}(
			(replacetypevars(f, fieldtype(T,i), V, A, B, getfield(x,i))
			for i in 1:fieldcount(T))...,)
	error("unreachable")
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
	(V, A) = find_typevar(T.body, TX)
	@assert TX == T{A}
	isnothing(B) && (B = only(Base.return_types(f, Tuple{A})))
	replacetypevars(f, T.body, V, A, B, x)
end
end
include("dottedenums.jl"); using .DottedEnums

import Base.read
using Printf
using StaticArrays

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
@inline Base.codeunit(s::StaticString, i) = s.char[i]
@inline Base.codeunit(::StaticString) = UInt8
# We handle only ASCII strings...
@inline Base.isvalid(::StaticString, ::Integer) = true
@inline function Base.iterate(s::StaticString, i::Integer = 1)
	i > length(s) && return nothing
	c = s.chars[i]
	iszero(c) && return nothing
	(Char(c), i+1)
end
@inline function (T::Type{<:StaticString{N}})(s::AbstractString) where{N}
	@assert length(s) ≤ N "string must be at most $N characters long: \"$s\""
	T(codeunits(rpad(uppercase(s), N, '\0')))
end
@inline read(io::IO, T::Type{StaticString{N}}) where{N} = T(read(io, N))

# ««2 Type wrapper
struct Typewrap{T,S}
	data::T
	@inline (::Type{Typewrap{T,S}})(args...) where{T,S} = new{T,S}(T(args...))
end
@inline read(io::IO, X::Type{<:Typewrap{T}}) where{T,S} = X(read(io, T))
@inline (::Type{Typewrap{T,S}})(x::Typewrap{T,S}) where{T,S} = x
@inline Base.show(io::IO, t::Typewrap{T,S}) where{T,S} =
	(print(io, S, '('); show(io, t.data); print(io, ')'))
@inline (J::Type{<:Union{Integer,AbstractString}})(x::Typewrap) = J(x.data)

# ««2 Indices: Strref etc.
"""    Strref

Index (32-bit) referring to a translated string in dialog.tlk/dialogF.tlk.
"""
const Strref = Typewrap{UInt32, :Strref}

"""    Resource"Type"

Index (64-bit, or 8 char string) referring to a resource.
This index carries type information marking the resource type,
and indicated by the string parameter: Resource"ITM"("BLUN01")
describes an item, etc.
This allows dispatch to be done correctly at compile-time.
"""
const Resource{T} =Typewrap{Typewrap{StaticString{8},T},:Resource}
macro Resource_str(s) Resource{Symbol(uppercase(s))} end

"""    BifIndex
32-bit index of resource in bif files.
"""
const BifIndex = Typewrap{UInt32, :BifIndex} # bif indexing
@inline sourcefile(r::BifIndex) = Int32(r) >> 20
@inline tilesetindex(r::BifIndex) = (Int32(r) >> 14) && 0x3f
@inline resourceindex(r::BifIndex) = Int32(r) & 0x3fff

"""    Resindex

16-bit value indexing a resource type in key file.
(This is immediately translated to a string value when reading this file).
"""
const Resindex = Typewrap{UInt16, :Resindex} # 16-bit version
# ««2 File type determination

# Restype type is a compile-time correspondence between UInt16 and strings
# This is stored as:
#  - UInt16 in key file and
#  - strings in source code and filesystem files
#
# Use cases:
#  - locate resource by string name in KeyIndex: source = string =>KeyIndex
#  - build Key index from .key file: int=>KeyIndex
#  - read from file: source = string => string

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
@inline String(x::Resindex) = get(RESOURCE_TABLE, UInt16(x), repr(UInt16(x)))
@inline Base.Symbol(x::Resindex) = Symbol(String(x))
"""    Restype{Type}

A structure holding an IO stream for game data (either from a filesystem
file or from a bif resource) together with the resource name and type
identifier.

 - Since the file type determines the `read`/`write` methods (and only takes
   a finite set of values) it is a type parameter.
 - The resource name (i.e. basename of the file without extension) is stored
   as a field. The name is canonicalized as upper-case.

Defined methods include:
 - `Restype"EXT"`: macro defining static value for this file type.
 - `Restype{T}(filename)`: constructor opening the file.
 - `Restype(filename)`: same as above with automatic file type determination.
 - `Restype{T}(filename, io)`: constructor for existing IO.
 - `Base.read(Restype)`: reads to appropriate structure type.

Constructors:
"""
mutable struct Restype{T,IO}
	name::String
	io::IO
	@inline Restype{T}(name::AbstractString, io::IO) where{T,IO} =
		finalizer(x->close(x.io), new{T,IO}(uppercase(name),io))
end
@inline Restype{T}(f::AbstractString) where{T} =
	((a,b) = splitext(basename(f)); Restype{T}(a, open(f)))
@inline Restype(f::AbstractString) = 
	((a,b) = splitext(basename(f)); Restype{Symbol(uppercase(b[2:end]))}(a, open(f)))
macro Restype_str(s) Restype{Symbol(uppercase(s))} end

# ««1 tlk
struct TlkStrings{X}
	strings::Vector{X}
	index::Dict{String,UInt32}
	@inline TlkStrings(str::AbstractVector{X}) where{X} = new{X}(str, Dict())
end

function Base.getindex(f::TlkStrings{X}, i::Strref) where{X}
	i = UInt32(i)
	i == typemax(i) && (i = 0)
	f.strings[i+1]::X
end

"""    instantiate(str::TlkStrings, x)

Instantiates all `Strref` fields as strings in this object, using `str`
as a string index.
"""
function instantiate(x::TlkStrings)
end

@block TLK_hdr begin
	b"TLK V1  "
	lang::UInt16
	nstr::UInt32
	offset::UInt32
end
@block TLK_str begin
	flags::UInt16
	sound::Resource"WAV"
	volume::UInt32
	pitch::UInt32
	offset::UInt32
	length::UInt32
end

function read(f::Restype"TLK")
	header = read(f.io, TLK_hdr)
	strref = read(f.io, TLK_str, header.nstr)
	strings = [ (string = string0(f.io, header.offset + s.offset, s.length),
		flags = s.flags, sound = s.sound, volume = s.volume, pitch = s.pitch)
		for s in strref ]
	return TlkStrings(strings)
end
struct TlkStringSet{X}
	strings1::TlkStrings{X}
	strings2::TlkStrings{X}
end

# ««1 key/bif
@block KEY_hdr begin
	b"KEY V1  "
	nbif::Int32
	nres::Int32
	bifoffset::UInt32
	resoffset::UInt32
end
@block KEY_bif begin
	filelength::UInt32
	offset::UInt32
	namelength::UInt16
	location::UInt16
end
@block KEY_res begin
	name::StaticString{8}
	type::Resindex
	location::BifIndex
end

struct KeyIndex{X}
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
	header = read(io, KEY_hdr)
	seek(io, header.bifoffset)
	bifentries = read(io, KEY_bif, header.nbif)
	bifnames = [ string0(io, x.offset, x.namelength) for x in bifentries ]
	seek(io, header.resoffset)
	resentries = read(io, KEY_res, header.nres)
	return KeyIndex(dir, bifnames, resentries)
end

const XOR_KEY = b"\x88\xa8\x8f\xba\x8a\xd3\xb9\xf5\xed\xb1\xcf\xea\xaa\xe4\xb5\xfb\xeb\x82\xf9\x90\xca\xc9\xb5\xe7\xdc\x8e\xb7\xac\xee\xf7\xe0\xca\x8e\xea\xca\x80\xce\xc5\xad\xb7\xc4\xd0\x84\x93\xd5\xf0\xeb\xc8\xb4\x9d\xcc\xaf\xa5\x95\xba\x99\x87\xd2\x9d\xe3\x91\xba\x90\xca"

function decrypt(io::IO)
	peek(io) != 0xff && return io
	buf = collect(codeunits(read(io, String)))
	for i in eachindex(buf)
		buf[i] ⊻= XOR_KEY[mod1(i-2, length(XOR_KEY))]
	end
	return IOBuffer(buf[2:end])
end
@inline decrypt(::Val, io::IO) = io
@inline decrypt(::Union{Val{Symbol("2DA")},Val{:IDS}}, io::IO) = decrypt(io)

@block BIF_hdr begin
	b"BIFFV1  "
	nres::UInt32
	ntilesets::UInt32
	offset::UInt32
end

@block BIF_resource begin
	locator::BifIndex
	offset::UInt32
	size::UInt32
	type::Resindex
	unknown::UInt16
end

@block BIF_tileset begin
	locator::BifIndex
	offset::UInt32
	ntiles::UInt32
	size::UInt32
	UInt16(0x03eb)
	unknown::UInt16
end

bifcontent(file::AbstractString, index::Integer) = open(file, "r") do io
	header = read(io, BIF_hdr)
	seek(io, header.offset)
	resources = read(io, BIF_resource, header.nres)
	IOBuffer(read(seek(io, resources[index+1].offset), resources[index+1].size))
end

function Restype{T}(key::KeyIndex, name) where{T}
	loc = get(key.location, (StaticString{8}(name), T), nothing)
	isnothing(loc) && return nothing
	bif = joinpath(key.directory, key.bif[1+sourcefile(loc)])
	io = bifcontent(bif, resourceindex(loc))
	return Restype{T}(String(name), decrypt(Val{T}(), io))
end
Base.all(key::KeyIndex, ::Type{Restype{T}}) where{T} =
	(Resource{T}(r[1]) for r in keys(key.location) if r[2] == T)

# ««1 ids
# useful ones: PROJECTL SONGLIST ITEMCAT NPC ANISND ?
function read(f::Restype"IDS"; debug=false)
	debug && (mark(f.io); println("(", read(f.io, String), ")"); reset(f.io))
	line = readline(f.io)
	(!contains(line, " ") || startswith(line, "IDS")) && (line = readline(f.io))
	!isnothing(match(r"^[0-9]*$", line)) && (line = readline(f.io))
	list = Pair{Int,String}[]
	while true
		line = replace(line, r"\s*$" => "")
		if !isempty(line)
			s = split(line, r"\s+"; limit=2)
			length(s) ≤ 1 && error("could not split IDS line: $line")
			push!(list, (parse(Int, s[1]) => s[2]))
		end
		eof(f.io) && break
		line = readline(f.io)
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
function read(f::Restype"2DA"; debug=false, aligned=false)
	debug && (mark(f.io); println("(", read(f.io, String), ")"); reset(f.io))
	line = readline(f.io)
# 	@assert contains(line, r"^\s*2da\s+v1.0"i) "Bad 2da first line: '$line'"
	defaultvalue = replace(replace(readline(f.io), r"^\s*" => ""), r"\s*$" => "")
	line = readline(f.io)
	if !aligned
		cols = split(line)
		lines = readlines(f.io)
		mat = split.(lines)
		MatrixWithHeaders(first.(mat), cols,
			[m[j+1] for m in mat, j in eachindex(cols)])
	else
		positions = [ i for i in 2:length(line)
			if isspace(line[i-1]) && !isspace(line[i]) ]
		cols = [ match(r"^\S+", line[i:end]).match for i in positions ]
		lines = readlines(f.io)
		mat = [ match(r"^\S+", line[i:end]).match for line in lines, i in positions]
		MatrixWithHeaders([match(r"^\S+", line).match for line in lines], cols,mat)
	end
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
const ItemAnimation = Typewrap{StaticString{2},:ItemAnimation}
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
@block ITM_hdr{S} begin
	b"ITM V1  "
	unidentified_name::S
	identified_name::S
	replacement::Resource"ITM"
	flags::ItemFlag
	item_type::ItemCat
	usability::UsabilityFlags
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
	weight::UInt32
	unidentified_description::S
	identified_description::S
	description_icon::Resource"BAM"
	enchantment::UInt32
	ext_header_offset::UInt32
	ext_header_count::UInt16
	feature_offset::UInt32
	feature_index::UInt16
	feature_count::UInt16
end
@block ITM_ability begin
	attack_type::UInt8
	must_identify::UInt8
	location::UInt8
	alternative_dice_sides::UInt8
	use_icon::Resource"BAM"
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
# ««2 Item function
function read(f::Restype"ITM")
	read(f.io, ITM_hdr{Strref})
end
# ««1 dlg
@dottedflags DialogTransitionFlags::UInt32 begin
	HasText
	HasTrigger
	HasAction
	TerminatesDialog
	Interrupt
	AddUnsolvedQuest
	AddJournalNote
	AddSolvedQuest
	ImmediateScriptActions
	ClearActions
end
@block DLG_hdr begin
	b"DLG V1.0"
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
end
@block DLG_state{S} begin
	text::S
	first_transition::Int32
	number_transitions::Int32
	trigger::UInt32
end
@block DLG_transition{S} begin
	flags::DialogTransitionFlags
	player_text::S
	journal_text::S
	index_trigger::Int32
	index_action::Int32
	next_actor::Resource"DLG"
	next_state::Int32
end
# this is the same type for state trigger, transition trigger, actions:
@block DLG_string begin
	offset::UInt32 # of trigger string
	length::UInt32 # idem
end

"""    Dialog{S}

Describes the state machine associated with a dialog.
`S` is the type used for dialog strings (either `Strref` or `String`).
"""
struct Dialog{S}
	self::Resource"DLG"
	flags::UInt32
	states::Vector{DLG_state{S}}
	transitions::Vector{DLG_transition{S}}
	state_triggers::Vector{String}
	transition_triggers::Vector{String}
	actions::Vector{String}
end

@inline dialog_strings(io::IO, offset, count)= [string0(io, s.offset, s.length)
	for s in read(seek(io, offset), DLG_string, count)]

function read(f::Restype"DLG")
	header = read(f.io, DLG_hdr)
	return Dialog{Strref}(Resource"DLG"(basename(f.name)), header.flags,
		read(seek(f.io, header.offset_states), DLG_state{Strref},
			header.number_states),
		read(seek(f.io, header.offset_transitions), DLG_transition{Strref},
			header.number_transitions),
		dialog_strings(f.io, header.offset_state_triggers,
			header.number_state_triggers),
		dialog_strings(f.io, header.offset_transition_triggers,
			header.number_transition_triggers),
		dialog_strings(f.io, header.offset_actions, header.number_actions))
end

function printtransition(dialog::Dialog, str::TlkStrings, i, doneactions)
	t = dialog.transitions[i+1]
	print("  $i",
# 				t.flags
# 				&~DialogTransitionFlags.HasText
# 				&~DialogTransitionFlags.TerminatesDialog
		)
	if t.flags ∋ DialogTransitionFlags.TerminatesDialog
		print(" (final)")
	else
		print(" =>")
		t.next_actor ≠ dialog.self && print(" ", t.next_actor)
		print(" <$(t.next_state)>")
	end
	if t.flags ∋ DialogTransitionFlags.HasText
		print('"', str[t.player_text].string, '"')
	else
		print("(no text)")
	end
	println()
	if t.flags ∋ DialogTransitionFlags.HasAction
		a = t.index_action
		println(" action $a:")
		if a ∉ doneactions
			push!(doneactions, a)
			println(dialog.actions[t.index_action+1])
		end
	end
end

function printdialog(dialog::Dialog{Strref}, str::TlkStrings)
	donetransitions = Set{Int}()
	doneactions = Set{Int}()
	println("self is $(dialog.self)")
	for (i,s) in pairs(dialog.states)
		t = s.trigger
		println("state <$(i-1)>")
		tr = get(dialog.state_triggers, t, nothing)
		if !isnothing(tr)
			println("  trigger: $tr")
		end
		println('"', str[s.text].string, '"')
		trans = s.first_transition:s.first_transition+s.number_transitions-1
		println("  transitions: $trans")
		for i in trans
			i ∈ donetransitions && continue; push!(donetransitions, i)
			printtransition(dialog, str, i, doneactions)
		end
	end
end
#««1 Game
# shorthand: strings*dialog instantiates Strrefs
for T in (:Dialog,:ITM_hdr)
	@eval @inline Base.:*(str::TlkStrings, x::$T{Strref}) =
		Functors.functor(x->str[x].string::String, $T, x)
end
"""    Game

Main structure holding all top-level data for a game installation, including:
 - key/bif archived files,
 - tlk strings,
 - table of override files.
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
	for f in readdir(joinpath(directory, "override"))
		(n, e) = splitext(uppercase(basename(f))); t = Symbol(e[2:end])
		push!(get!(override, t, Set{String}()), n)
	end
	println("read $(sum(length(v) for (_,v) in override)) override resources")
	return Game(directory, key, override)
end
" returns 2 if override, 1 if key/bif, 0 if not existing."
function filetype(game::Game,::Type{Restype{T}}, name::AbstractString) where{T}
	haskey(game.override, T) && uppercase(String(name)) ∈ game.override[T] &&
		return 2
	haskey(game.key.location, (StaticString{8}(name), T)) && return 1
	return 0
end
function Restype{T}(game::Game, name::AbstractString) where{T}
	t = filetype(game, Restype{T}, name)
	if t == 2
		file = joinpath(game.directory, "override", String(name)*'.'*String(T))
		return Restype{T}(file)
	elseif t == 1
		return Restype{T}(game.key, name)
	else
		error("resource not found: $name.$T")
	end
end

# allow strong typing to occur on the index side instead of the read side:
@inline Restype(k::Union{Game,KeyIndex}, name::Resource{T}) where{T} =
	Restype{T}(k, name.data.data)
@inline read(k::Union{Game,KeyIndex}, name::Resource; kw...) =
	read(Restype(k, name); kw...)
@inline filetype(game::Game, name::Resource{T}) where{T} =
	filetype(game, Restype{T}, name)


# »»1
end
IE=InfinityEngine
x = [1,2,3]
y = IE.Functors.functor(string, Vector, x)

game = IE.Game("../bg2/game")
# key=IE.KeyIndex("../bg2/game/chitin.key")
str=read(IE.Restype("../bg2/game/lang/fr_FR/dialog.tlk"))
# dia=read(key["abaziga", "dlg"])
# dia1=str*dia
# IE.printdialog(dia,str)
nothing
