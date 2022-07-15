#=
Generic definition of a header:
 - field = type | constant (b"..") | hash table

 -  if type, this is a r/w value
     (including user types e.g. Strref, Resref etc.)
   if constant, this is a ro value (and written back in file)
   if hash table, this is displayed as the associated value
   (and read in the same manner)
	
=#
module InfinityEngine

module Files
using Printf

"""
    Format{F,T}

 - `F` is the actual contents (as a `NamedTuple`).
 - `T` is the set of types used for building, including pseudo-types.
"""
struct Format{F,T}
	fields::F
end

# FIXME: replace this by a @Format{...} macro returning an anonymous struct
# (with the same fields as the corresponding NamedTuple)
function Format(;kwargs...)
	names, types, kv = Symbol[], DataType[], []
	for (k,v) in pairs(kwargs)
		if v isa DataType
			push!(names, k)
			push!(types, v)
			push!(kv, Tuple{k,v})
		elseif v isa Base.CodeUnits
			push!(kv, (k, tuple(v...)))
		end
	end
	return Format{NamedTuple{tuple(names...),Tuple{types...}},Tuple{kv...}}
end

function Base.show(io::IO, ::MIME"text/plain", X::Type{Format{F,T}}) where{F,T}
	o = Int32(0)
	println(io, "Format type with fields:")
	for kv in T.parameters
		if kv isa DataType
			(n, t) = kv.parameters
			@printf(io, "0x%04x %4d %s\n", o, sizeof(t), n)
			o+= sizeof(t)
		else
			(n, t) = kv
			@printf(io, "0x%04x %4d =\"%s\"\n", o, sizeof(t), String([t...]))
			o+= sizeof(t)
		end
	end
	print(io, "total size = $o")
end

@inline readt(io::IO, T::DataType) = only(reinterpret(T,read(io, sizeof(T))))

@generated function Base.read(io::IO, X::Type{Format{F,T}}) where{F,T}
	vars = Symbol[]
	expr = :(begin end)
	for x in T.parameters
		if x isa DataType
			t = x.parameters[2]; v = gensym(); push!(vars, v)
			push!(expr.args, :($v = readt(io, $t)))
		else
			push!(expr.args, :(@assert readt(io, $(typeof(x[2]))) == $(x[2])))
		end
	end
	push!(expr.args, :(return ($F(($(vars...),)))))
	return expr
end
@inline Base.read(io::IO, T::Type{<:Format}, n::Integer) =
	[ read(io, T) for _ in Base.OneTo(n) ]

end

# ««1 Basic types
# ««2 Constant-length string
using StaticArrays
Bytes{N} = SVector{N,UInt8}

# zero-terminated string (chop trailing zeroes as needed)
@inline function string0(v::AbstractVector{UInt8})
	l = findfirst(iszero, v)
	String(isnothing(l) ? v : view(v, 1:l-1))
end
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))

# ««2 Indices: Strref, Resref
# String reference
primitive type Strref 32 end
@inline Strref(x::Integer) = reinterpret(Strref,UInt32(x))
# Base.convert(::Type{Int32}, x::Strref) = reinterpret(Int32, x)
@inline (T::Type{<:Union{UInt32,Int32}})(x::Strref) = reinterpret(T,x)

# Resource reference (as a name)
struct Resref
	x::SVector{8,UInt8}
end
@inline Resref(s::AbstractString) = 
	only(reinterpret(Resref,codeunits(rpad(uppercase(s), 8, Char(0)))))
macro res_str(s) Resref(s) end
@inline Base.show(io::IO, r::Resref) = print(io, string0(r.x))

# Resource locator (bif indexing)
primitive type Resloc 32 end
@inline (T::Type{<:Union{UInt32,Int32}})(x::Resloc) = reinterpret(T,x)
@inline sourcefile(r::Resloc) = Int32(r) >> 20
@inline tilesetindex(r::Resloc) = (Int32(r) >> 14) && 0x3f
@inline resourceindex(r::Resloc) = Int32(r) & 0x3fff

# Resource type (bif indexing)
primitive type Restype 16 end
@inline Restype(x::Integer) = reinterpret(Restype,UInt16(x))
const RESTYPES = [#««
	0x0001 => "BMP",
	0x0002 => "MVE",
	0x0004 => "WAV",
	0x0006 => "PLT",
	0x03E8 => "BAM",
	0x03E9 => "WED",
	0x03EA => "CHU",
	0x03EB => "TIS",
	0x03EC => "MOS",
	0x03ED => "ITM",
	0x03EE => "SPL",
	0x03EF => "BCS",
	0x03F0 => "IDS",
	0x03F1 => "CRE",
	0x03F2 => "ARE",
	0x03F3 => "DLG",
	0x03F4 => "2DA",
	0x03F5 => "GAM",
	0x03F6 => "STO",
	0x03F7 => "WMP",
	0x03F8 => "CHR",
	0x03F9 => "BS",
	0x03FA => "CHR",
	0x03FB => "VVC",
	0x03FC => "VFC",
	0x03FD => "PRO",
	0x03FE => "BIO",
	0x03FF => "WBM",
	0x0400 => "FNT",
	0x0402 => "GUI",
	0x0403 => "SQL",
	0x0404 => "PVRZ",
	0x0405 => "GLSL",
	0x0408 => "MENU",
	0x0409 => "LUA",
	0x040A => "TTF",
	0x040B => "PNG",
	0x044C => "BAH",
	0x0802 => "INI",
	0x0803 => "SRC",
]#»»
for (k,v) in RESTYPES
	@eval $(Symbol("RES_"*v)) = Restype($k)
end
function Base.show(io::IO, x::Restype)
	i = searchsorted(RESTYPES, reinterpret(UInt16,x); by=first)
	if length(i) == 1
		print(io, "RES_", RESTYPES[i[1]][2])
	else
		print(io, Restype, '(', reinterpret(UInt16,x), ')')
	end
end


# mutable struct File{S,X<:IO,Y<:NamedTuple}
# 	io::X
# 	data::Y
# 	@inline File{S}(x::X, y::Y) where{S,X<:IO, Y<:NamedTuple} =
# 		finalizer(x->close(x.io), new{S,X,Y}(x,y))
# end
# @inline File{S}(x::IO; kwargs...) where{S}= File{S}(x, NamedTuple(kwargs))

# ««1 tlk
struct TlkStrings{X}
	strings::Vector{X}
	index::Dict{String,UInt32}
	@inline TlkStrings(str::AbstractVector{X}) where{X} = new{X}(str, Dict())
end

const TLK_hdr = Files.Format(_ = b"TLK V1  ", lang = UInt16,
	nstr = UInt32, offset = UInt32)
const TLK_str = Files.Format(flags = UInt16, sound = Resref,
	volume = UInt32, pitch = UInt32, offset = UInt32, length = UInt32)

@inline function TlkStrings(file)
	io = open(file, "r")
	header = read(io, TLK_hdr)
	strref = read(io, TLK_str, header.nstr)
	strings = [ (string = string0(io, header.offset + s.offset, s.length),
		flags = s.flags, sound = s.sound, volume = s.volume, pitch = s.pitch)
		for s in strref ]
	return TlkStrings(strings)
end

struct TlkStringSet{X}
	strings1::TlkStrings{X}
	strings2::TlkStrings{X}
end

# ««1 key

@inline resource_name(t::Integer) =
	ResourceType[searchsorted(ResourceType, t; by=first)] |>only|>last|>String
@inline resource_type(s::AbstractString) =
	for (k,v) in ResourceType; v == codeunits(uppercase(s)) && return k; end

const KEY_hdr = Files.Format(_ = b"KEY V1  ", nbif = UInt32, nres = UInt32,
	bifoffset=UInt32, resoffset=UInt32)
const KEY_bif = Files.Format(filelength=UInt32, offset=UInt32,
	namelength=UInt16, location=UInt16)
const KEY_res = Files.Format(name=Resref, type=Restype, location=Resloc)

struct KeyIndex{X}
	bif::Vector{String}
	resources::Vector{X}
	location::Dict{Tuple{Resref,Restype},Resloc}
	function KeyIndex(bif, res::AbstractVector{X}) where{X}
		loc = Dict((r.name, r.type) => r.location for r in res)
		new{X}(bif, res, loc)
	end
end
function KeyIndex(filename::AbstractString)
	io = open(filename, "r")
	header = read(io, KEY_hdr)
	seek(io, header.bifoffset)
	bifentries = read(io, KEY_bif, header.nbif)
	bifnames = [ string0(io, x.offset, x.namelength) for x in bifentries ]
	seek(io, header.resoffset)
	resentries = read(io, KEY_res, header.nres)
	return KeyIndex(bifnames, resentries)
end

function Base.getindex(f::KeyIndex, resname::Resref, restype::Restype)
	f.location[(resname, restype)]
	# TODO: check if resname is unique w/o restype (seems false)
end

# function resource(f::KeyIndex, resname::AbstractString, restype::AbstractString)
# 	k = (codeunits(rpad(uppercase(resname), 8, '\0')), resource_type(restype))
# 	x = f.location[k]
# 	return (bifindex(x), tilesetindex(x), fileindex(x))
# end

# »»1
# ««1 bif
const BIF_hdr = Files.Format(_ = b"BIFFV1  ",
	nres=UInt32, ntilesets=UInt32, offset=UInt32)

function bif(f::AbstractString)
	io = open(f, "r")
	header = read_struct(io; marker = b"BIFFV1  ", nres=UInt32, ntilesets=UInt32,
		offset=UInt32)
	seek(io, header.offset)
	resources = read_structs(io, header.nres;
		locator=UInt32, offset=UInt32, size=UInt32, type=UInt16, unknown=UInt16)
	tilesets = read_structs(io, header.ntilesets;
		locator=UInt32, offset=UInt32, ntiles=UInt32, size=UInt32,
		type=UInt32(0x03eb), unknown=UInt16)
	return File{:bif}(io; header, resources, tilesets)
end

# @inline resource(f::File{:bif}, i::Integer) =
# 	read(seek(f.io, f.data.resources[i].offset), f.data.resources[i].size)

# ««1 itm
const ItmFormat = (
	marker = b"ITM V1  ",
	unidentified_name=Strref,
	identified_name=Strref,
	replacement=Resref,
	flags=UInt32,
	item_type=UInt16,
	usability=Bytes{4},
	animation=UInt16,
	minlevel=UInt16,
	minstrength=UInt16,
	minstrenghtbonus=UInt8,
	kit1=UInt8,
	minintelligence=UInt8,
	kit2=UInt8,
	mindexterity=UInt8,
	kit3=UInt8,
	minwisdom=UInt8,
	kit4=UInt8,
	minconstitution=UInt8,
	proficiency=UInt8,
	mincharisma=UInt16,
	price=UInt32,
	stackamount=UInt16,
	inventoryicon=Bytes{8},
	lore=UInt16,
	groundicon=Bytes{8},
	weight=UInt32,
	unidentified_description=Strref,
	identified_description=Strref,
	description_icon=Bytes{8},
	enchantment=UInt32,
	ext_header_offset=UInt32,
	ext_header_count=UInt16,
	feature_offset=UInt32,
	feature_index=UInt16,
	feature_count=UInt16)

function read_struct(io::IO, T::Type)
	buf = zeros(UInt8, sizeof(T))
	read!(io, buf)
	return only(reinterpret(T, buf))
end
function itm(f::AbstractString)
	io = open(f, "r")
	header = read_struct(io; ItmFormat...)
end
#»»1
end
IE=InfinityEngine; S = IE.Files
kf=S.Format(_=b"KEY V1  ", nbif=UInt32, nres=UInt32,)
# Itm=S.Format(_ = b"ITM V1  ", un_name = IE.Strref, id_name = IE.Strref,)

# tlk=IE.tlk("../bg2/game/lang/fr_FR/dialog.tlk")
# key=IE.key("../bg2/game/chitin.key")
# key=IE.keyfile("../bg2/game/chitin.key")
# bif=IE.bif("../bg2/game/data/items.bif")
# IE.fields(IE.ItmFormat)
# IE.fields(IE.Foo)
