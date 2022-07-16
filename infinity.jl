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

@inline Base.display(X::Type{<:Format}) = display(stdout, X)
function Base.display(io::IO, X::Type{Format{F,T}}) where{F,T}
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

@generated function Base.filesize(::Type{Format{F,T}}) where{F,T}
	s = 0
	for x in T.parameters
		if x isa DataType
			s+= sizeof(x.parameters[2])
		else
			s+= sizeof(x[2])
		end
	end
	:($s)
end
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

# zero-terminated string (chop trailing zeroes as needed)
@inline function string0(v::AbstractVector{UInt8})
	l = findfirst(iszero, v)
	String(isnothing(l) ? v : view(v, 1:l-1))
end
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))

# ««2 Indices: Strref, Resref etc.
# Type families:
# - integer: Strref, Resref etc.
# - enumerated: Restype etc.
#
# @StrongEnum Restype ( 0xabcd => "2da" etc.)
#  defines Restype_2da and the show methods
#
# - flags: Usability etc.
# @StrongFlags Usability (Chaotic, Evil, Good, Unaligned, etc.)
#   defines Usability_Chaotic | Usability_Evil | ...
#   and the show methods

struct StrongInt{I<:Integer,K}
	n::I
end
@inline (::Type{<:Integer})(x::StrongInt) = x.n
@inline Base.show(io::IO, x::StrongInt{I,K}) where{I,K}=print(io, K,'(',x.n,')')

const Strref = StrongInt{UInt32,:Strref}
const Resref = StrongInt{UInt64,:Resref}
@inline Resref(s::AbstractString) = 
	only(reinterpret(Resref,codeunits(rpad(uppercase(s), 8, Char(0)))))
@inline Base.string(x::Resref) =
	string0(only(reinterpret(SVector{8,UInt8}, SA[x.n])))
@inline Base.show(io::IO, x::Resref) = print(io, string(x))
macro res_str(s) Resref(s) end

const Resloc = StrongInt{UInt32,:Resloc} # bif indexing
@inline sourcefile(r::Resloc) = Int32(r) >> 20
@inline tilesetindex(r::Resloc) = (Int32(r) >> 14) && 0x3f
@inline resourceindex(r::Resloc) = Int32(r) & 0x3fff

const Restype = StrongInt{UInt16,:Restype} # resource name

const RESTYPES = [#««
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
	0x03fa => "CHR",
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

# ««1 tlk
struct TlkStrings{X}
	strings::Vector{X}
	index::Dict{String,UInt32}
	@inline TlkStrings(str::AbstractVector{X}) where{X} = new{X}(str, Dict())
end

@inline Base.getindex(f::TlkStrings, i::Strref) = f.strings[Int32(i)+1]

const TLK_hdr = Files.Format(_ = b"TLK V1  ", lang = UInt16,
	nstr = UInt32, offset = UInt32)
const TLK_str = Files.Format(flags = UInt16, sound = Resref,
	volume = UInt32, pitch = UInt32, offset = UInt32, length = UInt32)

@inline TlkStrings(filename::AbstractString) = open(filename, "r") do io
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

# ««1 key/bif

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
	directory::String
	bif::Vector{String}
	resources::Vector{X}
	location::Dict{Tuple{Resref,Restype},Resloc}
	function KeyIndex(dir, bif, res::AbstractVector{X}) where{X}
		loc = Dict((r.name, r.type) => r.location for r in res)
		new{X}(dir, bif, res, loc)
	end
end
@inline KeyIndex(filename::AbstractString) = open(filename, "r") do io
	header = read(io, KEY_hdr)
	seek(io, header.bifoffset)
	bifentries = read(io, KEY_bif, header.nbif)
	bifnames = [ string0(io, x.offset, x.namelength) for x in bifentries ]
	seek(io, header.resoffset)
	resentries = read(io, KEY_res, header.nres)
	return KeyIndex(dirname(filename), bifnames, resentries)
end

function Base.getindex(f::KeyIndex, resname::Resref, restype::Restype)
# Resources are not unique w.r.t the name, but only name + type
# e.g. plangood.{bcs,cre,chr}
	loc = f.location[(resname, restype)]
	file = joinpath(f.directory, f.bif[1+sourcefile(loc)])
	return bifresource(file, resourceindex(loc))
end

const BIF_hdr = Files.Format(_ = b"BIFFV1  ",
	nres=UInt32, ntilesets=UInt32, offset=UInt32)
const BIF_resource = Files.Format(locator = Resloc, offset = UInt32,
	size = UInt32, type = Restype, _ = UInt16)
const BIF_tileset = Files.Format(locator = Resloc, offset=UInt32,
	ntiles=UInt32, size=UInt32, type=RES_TIS, unknown=UInt16)

bifresource(file::AbstractString, index::Integer) = open(file, "r") do io
	header = read(io, BIF_hdr)
	seek(io, header.offset)
	resources = read(io, BIF_resource, header.nres)
	IOBuffer(read(seek(io, resources[index+1].offset), resources[index+1].size))
end

# »»1
# ««1 itm
const ITM_hdr = Files.Format(_ = b"ITM V1  ",
	unidentified_name=Strref, identified_name=Strref, replacement=Resref,
	flags=UInt32, item_type=UInt16, usability=UInt32,
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
	inventoryicon=Resref,
	lore=UInt16,
	groundicon=Resref,
	weight=UInt32,
	unidentified_description=Strref,
	identified_description=Strref,
	description_icon=Resref,
	enchantment=UInt32,
	ext_header_offset=UInt32,
	ext_header_count=UInt16,
	feature_offset=UInt32,
	feature_index=UInt16,
	feature_count=UInt16)
const ITM_ext_hdr = Files.Format(type = UInt8, must_identify = UInt8,
	location = UInt8, alternative_dice_sides = UInt8, use_icon = Resref,
	target_type = UInt8, target_count = UInt8, range = UInt16,
	launcher_required = UInt8, alternative_dice_thrown = UInt8,
	speed_factor = UInt8, alternative_damage_bonus = UInt8,
	thac0_bonus = UInt16, dice_sides = UInt8, primary_type = UInt8,
	dice_thrown = UInt8, secondary_type = UInt8, damage_bonus = UInt16,
	damage_type = UInt16, nfeatures = UInt16, idxfeatures = UInt16,
	max_charges = UInt16, depletion = UInt16, flags = UInt32,
	projectile_animation = UInt16,
	overhand_chance = UInt16, backhand_chance = UInt16, thrust_chance = UInt16,
	is_arrow = UInt16, is_bolt = UInt16, is_bullet = UInt16,
	)


@inline function Item(io::IO)
	read(io, ITM_hdr)
end
@inline Item(f::AbstractString) = open(Item, f, "r")
#»»1
end
IE=InfinityEngine; S = IE.Files

key=IE.KeyIndex("../bg2/game/chitin.key");
str=IE.TlkStrings("../bg2/game/lang/fr_FR/dialog.tlk");
