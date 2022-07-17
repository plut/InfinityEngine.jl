#=
# TODO:
#  - dotted enum
#  - (minor variant) dotted bitflag (with ~ as well)
=#

module InfinityEngine
using Printf
using StaticArrays
using BitFlags

# ««1 Basic types
# zero-terminated string (chop trailing zeroes as needed)
@inline function string0(v::AbstractVector{UInt8})
	l = findfirst(iszero, v)
	String(isnothing(l) ? v : view(v, 1:l-1))
end
@inline string0(io::IO, s::Integer, l::Integer) = string0(read(seek(io, s), l))

# ««2 Indices: Strref, Resref etc.

abstract type AbstractStrongInt end

struct StrongInt{I<:Integer,K} <: AbstractStrongInt
	n::I
end
@inline (::Type{<:Integer})(x::StrongInt) = x.n
@inline Base.show(io::IO, x::StrongInt{I,K}) where{I,K} =
	print(io, K,'(',repr(x.n),')')

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

@enum Restype::UInt16 begin
	Restype_BMP = 0x0001
	Restype_MVE = 0x0002
	Restype_WAV = 0x0004
	Restype_PLT = 0x0006
	Restype_BAM = 0x03e8
	Restype_WED = 0x03e9
	Restype_CHU = 0x03ea
	Restype_TIS = 0x03eb
	Restype_MOS = 0x03ec
	Restype_ITM = 0x03ed
	Restype_SPL = 0x03ee
	Restype_BCS = 0x03ef
	Restype_IDS = 0x03f0
	Restype_CRE = 0x03f1
	Restype_ARE = 0x03f2
	Restype_DLG = 0x03f3
	Restype_2DA = 0x03f4
	Restype_GAM = 0x03f5
	Restype_STO = 0x03f6
	Restype_WMP = 0x03f7
	Restype_CHR = 0x03f8
	Restype_BS = 0x03f9
	Restype_CHR2 = 0x03fa
	Restype_VVC = 0x03fb
	Restype_VFC = 0x03fc
	Restype_PRO = 0x03fd
	Restype_BIO = 0x03fe
	Restype_WBM = 0x03ff
	Restype_FNT = 0x0400
	Restype_GUI = 0x0402
	Restype_SQL = 0x0403
	Restype_PVRZ = 0x0404
	Restype_GLSL = 0x0405
	Restype_MENU = 0x0408
	Restype_LUA = 0x0409
	Restype_TTF = 0x040a
	Restype_PNG = 0x040b
	Restype_BAH = 0x044c
	Restype_INI = 0x0802
	Restype_SRC = 0x0803
end

macro dotenum(type, values)
	typename = Meta.isexpr(type, :(::)) ? type.args[1] : type
	newvalues = :(begin end)
	for v in values.args
		v isa LineNumberNode && continue
		println("value $v")
		if Meta.isexpr(v, :(=))
			(x, y) = v.args; s = Symbol(string(typename)*'_'*string(x))
			push!(newvalues.args, :($s=$y))
		else
			s = Symbol(string(typename)*'_'*string(v))
			push!(newvalues.args, :($s))
		end
	end
	println(newvalues)
end
@bitflag ItemFlags::UInt32 begin
	ItemFlags_Unsellable
	ItemFlags_TwoHanded
	ItemFlags_Movable
	ItemFlags_Displayable
	ItemFlags_Cursed
	ItemFlags_CannotScribe
	ItemFlags_Magical
	ItemFlags_LeftHanded
	# byte 2
	ItemFlags_Silver
	ItemFlags_ColdIron
	ItemFlags_OffHanded
	ItemFlags_Conversable
	ItemFlags_FakeTwoHanded
	ItemFlags_ForbidOffHandAnimation
	ItemFlags_UsableInInventory # PSTEE
	ItemFlags_Adamantine
	# byte 4
	ItemFlags_Undispellable = 0x01000000
	ItemFlags_ToggleCriticalHitAversion
end
@enum ItemType::UInt16 begin
	ItemTypeMisc = 0x0000
	ItemTypeAmulet = 0x0001
	ItemTypeArmor = 0x0002
  ItemTypeBelt = 0x0003
  ItemTypeBoots = 0x0004
  ItemTypeArrow = 0x0005
  ItemTypeBracers = 0x0006
  ItemTypeHeadgear = 0x0007
  ItemTypeKeys = 0x0008 # not in IWD
  ItemTypePotion = 0x0009
  ItemTypeRing = 0x000a
  ItemTypeScroll = 0x000b
  ItemTypeShield = 0x000c
  ItemTypeFood = 0x000d
  ItemTypeBullet = 0x000e
  ItemTypeBow = 0x000f
  ItemTypeDagger = 0x0010
  ItemTypeMace = 0x0011 # includes clubs in BG
  ItemTypeSling = 0x0012
  ItemTypeSmallSword = 0x0013
  ItemTypeLargeSword = 0x0014
  ItemTypeHammer = 0x0015
  ItemTypeMorningstar = 0x0016
  ItemTypeFlail = 0x0017
  ItemTypeDart = 0x0018
  ItemTypeAxe = 0x0019
  ItemTypeQuarterstaff = 0x001a
  ItemTypeCrossbow = 0x001b
  ItemTypeFistWeapon = 0x001c
  ItemTypeSpear = 0x001d
  ItemTypeHalberd = 0x001e # and two-handed axes
  ItemTypeBolt = 0x001f
  ItemTypeCloak = 0x0020
  ItemTypeGold = 0x0021 # for monster-dropped treasure
  ItemTypeGem = 0x0022
  ItemTypeWand = 0x0023
  ItemTypeContainer = 0x0024 # eye/broken armor
  ItemTypeBooks = 0x0025 # broken shield/bracelet
  ItemTypeFamiliar = 0x0026 # broken sword/earrings
  ItemTypeTattoo = 0x0027 # PST
  ItemTypeLenses = 0x0028 # PST
  ItemTypeBuckler = 0x0029 # teeth (PST)
  ItemTypeCandle = 0x002a
  ItemTypeClub = 0x002c # IWD
  ItemTypeLargeShield = 0x002f # IWD
  ItemTypeMediumShield = 0x0031 # IWD
  ItemTypeNotes = 0x0032
  ItemTypeSmallShield = 0x0035 # IWD
  ItemTypeTelescope = 0x0037 # IWD
  ItemTypeDrink = 0x0038 # IWD
  ItemTypeGreatSword = 0x0039 # IWD
  ItemTypeContainer1 = 0x003a
  ItemTypeFur = 0x003b
  ItemTypeLeatherArmor = 0x003c
  ItemTypeStuddedLeatherArmor = 0x003d
  ItemTypeChainMail = 0x003e
  ItemTypeSplintMail = 0x003f
  ItemTypeHalfPlate = 0x0040
  ItemTypeFullPlate = 0x0041
  ItemTypeHideArmor = 0x0042
  ItemTypeRobe = 0x0043
  ItemTypeBastardSword = 0x0045
  ItemTypeScarf = 0x0046
  ItemTypeFoodIWD2 = 0x0047
  ItemTypeHat = 0x0048
  ItemTypeGauntlet = 0x0049
end
@enum UsabilityFlags::UInt32 begin
	UsabilityFlags_Chaotic
end

# Formatted blocks ««1

abstract type AbstractBlock end

reprb(x) = repr(x)
reprb(x::Base.CodeUnits) = 'b'*repr(String(x))

macro Block(name, fields)
	type_fields = Expr[]
	read_fields = Expr[]
	read_vars = Symbol[]
	offset = 0
	description = ""
	for expr in fields.args
		expr isa LineNumberNode && continue
		if Meta.isexpr(expr, :(::))
			field, content = expr.args; content = eval(content)
			push!(type_fields, expr)
			v = gensym(); push!(read_vars, v);
			push!(read_fields, :($v = readt(io, $content)))
		else
			field, content = reprb(expr), eval(expr)
			push!(read_fields, :(@assert read(io, $(sizeof(content))) == $content))
		end
		description*= @sprintf("0x%04x %4d %s\n", offset, sizeof(content), field)
		offset+= sizeof(content)
	end
	description*= "total size = "*string(offset)
	name = esc(name)
	quote
		struct $(name) <: AbstractBlock
			$(type_fields...)
		end
		@inline Base.show(io::IO, ::MIME"text/plain", ::Type{$name}) =
			print(io, $description)
		function Base.read(io::IO, ::Type{$name})
			$(read_fields...)
			return $name($(read_vars...))
		end
	end
end

@inline readt(io::IO, T::DataType) = only(reinterpret(T,read(io, sizeof(T))))
@inline Base.read(io::IO, T::Type{<:AbstractBlock}, n::Integer) =
	[ read(io, T) for _ in Base.OneTo(n) ]

# ««1 tlk
struct TlkStrings{X}
	strings::Vector{X}
	index::Dict{String,UInt32}
	@inline TlkStrings(str::AbstractVector{X}) where{X} = new{X}(str, Dict())
end

@inline Base.getindex(f::TlkStrings, i::Strref) = f.strings[Int32(i)+1]

@Block TLK_hdr begin
	b"TLK V1  "
	lang::UInt16
	nstr::UInt32
	offset::UInt32
end
@Block TLK_str begin
	flags::UInt16
	sound::Resref
	volume::UInt32
	pitch::UInt32
	offset::UInt32
	length::UInt32
end

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
@Block KEY_hdr begin
	b"KEY V1  "
	nbif::Int32
	nres::Int32
	bifoffset::UInt32
	resoffset::UInt32
end
@Block KEY_bif begin
	filelength::UInt32
	offset::UInt32
	namelength::UInt16
	location::UInt16
end
@Block KEY_res begin
	name::Resref
	type::Restype
	location::Resloc
end

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

@Block BIF_hdr begin
	b"BIFFV1  "
	nres::UInt32
	ntilesets::UInt32
	offset::UInt32
end

@Block BIF_resource begin
	locator::Resloc
	offset::UInt32
	size::UInt32
	type::Restype
	unknown::UInt16
end

@Block BIF_tileset begin
	locator::Resloc
	offset::UInt32
	ntiles::UInt32
	size::UInt32
	0x03eb
	unknown::UInt16
end

bifresource(file::AbstractString, index::Integer) = open(file, "r") do io
	header = read(io, BIF_hdr)
	seek(io, header.offset)
	resources = read(io, BIF_resource, header.nres)
	IOBuffer(read(seek(io, resources[index+1].offset), resources[index+1].size))
end

# »»1
# ««1 itm

@Block ITM_hdr begin
	b"ITM V1  "
	unidentified_name::Strref
	identified_name::Strref
	replacement::Resref
	flags::ItemFlags
	item_type::ItemType
	usability::UInt32
	animation::UInt16
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
	proficiency::UInt8
	mincharisma::UInt16
	price::UInt32
	stackamount::UInt16
	inventoryicon::Resref
	lore::UInt16
	groundicon::Resref
	weight::UInt32
	unidentified_description::Strref
	identified_description::Strref
	description_icon::Resref
	enchantment::UInt32
	ext_header_offset::UInt32
	ext_header_count::UInt16
	feature_offset::UInt32
	feature_index::UInt16
	feature_count::UInt16
end

# const ITM_ext_hdr = Files.Format(type = UInt8, must_identify = UInt8,
# 	location = UInt8, alternative_dice_sides = UInt8, use_icon = Resref,
# 	target_type = UInt8, target_count = UInt8, range = UInt16,
# 	launcher_required = UInt8, alternative_dice_thrown = UInt8,
# 	speed_factor = UInt8, alternative_damage_bonus = UInt8,
# 	thac0_bonus = UInt16, dice_sides = UInt8, primary_type = UInt8,
# 	dice_thrown = UInt8, secondary_type = UInt8, damage_bonus = UInt16,
# 	damage_type = UInt16, nfeatures = UInt16, idxfeatures = UInt16,
# 	max_charges = UInt16, depletion = UInt16, flags = UInt32,
# 	projectile_animation = UInt16,
# 	overhand_chance = UInt16, backhand_chance = UInt16, thrust_chance = UInt16,
# 	is_arrow = UInt16, is_bolt = UInt16, is_bullet = UInt16,
# 	)


@inline function Item(io::IO)
	read(io, ITM_hdr)
end
@inline Item(f::AbstractString) = open(Item, f, "r")
#»»1
end
IE=InfinityEngine;
test() = open("../bg2/game/chitin.key", "r") do io
	read(io, IE.KEY_hdr)
end

key=IE.KeyIndex("../bg2/game/chitin.key");
str=IE.TlkStrings("../bg2/game/lang/fr_FR/dialog.tlk");
nothing
