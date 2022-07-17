#=
# TODO:
#  - dotted enum
#  - (minor variant) dotted bitflag (with ~ as well)
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
	type_fields = Expr[]
	read_fields = Expr[]
	read_vars = Symbol[]
	field_frag = Dict{Symbol,BitVector}()
	offset = 0
	description = ""
	for expr in fields.args
		expr isa LineNumberNode && continue
		if Meta.isexpr(expr, :ref)
			error("not implemented")
# 			field, r = expr.args[1], Core.eval(__module__, expr.args[2])
# 			if Meta.isexpr(field, :(::))
# 				field, content = expr.args[1], Core.eval(__module__, field.args[2])
# 				@assert !haskey(field_frag, field)
# 				field_frag[field] = falses(sizeof(content))
# 				push!(type_fields, :($field::$(esc(content))))
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
			field, content = expr.args[1], Core.eval(__module__, expr.args[2])
			push!(type_fields, esc(expr))
			v = field; push!(read_vars, v)
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
	block = quote
		Base.@__doc__(struct $name <: AbstractBlock
			$(type_fields...)
			$name(::Type{AbstractBlock};$(read_vars...)) = new($(read_vars...))
		end)
		# this hook allows overriding the kwargs method:
		@inline $name(;kwargs...) = $name(AbstractBlock; kwargs...)
		@inline Base.show(io::IO, ::MIME"text/plain", ::Type{$name}) =
			print(io, $description)
		function Base.read(io::IO, ::Type{$name})
			$(read_fields...)
			return $name(;$(read_vars...))
		end
	end
	block.head = :toplevel
	return block
end
@block X begin
	a::Int
	foo::UInt32
end


@inline readt(io::IO, T::DataType) = only(reinterpret(T,read(io, sizeof(T))))
@inline Base.read(io::IO, T::Type{<:AbstractBlock}, n::Integer) =
	[ read(io, T) for _ in Base.OneTo(n) ]
end
using .Blocks: @block

include("dottedenums.jl"); using .DottedEnums

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
@inline Base.show(io::IO,s::StaticString) = @printf io "b\"%s\"" String(s.chars)
@inline (::Type{String})(s::StaticString) = string0(s.chars)
@inline (T::Type{<:StaticString{N}})(s::AbstractString) where{N} =
	T(codeunits(rpad(uppercase(s), N, '\0')))

# ««2 Type wrapper
struct Typewrap{T,S}
	data::T
	@inline (::Type{Typewrap{T,S}})(args...) where{T,S} = new{T,S}(T(args...))
end
@inline (::Type{Typewrap{T,S}})(x::Typewrap{T,S}) where{T,S} = x
@inline Base.show(io::IO, t::Typewrap{T,S}) where{T,S} =
	(print(io, S, '('); show(io, t.data); print(io, ')'))
@inline (J::Type{<:Integer})(x::Typewrap{<:Integer}) = J(x.data)
@inline (J::Type{<:AbstractString})(x::Typewrap{<:AbstractString}) = J(x.data)

# ««2 Indices: Strref, Resref etc.

const Resref = Typewrap{StaticString{8},:Resref}
macro res_str(s) Resref(s) end

const Strref = Typewrap{UInt32, :Strref}

const Resloc = Typewrap{UInt32, :Resloc} # bif indexing
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
@inline Restype(t::AbstractString) = eval(Symbol("Restype_"*uppercase(t)))

# Formatted blocks ««1

# ««1 tlk
struct TlkStrings{X}
	strings::Vector{X}
	index::Dict{String,UInt32}
	@inline TlkStrings(str::AbstractVector{X}) where{X} = new{X}(str, Dict())
end

@inline Base.getindex(f::TlkStrings, i::Strref) = f.strings[Int32(i)+1]

@block TLK_hdr begin
	b"TLK V1  "
	lang::UInt16
	nstr::UInt32
	offset::UInt32
end
@block TLK_str begin
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

const XOR_KEY = b"\x88\xa8\x8f\xba\x8a\xd3\xb9\xf5\xed\xb1\xcf\xea\xaa\xe4\xb5\xfb\xeb\x82\xf9\x90\xca\xc9\xb5\xe7\xdc\x8e\xb7\xac\xee\xf7\xe0\xca\x8e\xea\xca\x80\xce\xc5\xad\xb7\xc4\xd0\x84\x93\xd5\xf0\xeb\xc8\xb4\x9d\xcc\xaf\xa5\x95\xba\x99\x87\xd2\x9d\xe3\x91\xba\x90\xca"

function decrypt(io::IO)
	peek(io) != 0xff && return io
	buf = collect(codeunits(read(io, String)))
	for i in eachindex(buf)
		buf[i] ⊻= XOR_KEY[mod1(i-2, length(XOR_KEY))]
	end
	return IOBuffer(buf[2:end])
end
function Base.getindex(f::KeyIndex, resname::Union{AbstractString,Resref},
		restype::Union{AbstractString,Restype})
	resname, restype = Resref(resname), Restype(restype)
# Resources are not unique w.r.t the name, but only name + type
# e.g. plangood.{bcs,cre,chr}
	loc = f.location[(resname, restype)]
	file = joinpath(f.directory, f.bif[1+sourcefile(loc)])
	io = bifresource(file, resourceindex(loc))
	return restype ∈ (Restype_2DA, Restype_IDS) ? decrypt(io) : io
end
# @inline Base.getindex(f::KeyIndex, s::AbstractString, t) = getindex(f, Resref(s), t)
# @inline Base.getindex(f::KeyIndex, resname::Resref, t::AbstractString) =
# 	getindex(f, resname, Restype(t))
@inline (::Type{String})(f::KeyIndex, resname, t) = read(f[resname, t], String)
grep(f::KeyIndex, t, s) =
	String.(r for r in allresources(f, t) if contains(String(f,r,t), s))
# half-orc: 53187 53213 224201 224203 265330
@inline allresources(f::KeyIndex,t::AbstractString) = allresources(f,Restype(t))
@inline allresources(f::KeyIndex, t::Restype) =
	( r.name for r in f.resources if r.type == t )

@block BIF_hdr begin
	b"BIFFV1  "
	nres::UInt32
	ntilesets::UInt32
	offset::UInt32
end

@block BIF_resource begin
	locator::Resloc
	offset::UInt32
	size::UInt32
	type::Restype
	unknown::UInt16
end

@block BIF_tileset begin
	locator::Resloc
	offset::UInt32
	ntiles::UInt32
	size::UInt32
	UInt16(0x03eb)
	unknown::UInt16
end

bifresource(file::AbstractString, index::Integer) = open(file, "r") do io
	header = read(io, BIF_hdr)
	seek(io, header.offset)
	resources = read(io, BIF_resource, header.nres)
	IOBuffer(read(seek(io, resources[index+1].offset), resources[index+1].size))
end

# ««1 ids
function Ids(io::IO; debug=false)
	debug && (mark(io); println("(", read(io, String), ")"); reset(io))
	line = readline(io)
	startswith(line, "IDS") && (line = readline(io))
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
@inline Ids(key::KeyIndex, r) = Ids(key[r, Restype_IDS])
@inline Ids(f::AbstractString) = open(Ids, f, "r")
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

function TwoDA(io::IO; debug=false, aligned=false)
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
@inline TwoDA(key::KeyIndex, r; kw...) = TwoDA(key[r, Restype_2DA]; kw...)
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
@block ITM_hdr begin
	b"ITM V1  "
	unidentified_name::Strref
	identified_name::Strref
	replacement::Resref
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
@block ITM_ability begin
	attack_type::UInt8
	must_identify::UInt8
	location::UInt8
	alternative_dice_sides::UInt8
	use_icon::Resref
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
@inline function Item(io::IO)
	read(io, ITM_hdr)
end
@inline Item(f::AbstractString) = open(Item, f, "r")
@inline Item(key::KeyIndex, r::Resref) = Item(key[r, Restype_ITM])
# ««1 dlg
@block DLG_hdr begin
end
# »»1
end
IE=InfinityEngine
test() = open("../bg2/game/chitin.key", "r") do io
	read(io, IE.KEY_hdr)
end

key=IE.KeyIndex("../bg2/game/chitin.key")
str=IE.TlkStrings("../bg2/game/lang/fr_FR/dialog.tlk")
nothing
