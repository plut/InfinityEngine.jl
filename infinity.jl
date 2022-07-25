#=
# TODO:
# - TEST 1: define an item
# - a type for marked-language strings (pull from dialogs.jl)
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
# - Pack: allow invisible fields (i.e. present in the structure
#   but not in the file)
#   + add extra constructors: init(fieldtype, structtype, fieldname)
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
module Languages
using UniqueVectors

# TODO: should we make this a static list? There are 30 languages total...
const languages = UniqueVector{String}()
const current_language = Ref(0)

"""    language(l)

Sets current language to `l`. This will be used for future invocations
of `LangString(l)`.
"""
@inline language(str::AbstractString) =
	current_language[] = findfirst!(isequal(str), languages)
"""    language()

Returns current language (as a string).
"""
@inline language() = language(current_language[])

"""    GameText

A value describing a translated in-game string: either a `Strref`
(i.e. already present in the game), or a `LangString` (a user-added string,
annotated with its language tag, and to be later translated and converted
to a `Strref`). """
abstract type GameText end

"""    Strref

Index (32-bit) referring to a translated string in dialog.tlk or dialogF.tlk.
"""
struct Strref <: GameText
	index::Int32
end

"""    LangString

A string annotated with its source language.
"""
struct LangString <: GameText
	lang::Int8 # entry in the language type; no more than 256 languages...
	string::String
end

@inline language(s::LangString) = languages[s.lang]
@inline str(s::LangString) = s.string

@inline Base.show(io::IO, s::LangString) = print(io, s|>language, s|>str|>repr)

"""    GameText(string)

Converts `string` to a `GameText`; usually a `LangString` using current
language."""
@inline GameText(str::AbstractString) = LangString(current_language[], str)
@inline GameText(::Nothing) = Strref(0) # graceful fail...
@inline GameText(t::GameText) = t

@inline Base.isvalid(::LangString) = true
@inline Base.isvalid(s::Strref) = (s.index > 0)

export GameText, Strref, LangString, language
end
using .Languages
include("pack.jl"); using .Pack
include("dottedenums.jl"); using .DottedEnums
include("dialogs.jl")

using Printf
using StaticArrays
using Parameters
import Base.read

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
	begin
		println("chars = $chars")
		new{N}(chars)
	end
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

# ««2 Indices: Resref etc.
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

module Opcodes
	@enum Opcode::UInt16 begin
AC_bonus = 0
Modify_attacks_per_round
Cure_sleep
Berserk
Cure_berserk
Charm_creature
Charisma_bonus
Set_color
Set_color_glow_solid
Set_color_glow_pulse
Constitution_bonus
Cure_poison
Damage
Kill_target
Defrost
Dexterity_bonus
Haste
Current_HP_bonus
Maximum_HP_bonus
Intelligence_bonus
Invisibility
Lore_bonus
Luck_bonus
Reset_morale
Panic
Poison
Remove_curse
Acid_resistance_bonus
Cold_resistance_bonus
Electricity_resistance_bonus
Fire_resistance_bonus
Magic_damage_resistance_bonus
Raise_dead
Save_vs_death_bonus
Save_vs_wand_bonus
Save_vs_polymorph_bonus
Save_vs_breath_bonus
Save_vs_spell_bonus
Silence
Sleep
Slow
Sparkle
Bonus_wizard_spells
Stone_to_flesh
Strength_bonus
Stun
Cure_stun
Remove_invisibility
Vocalize
Wisdom_bonus
Character_color_pulse
Character_tint_solid
Character_tint_bright
Animation_change
Base_THAC0_bonus
Slay
Invert_alignment
Change_alignment
Dispel_effects
Move_silently_bonus
Casting_failure
Creature_RGB_color_fade
Bonus_priest_spells
Infravision
Remove_infravision
Blur
Translucency
Summon_creature
Unsummon_creature
Nondetection
Remove_nondetection
Change_gender
Change_AI_type
Attack_damage_bonus
Blindness
Cure_blindness
Feeblemindedness
Cure_feeblemindedness
Disease
Cure_disease
Deafness
Cure_deafness
Set_AI_script
Immunity_to_projectile
Magical_fire_resistance_bonus
Magical_cold_resistance_bonus
Slashing_resistance_bonus
Crushing_resistance_bonus
Piercing_resistance_bonus
Missile_resistance_bonus
Open_locks_bonus
Find_traps_bonus
Pick_pockets_bonus
Fatigue_bonus
Intoxication_bonus
Tracking_bonus
Change_level
Exceptional_strength_bonus
Regeneration
Modify_duration
Protection_from_creature_type
Immunity_to_effect
Immunity_to_spell_level
Change_name
XP_bonus
Remove_gold
Morale_break
Change_portrait
Reputation_bonus
Paralyze
Retreat_from
Create_weapon
Remove_item
Equip_weapon
Dither
Detect_alignment
Detect_invisible
Clairvoyance
Show_creatures
Mirror_image
Immunity_to_weapons
Visual_animation_effect
Create_inventory_item
Remove_inventory_item
Teleport
Unlock
Movement_rate_bonus
Summon_monsters
Confusion
Aid_non_cumulative
Bless_non_cumulative
Chant_non_cumulative
Draw_upon_holy_might_non_cumulative
Luck_non_cumulative
Petrification
Polymorph
Force_visible
Bad_chant_non_cumulative
Set_animation_sequence
Display_string
Casting_glow
Lighting_effects
Display_portrait_icon
Create_item_in_slot
Disable_button
Disable_spellcasting
Cast_spell
Learn_spell
Cast_spell_at_point
Identify
Find_traps
Replace_self
Play_movie
Sanctuary
Entangle_overlay
Minor_globe_overlay
Protection_from_normal_missiles_overlay
Web_effect
Grease_overlay
Mirror_image_effect
Remove_sanctuary
Remove_fear
Remove_paralysis
Free_action
Remove_intoxication
Pause_target
Magic_resistance_bonus
Missile_THAC0_bonus
Remove_creature
Prevent_portrait_icon
Play_damage_animation
Give_innate_ability
Remove_spell
Poison_resistance_bonus
Play_sound
Hold_creature
Movement_rate_bonus_2
Use_EFF_file
THAC0_vs_type_bonus
Damage_vs_type_bonus
Disallow_item
Disallow_item_type
Use_EFF_file_do_not_use
Use_EFF_file_while_on_type
No_collision_detection
Hold_creature_2
Move_creature
Set_local_variable
Increase_spells_cast_per_round
Increase_casting_speed_factor
Increase_attack_speed_factor
Casting_level_bonus
Find_familiar
Invisibility_detection
Ignore_dialogue_pause
Drain_CON_and_HP_on_death
Disable_familiar
Physical_mirror
Reflect_specified_effect
Reflect_spell_level
Spell_turning
Spell_deflection
Reflect_spell_school
Reflect_spell_type
Protection_from_spell_school
Protection_from_spell_type
Protection_from_spell
Reflect_specified_spell
Minimum_HP
Power_word_kill
Power_word_stun
Imprisonment
Freedom
Maze
Select_spell
Play_visual_effect
Level_drain
Power_word_sleep
Stoneskin_effect
Attack_and_Saving_Throw_roll_penalty
Remove_spell_school_protections
Remove_spell_type_protections
Teleport_field
Spell_school_deflection
Restoration
Detect_magic
Spell_type_deflection
Spell_school_turning
Spell_type_turning
Remove_protection_by_school
Remove_protection_by_type
Time_stop
Cast_spell_on_condition
Modify_proficiencies
Create_contingency
Wing_buffet
Project_image
Set_image_type
Disintegrate
Farsight
Remove_portrait_icon
Control_creature
Cure_confusion
Drain_item_charges
Drain_wizard_spells
Check_for_berserk
Berserk_effect
Attack_nearest_creature
Melee_hit_effect
Ranged_hit_effect
Maximum_damage_each_hit
Change_bard_song
Set_trap
Set_automap_note
Remove_automap_note
Create_item_days
Spell_sequencer
Create_spell_sequencer
Activate_spell_sequencer
Spell_trap
Activate_spell_sequencer_at_point
Restore_lost_spells
Visual_range_bonus
Backstab_bonus
Drop_item
Modify_global_variable
Remove_protection_from_spell
Disable_display_string
Clear_fog_of_war
Shake_screen
Unpause_target
Disable_creature
Use_EFF_file_on_condition
Zone_of_sweet_air
Phase
Hide_in_shadows_bonus
Detect_illusion_bonus
Set_traps_bonus
THAC0_bonus
Enable_button
Wild_magic
Wild_surge_bonus
Modify_script_state
Use_EFF_file_as_curse
Melee_THAC0_bonus
Melee_weapon_damage_bonus
Missile_weapon_damage_bonus
Remove_feet_circle
Fist_THAC0_bonus
Fist_damage_bonus
Change_title
Disable_visual_effects
Immunity_to_backstab
Set_persistent_AI
Set_existence_delay
Disable_permanent_death
Immunity_to_specific_animation
Immunity_to_turn_undead
Pocket_plane
Chaos_shield_effect
Modify_collision_behavior
Critical_hit_bonus
Can_use_any_item
Backstab_every_hit
Mass_raise_dead
Off_hand_THAC0_bonus
Main_hand_THAC0_bonus
Tracking
Immunity_to_tracking
Modify_local_variable
Immunity_to_time_stop
Wish
Immunity_to_sequester
High_level_ability
Stoneskin_protection
Remove_animation
Rest
Haste_2
Protection_from_resource
Restrict_item
Change_weather
Remove_effects_by_resource
AoE_evade_check_deprecated
Turn_undead_level
Immunity_to_resource_and_message
All_saving_throws_bonus
Apply_effects_list
Show_visual_effect
Set_spell_state
Slow_poison
Float_text
Summon_creatures_2
Attack_damage_type_bonus
Static_charge
Turn_undead
Seven_eyes
Seven_eyes_overlay
Remove_effects_by_opcode
Disable_rest_or_save
Alter_visual_animation_effect
Backstab_hit_effect
Critical_hit_effect
Override_creature_data
HP_swap
Enchantment_vs_creature_type
Enchantment_bonus
Save_vs_school_bonus
Move_view_to_target
Unknown_348
Unknown_349
Unknown_350
Unknown_351
Change_Background
Tint_screen
Flash_screen
Soul_exodus
Stop_all_actions
Set_state
Set_AI_script_2
Unknown_359
Ignore_reputation_breaking_point
Cast_spell_on_critical_miss
Critical_miss_bonus
Movement_check
Unknown_364
Make_unselectable
Apply_spell_on_movement
Minimum_base_stats
	end
end
using .Opcodes: Opcode
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
@inline read(f::Resource; kw...) = open(f) do io; read(io, f; kw...); end

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
struct TlkString
	string::String
	flags::UInt16
	sound::Resref"WAV"
	volume::UInt32
	pitch::UInt32
end
struct TlkStrings
	strings::Vector{TlkString}
	index::Dict{String,Int32}
	# i is the Julia index, not the strref
	@inline TlkStrings(strings::AbstractVector{TlkString}) =
		new(strings, Dict(s.string=>i for (i,s) in pairs(strings)))
end
@inline Base.show(io::IO, tlk::TlkStrings) =
	print(io, "<TlkStrings with ", length(tlk.strings), " entries>")

function Base.getindex(f::TlkStrings, s::Strref)
	i = s.index
	i ∈ eachindex(f.strings) || (i = 0)
	f.strings[i+1]
end

struct TLK_str
	flags::UInt16
	sound::Resref"WAV"
	volume::UInt32
	pitch::UInt32
	offset::UInt32
	length::UInt32
end
@pack struct TLK_hdr
	"TLK V1  "
	lang::UInt16
	length(strings)::UInt32
	offset(strings)::UInt32
	strings::Vector{TLK_str}
# 	nstr::UInt32
# 	offset::UInt32
end

function read(io::IO, f::Resource"TLK")
	header = unpack(io, TLK_hdr)
	return header
# 	strref = unpack(io, TLK_str, header.nstr)
	return TlkStrings([ TlkString(string0(io, header.offset + s.offset, s.length),
		s.flags, s.sound, s.volume, s.pitch) for s in header.strings ])
# 	return TlkStrings([ (string = string0(io, header.offset + s.offset, s.length),
# 		flags = s.flags, sound = s.sound, volume = s.volume, pitch = s.pitch)
# 		for s in strref ])
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
# ««2 Resource type table

# Static correspondence between UInt16 and strings (actually symbols).
const RESOURCE_TABLE = Dict{UInt16,String}(#««
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
)#»»
@inline String(x::Restype) = get(RESOURCE_TABLE, x.data, x.data|>repr)
@inline Base.Symbol(x::Restype) = Symbol(String(x))

# ««2 File blocks
@pack struct KEY_hdr
	"KEY V1  "
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

 - `Resource(key, resref)`: converts a `Resref` to a `Resource` of matching type.
 - `read(key, resref)`: opens the resource and reads it.
 - `names(key, Resource"type")`: returns an iterator over all names of resources
   of this type present in the game.
 - `findall(Resource"type", key)`: returns an iterator over all resources.
"""
struct KeyIndex
	directory::String
	bif::Vector{String}
	resources::Vector{KEY_res}
	location::Dict{Tuple{StaticString{8},Symbol},BifIndex}
	function KeyIndex(dir, bif, res::AbstractVector{KEY_res})
		loc = Dict((uppercase(r.name), Symbol(r.type)) => r.location for r in res)
		new(dir, bif, res, loc)
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
@pack struct BIF_hdr
	"BIFFV1  "
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

@pack struct BIF_tileset
	locator::BifIndex
	offset::UInt32
	ntiles::UInt32
	size::UInt32
	0x03eb
	_1::UInt16
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
function read(io::IO, f::Resource"IDS"; debug=false)
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
function read(io::IO, f::Resource"2DA"; debug=false, aligned=false)
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
@pack mutable struct CRE_hdr
	"CRE V1.0"
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
function read(io::IO, f::Resource"CRE")
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
@with_kw mutable struct ITM_ability
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
@with_kw mutable struct ITM_feature
	opcode::Opcode
	target::UInt8
	power::UInt8
	parameters::SVector{2,UInt32}
	timing_mode::UInt8
	dispel_resist::UInt8
	duration::UInt32
	probabilities::SVector{2,UInt8}
	resource::Resref"spl"
	dice_thrown::Int32
	dice_sides::Int32
	saving_throw_type::UInt32
	saving_throw_bonus::Int32
end
@pack mutable struct ITM_hdr{S}
	"ITM V1  "
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
	offset(abilities)::UInt32
	length(abilities)::UInt16
# 	ext_header_offset::UInt32
# 	ext_header_count::UInt16
	feature_offset::UInt32
	feature_index::UInt16
	feature_count::UInt16
	abilities::Vector{ITM_ability}
end
@inline searchkey(i::ITM_hdr) = i.identified_name

# ««2 Item function
struct Item
	header::ITM_hdr
	abilities::Vector{ITM_ability}
	features::Vector{ITM_feature}
end
function read(io::IO, ::Resource"ITM")
	header = unpack(io, ITM_hdr{Strref})
# 	abilities = unpack(seek(io, header.ext_header_offset), ITM_ability,
# 		header.ext_header_count)
# 	features = unpack(seek(io, header.feature_offset), ITM_feature,
# 		header.feature_count)
# 	return Item(header, abilities, features)
end
function write(io::IO, itm::Item)
	itm.header.ext_header_offset = 114
	itm.header.ext_header_count = length(itm.abilities)
	itm.header.feature_offset =
		114 + length(itm.abilities)*packed_sizeof(ITM_ability)
	itm.header.feature_index = 0 # FIXME
	itm.header.feature_count = length(itm.features)
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
@pack struct DLG_hdr
	"DLG V1.0"
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


function read(io::IO, f::Resource"DLG")
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

	global H = header
	dialog = Dialogs.top
	Dialogs.namespace("main")
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
IE=InfinityEngine; S=IE.Pack
IE.language("en")
itm = read(IE.Resource"../ciopfs/bg2/game/override/sw1h06.itm")
# game = IE.Game("../bg/game")
# itm=read(game, IE.Resref"blun01.itm")
str=read(IE.Resource"../bg/game/lang/fr_FR/dialog.tlk")
# # # IE.search(game, str, IE.Resource"ITM", "Varscona")
# # key = IE.KeyIndex("../bg/game/chitin.key")
# # # dlg = read(game, IE.Resref"melica.dlg")
# dlg = read(game, IE.Resref"zorl.dlg")
# # # dia = read(IE.Resource"dlg"(key, "abazigal"))
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
