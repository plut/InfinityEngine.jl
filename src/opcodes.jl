module Opcodes
using ..DottedEnums
using ..DottedEnums: flagstext, enumtext
	@SymbolicEnum Opcode::UInt16 begin
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
#««1 Individual opcodes
str(op::Opcode, parameters...) =
	repr(op; context=(:compact=>true)) *'('*
	join(argstr(Val(UInt16(op)), parameters...), ", ")*')'
argstr(::Val, parameters...) = string.(parameters)
#««2 0 AC bonus
# @SymbolicEnum AC_Type All=0 Crushing=1 Missile Piercing Slashing Base
const AC_types = Dict(0x00 => "All", 0x01 => "Crushing",
	0x02 => "Missile", 0x04 => "Piercing", 0x08 => "Slashing",
	0x10 => "Set Base AC")
argstr(::Val{0x0000}, ac_mod, type) = (flagstext(AC_types, type), string(ac_mod))
#««2 1 APR bonus
argstr(::Val{0x0001}, j, type) = statmod(((2*j)%11)/2, type)
argstr(::Val{0x0003}, _, type) = (isone(type) ? "permanent" : "in combat",)
general_ids = Dict(0 => "general_item", 1 => "humanoid", 2 => "animal",
	3 => "dead", 4 => "undead", 5 => "giant humanoid", 6 => "frozen",
	7 => "plant", 101 => "weapon", 102 => "armor", 103 => "amulet",
	104 => "belt", 105 => "boots", 106 => "ammo", 107 => "helmet",
	108 => "key", 109 => "potion", 110 => "ring", 111 => "scroll",
	112 => "shield", 113 => "gloves", 255 => "monster")
function argstr(::Val{0x0005}, gt, ct)
	charmtypes = ["neutral", "hostile", "neutral dire", "hostile dire",
		"cleric controlled", "thrall"]
	sct = charmtypes[1+(ct%1000)]
	ct ≥ 1000 && (sct *= " silent")
	return (enumtext(general_ids, gt), sct)
end

#««2 7 Set color
const Color_types = Dict(0x00 => "Buckle/Amulet", 0x01 => "Minor",
	0x02 => "Major", 0x03 => "Skin", 0x04 => "Strap",
	0x05 => "Armor", 0x06 => "Hair",
	0x10 => "Weapon crossguard",
	0x14 => "Weapon grip", 0x15 => "Weapon blade",
	0x20 => "Shield hub", 0x21 => "Shield interior",
	0x22 => "Shield panelling", 0x25 => "Shield rim",
	0x30 => "Helmet wings", 0x31 => "Helmet feathers",
	0x32 => "Helmet feathers 2", 0x34 => "Helmet face",
	0x35 => "Helmet main",
	0xff => "Whole body")
const Color_gradients = [ "Red Tinted Black", "Dark Bronze", "Dark Gold",
"Light Gold", "Auburn", "Light Silver", "Dark Silver", "Light Metallic Green",
"Dark Muddish Brown", "Light Muddish Brown", "Light Pure Gold",
"Light Rose Red", "Light Carnation Pink", "Light Pure Silver",
"Dark Pure Silver", "Easter Green", "Silver Gold", "Light Blue", "Dark Blue",
"Dark Rose Red", "Dark Moldy Green", "Dark Iron Grey", "Dark Brown",
"Light Copper", "Dark Gold", "Dark Pure Gold", "Wood", "Silver",
"Ghostly Greyish Silver", "Red Tinted Black", "Light Iron Grey",
"Light Sea Blue", "Dark Sea Green", "Dark Metallic Purple", "Ghostly Green",
"Dark Ghostly Pink", "Light Dirty Yellow", "Dark Dirty Yellow",
"Light Dirtish Brown", "Dark Dirtish Brown", "Dark Dirty Copper", "Dark Brown",
"Light Blurry Grey", "Dark Dirtish Brown", "Light Purple", "Dark Purple",
"Light Red", "Dark Red", "Light Bronze", "Dark Bronze", "Dark Yellow",
"Dark Olive Green", "Light Green", "Green", "Dark Green", "Light Sea Blue",
"Dark Sea Blue", "Light Blue", "Dark Blue", "Light Purple", "Dark Purple",
"Lavender", "Dark Lavender", "Light Grey", "Dark Grey", "Really Dark Grey",
"Faded Black", "Shiny Gold", "Shiny Blue", "Shiny Green", "Light Metallic Red",
"Frost Blue", "Steel Grey", "Christmas Green", "Pure White", "Pure Black",
"Pure Red", "Pure Green (invisible in icons)", "Pure Blue", "Light Silver",
"Dark Bronze", "Faded Gold", "Dark Metallic Blue", "Dark Adamantite Grey",
"Peachy", "21 Caret Gold", "Chrome Green", "Dark Poopy Brown", "Chrome Pink",
"Metallic Pink", "Moldy Gold", "Dark Choclate", "Light Chocolate",
"Dark Cement Grey", "Rhubarb", "Light Dirtish Brown", "Chrome Blue",
"Light Minty Blue", "Leaf Green", "Dark Gold", "Dark Iron Grey",
"Light Metallic Red", "Sea Blue", "Forest Green", "Dark Chrome Purple",
"White", "Pale Pink", "Khaki", "Pinkish Grey", "Terracotta", "Ash",
"Burnt Sienna", "Seafoam", "Fog", "Yet Another Brown", "Sunkissed",
"#6437A2", "#3F2771", "#3E2F59", "#312546", "#176F8C", "#0E4260", "#2448A6",
"#052E6E", "#354F94", "#133160", "#963E34", "#631212", "#CA7A23", "#A44D0A",
"#277E0B", "#832A4B", "#600C36", "#739400", "#577000", "#8E8E8E", "#1B1B1B",
"#8A7B5A", "#554A37", "#926C83", "#5E4554", "#798E9F", "#515F6B", "#C27367",
"#8A514A", "#84A369", "#576D46", "#AD5322", "#2C95A1", "#59A321", "#B82C4D",
"#7465A4", "#C4AA98", "#C1A4B4", "#D0AF94", "#C48D74", "#CB947D", "#F0BC97",
"#936053", "#6B5A4D", "#543829", "#CBA190", "#C59E77", "#89683F", "#58392B",
"#B1A0AF", "#B28493", "#B39DB1", "#908758", "#5A6688", "#A39FA4", "#C4B3A5",
"#D1A452", "#A2A083", "#C4AA98", "#9FBAA6", "#94A2A7", "#9CB1B4", "#96AE8A",
"#AB9DBC", "#9B7990", "#AB98C4", "#33363E", "#343440", "#61687C", "#585268",
"#4C3B46", "#615F68", "#6A6064", "#A7AD7F", "#212033", "#4D574E", "#697D6B",
"#517666", "#D17AAA", "#761313", "#4F495D", "#29130C", "#210707", "#1C2425",
"#729E9E", "#80954D", "#616B84", "#6E8461", "#3C1B2F", "#64676F", "#596259",
"#A18918", "#A79344", "#7A745F", "#863130", "#B07B41", "#6C707A", "#526660",
"#FFB034", "#E78548", "#F5A930", "#97ABB7", "#7294B7", "#88B382", "#1C3B4B",
"#BECE59", "#E5E2C2", "#A9AF79", "#D2C757", "#E3DC94", "#A59984", "#AE5C38",
"#BBB276", "#473960", "#546F2E", "#79849A", "#6E6D67", "#B78251", "#9D5F4F",
"#5B5B6A", "#556250", "#402920", "#723523", "#947459", "#A27545", "#8F7646",
"#A79361", "#933635", "#38484A", "#586D6F", "#606F58", "#8A3F3C", "#96938A",
"#8A8C97", "#DBAE4F", "#829943", "#706815", "#7C6B41", "#3A392F", "#181818",]
 
argstr(::Val{0x0007}, gradient, location) =
	(enumtext(Color_types, location), enumtext(Color_gradients, gradient))
argstr(::Val{0x0008}, rgb, loc) =
	(enumtext(Color_types, location), string(rgb))
#««2 0x2a Bonus wizard spells / 0x3e priest spells
argstr(::Val{0x002a}, amount, lvl) = iszero(lvl) ?
	("double", "lvl≤"*string(amount)) :
	("+"*string(amount), "lvl="*string(9-leading_zeros(UInt8(lvl-1))))
argstr(::Val{0x003e}, amount, lvl) = argstr(Val(0x002a), amount, lvl)
#««2 Opcodes without parameters
for x in (0x002, 0x0004, 0x000b, 0x000e, 0x001a, 0x0026, 0x0028, 0x002b, 0x002d, 0x002e, 0x002f, 0x0030, 0x0038, 0x003f, 0x0040, 0x0041, 0x0045, 0x0046, 0x004a, 0x004b, 0x004c, 0x004d, 0x004f, 0x0050, 0x0051, 0x006e, 0x0070, 0x0071, 0x0072)
	@eval argstr(::Val{$x}, _...) = ()
end
statmod(val, type) =
	(type == 1) ? ("="*string(val),) :
	(type == 2) ? ("*="*string(val)*"%",) :
	("+="*string(val),)
#««2 Stat bonus
for x in (0x0006, 0x000a, 0x000f, 0x0011, 0x0012, 0x0013, 0x0015, 0x0016, 0x0017, 0x001b, 0x001c, 0x001d, 0x001e, 0x001f, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x002c, 0x0031, 0x0036, 0x003b, 0x0049, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, 0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f, 0x0060, 0x0061, 0x0068, 0x0069, 0x006a, 0x006c, 0x007e, 0x00a6, 0x00a7, 0x00ad, 0x00b0, 0x0106, 0x0107, 0x0113, 0x0114, 0x0115, 0x0116, 0x0119, 0x011c, 0x011d, 0x011e, 0x0120, 0x0121, 0x0131, 0x0132, 0x0143, 0x0145, 0x015a,)
	@eval @inline argstr(::Val{$x}, val, type) = statmod(val, type)
end

#««2 0x65 Immunity to effect
argstr(::Val{0x0065}, _, opcode) = (repr(Opcode(opcode);context=(:compact=>true)),)
#»»1

#»»1
end
