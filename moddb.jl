#! /usr/bin/env julia
# todo:
# yoshimoromance
# a7-banteraccelerator
#
# hotpatch to sod2bg2_iu: https://www.gibberlings3.net/forums/topic/31088-bugs-issues-or-suggestions/page/6/
modtool_no_init = true
module ModDB
include("modtool.jl")
using .ModTool: Mod, ModComponent, ModCompat,
	findmod, addmods!,extract, isextracted, update,
	write_moddb, maybe_rewrite_moddb, read_moddb, save,
	printlog, printwarn, printsim, printerr,
	MODDB, MODS, DOWN, TEMP, SELECTION
using HTTP
# Constants««
const BWS_ROOT=ENV["HOME"]*"/jeux/ciopfs/EE-Mod-Setup-Master"
const BWS_MODDB="$BWS_ROOT/App/Config/BG2EE/Mod.ini"
# const BWS_BG1IO="$BWS_ROOT/App/Config/BG1EE/InstallOrder.ini"
const BWS_BG2IO="$BWS_ROOT/App/Config/BG2EE/InstallOrder.ini"
const BWS_MODDB="$BWS_ROOT/App/Config/BG2EE/Mod.ini"
const BWS_USER="$BWS_ROOT/App/Config/User.ini"
#»»

function bws_moddb(source = BWS_MODDB, orderfile=BWS_BG2IO)
	moddb = Dict{String,Mod}()
	dict = Dict{String,String}()
	printsim("reading BWS mod database $source")
	for line in eachline(source)
		startswith(line, '[') && (dict["id"] = lowercase(line[2:end-1]); continue)
		(k, v) = split(line, r"\s*=\s*"; limit=2)
		dict[k] = v
		k == "Tra" || continue # this is the last key
		id, url, description, archive =
			dict["id"], dict["Down"], dict["Name"], dict["Save"]
		m = match(r"github.com/(([^/])*/([^/]*))", url)
		!isnothing(m) && ( url = "github:"*m.captures[1]; archive = "")
		addmods!(moddb, Mod(;id, url, description, archive, class="noclass:$id"))
	end
	printsim("reading order file $orderfile")
	for line in eachline(orderfile)
		v = split(line, ";")
		first(v) ∈ ("MUC", "STD", "SUB") || continue
		id = lowercase(v[2]); m = get(moddb, id, nothing)
		isnothing(m) && continue
		m.class = ["Initial", "Fixes", "BigQuests", "Quests", "SmallQuests",
			"NPC", "NPC", "NPC-Related", "Tweak", "Tweak", "Items",
			"Tweak", "Kits", "UI"][1+parse(Int, v[4])]
	end
	moddb
end
function mod_exclusives(mod::Mod; except = String[])
	g = modgame(mod); gamedir = GAMEDIR[g]
	extract(mod)
	ret = Pair{Int, Vector{String}}[]
	cd(MODS) do
		for c in modcomponents(mod)
			printlog("Computing exclusivity for $(mod.id):$(c.id)=$(description(c))")
			excl = String[]
			for line in eachline(ignorestatus(`weidu --game $gamedir --skip-at-view --no-exit-pause --noautoupdate --list-actions --language $(mod.sel_lang) $(mod.tp2) --force-install $(c.id)`))
				m = match(r"SIMULATE\s+(\S.*\S)\s+(\S.*)$", line); 
				isnothing(m) && continue
				if m.captures[1] == "COPY false"
					s = replace(lowercase(m.captures[2]), "override/" => "")
					any(occursin(p, s) for p in except) || push!(excl, s)
				end
			end
			!isempty(excl) && push!(ret, parse(Int, c.id) => excl)
		end
	end
	return [ id => (exclusive = excl,) for (id, excl) in ret ]
end
function import_bws_moddb(source = BWS_MODDB, dest = MODDB,
		orderfile = BWS_BG2IO; simulate=false)
	moddb = bws_moddb(source, orderfile)
	delete!(moddb, "weidu")
	delete!(moddb, "weidu64")
	delete!(moddb, "zzzz")
	printlog(length(moddb), " mods read")
	printlog("adding new mods")
	function mkmod(id, url, description, class, archive = "")
		m = match(r"github.com/(([^/])*/([^/]*))", url)
		# use github urls whenever possible
		if startswith(url, "github:")
			archive = ""
		elseif startswith(url, "weaselmods:")
			req = HTTP.get("https://downloads.weaselmods.net/download/"*url[12:end];
				status_exception=false)
			for line in split(String(req.body), '\n')
	# 		for line in eachline(`wget https://downloads.weaselmods.net/download/$(url[12:end]) -O -`)
				x = match(r"https://[^\"]*\?wpdmdl=\d+", line)
				x  == nothing && continue
				url = x.match; break
			end
			@assert startswith(url, "https://")
			archive = id*".zip"
		elseif isempty(archive)
			archive = id*match(r"\.[^.]*$", url).match
		end
		addmods!(moddb, Mod(;id, url, description, class, archive))
	end
	# Argent77 ««2
	mkmod("a7-convenienteenpcs", "github:Argent77/A7-NoEENPCs",
		"Convenient EE NPCs", "NPC-Related")
	mkmod("extraexpandedenhancedencounters", "https://forums.beamdog.com/uploads/editor/ta/auj7pwad39pd.zip", "Extra Expanded Encounters", "Quests")
	# ArtemiusI ««2
	mkmod("housetweaks", "github:ArtemiusI/House-Rule-Tweaks",
		"House Rule Tweaks", "Tweak")
	# Spellhold Studios ««2
	mkmod("saradas_magic", "https://www.mediafire.com/download/rg5cyypji1om22o/Saradas%2520Magic%2520ENG%2520V_1.1.zip", "Saradas Magic", "Items")
	mkmod("saradas_magic_2", "github:SpellholdStudios/Saradas_Magic_for_BG2", "Saradas BG2", "Spells")
	mkmod("btl", "github:SpellholdStudios/Beyond_the_Law", "Beyond the Law", "NPC")
	mkmod("kiara-zaiya", "github:SpellholdStudios/Kiara-Zaiya", "Kiara Zaiya (CG HE wild mage, NE human monk)", "NPC")
# 	mkmod("iylos", "github:SpellholdStudios/Iylos", "Iylos (ToB monk)", "NPC")
	mkmod("firkraag", "github:SpellholdStudios/Super_Firkraag", "Super Firkraag", "Quests")
	mkmod("ruad", "github:SpellholdStudios/RuadRofhessaItemUpgrade", "Ruad Ro'fhessa Item Upgrade", "Items")
	mkmod("bolsa", "github:SpellholdStudios/Bolsa", "Bolsa (merchant)", "Items")
# 	next 2 mods are broken in EE games:
# 	mkmod("RPG-KP", "github:SpellholdStudios/RPG_Dungeon_Kit_Pack",
# 		"RPG Dungeon kit pack", "Kits")
# 	mkmod("RItemPack", "github:SpellholdStudios/RPG_Dungeon_Item_Pack",
# 		"RPG Dungeon item pack", "Items")
	# Gibberlings3 ««2
	mkmod("wheels", "github:Gibberlings3/WheelsOfProphecy", "Wheels of Prophecy", "Quests")
	mkmod("forgotten-armament", "github:Gibberlings3/Forgotten-Armament", "Forgotten Armament", "Items")
	mkmod("skills-and-abilities", "github:Gibberlings3/Skills-and-Abilities", "Skills and Abilities", "Kits")
	mkmod("c#sodboabri", "github:Gibberlings3/The_Boareskyr_Bridge_Scene", "The Boareskyr Bridge Scene", "Quests")
	mkmod("sodrtd", "github:Gibberlings3/Road_To_Discovery_for_SoD", "Road to Discovery", "Quests")
	mkmod("c#endlessbg1", "github:Gibberlings3/EndlessBG1", "Endless BG1", "Quests")
	mkmod("dw_lanthorn", "github:Gibberlings3/Restored-Rhynn-Lanthorn", "Restored Rhynn Lanthorn quest", "Quests")
	mkmod("druidsor", "github:Gibberlings3/Geomantic_Sorcerer", "Geomantic Sorcerer", "Kits")
	mkmod("valhorn", "github:Gibberlings3/Improved_Horns_of_Valhalla", "Improved Horn of Valhalla", "Items")
	mkmod("transitions", "github:Gibberlings3/transitions", "Transitions", "Fixes")
	# Gitjas ««2
	mkmod("c#brandock", "github:Gitjas/Brandock_the_Mage", "Brandock the Mage", "NPC")
	mkmod("c#solaufein", "github:Gitjas/Solaufeins_Rescue_NPC", "Solaufein's Rescue", "NPC")
	mkmod("c#brage", "github:Gitjas/Brages_Redemption", "Brage's Redemption", "NPC")
	# Github misc. ««2
	mkmod("d0tweak", "github:Pocket-Plane-Group/D0Tweak", "Ding0 Tweak Pack", "Tweak")
# 	mkmod("rttitempack", "github:GwendolyneFreddy/ReturnToTrademeet_ItemPack",  "Return to Trademeet item pack", "Items") # no tp2...
	mkmod("nanstein", "github:GwendolyneFreddy/Nanstein", "Nanstein item upgrade", "Items")
	mkmod("iwditempack", "github:GwendolyneFreddy/IWD_Item_Pack", "IWD item pack", "Items")
	mkmod("aurora", "github:Sampsca/Auroras-Shoes-and-Boots", "Aurora's shoes and boots", "Items")
	mkmod("monasticorders", "github:aquadrizzt/MonasticOrders", "Monastic Orders", "Kits")
	mkmod("deitiesoffaerun", "github:Raduziel/Deities-Of-Faerun", "Deities of Faerun", "Kits")
	mkmod("tnt", "github:BGforgeNet/bg2-tweaks-and-tricks", "Tweaks and Tricks", "Tweak")
	mkmod("mih_sp", "github:AngelGryph/MadeInHeaven_SpellPack", "Made In Heaven: Spell Pack", "Spells")
	mkmod("mih_eq", "github:AngelGryph/MadeInHeaven_EncountersAndQuests", "Made In Heaven: Encounters and Quests", "Quests")
	mkmod("themed_tweaks", "github:lzenner/themed_tweaks", "Themed tweaks", "Tweak")
	# Weasel mods ««2
	mkmod("thevanishingofskiesilvershield", "weaselmods:the-vanishing-of-skie-silvershield", "The vanishing of Skie Silvershield", "NPC")
	mkmod("bristlelick", "weaselmods:bristlelick", "Bristlelick (gnoll NPC)", "NPC")
	mkmod("walahnan", "weaselmods:walahnan", "Walahnan (gnome chronomancer)", "NPC")
	mkmod("ofheirloomsandclasses", "weaselmods:of-heirlooms-and-classes", "Of Heirlooms and Classes", "Items")
	mkmod("faldornbg2", "weaselmods:faldorn-bg2ee", "Faldorn BG2", "NPC")
	mkmod("khalidbg2", "weaselmods:khalid-bg2", "Khalid BG2", "NPC")
	mkmod("gahesh", "weaselmods:gahesh", "Gahesh (LG half-orc sorcerer)", "NPC")
	# Misc. ««2
	mkmod("mortis", "https://download1648.mediafire.com/7plvylpx6xbg/lspfz2ctae51735/Mortis+Mini+Mod+2.33.zip", "Mortis Mini Mod", "Items")
	mkmod("unique_items", "https://forums.beamdog.com/uploads/editor/az/70fwogntemm8.zip", "BGEE/SOD Item replacement fun pack", "Items")
	mkmod("arcanearcher", "www.shsforums.net/files/download/994-arcane-archer/", "Arcane Archer", "Kits")
	#»»2
	printlog("  now $(length(moddb)) mods stored")
	# Tweak mod descriptions««
	moddb["acbre"].description = "Breagar (LG dwarf blacksmith)"
	moddb["adrian"].description = "Adrian (LE half-elf mage)"
	moddb["amber"].description = "Amber (CG tiefling rogue)"
	moddb["angelo"].description = "Angelo (CN human fighter)"
	moddb["anishai"].description = "Anishai (CN/LE human monk)"
	moddb["aranw"].description = "Aran Whitehand (human fighter)"
	moddb["arath_eet"].description = "Arath (human druid)"
	moddb["ariena"].description = "Ariena (Aasimar half-orc)"
	moddb["aura_bg1"].description = "Aura (LG gnome artificer)"
	moddb["aurenaseph"].description = "Auren Aseph (fighter)"
	moddb["asharnpc"].description = "Ashar (half-orc barbarian)"
	moddb["calin"].description = "Calin (NG human Blade Master)"
	moddb["chloe"].description = "Chloe (N human kensai)"
	moddb["drake"].description = "Drake (priest of Tyr)"
	moddb["ehlastra"].description = "Ehlastra (LE human berserker)"
	moddb["evandra"].description = "Evandra (N elven illusionist)"
	moddb["faren"].description = "Faren (TN Fighter/Thief)"
	moddb["finchnpc"].description = "Finch (NG gnome cleric)"
	moddb["gavin"].description = "Gavin (cleric of Lathander): BG1 content"
	moddb["gavin_bg2"].description = "Gavin (clefic of Lathander): BG2 content"
	moddb["haldamir"].description = "Haldamir (CN elven fighter)"
	moddb["indinpc"].description = "Indira (LG half-elf fighter/mage)"
	moddb["isra"].description = "Isra (paladin of Sune): BG1 content"
	moddb["isra_bg2"].description = "Isra (paladin of Sune): BG2 content"
	moddb["keto"].description = "Keto (NG human Bard)"
	moddb["kido"].description = "Kido (CE Jester)"
	moddb["lena"].description = "Lena (E tiefling warrior)"
	moddb["luxleysoa"].description = "Luxley family (CN playwright, LN monk)"
	moddb["mur'neth"].description = "Mur'Neth (CE Ghaunadan/ooze thief)"
	moddb["nathaniel"].description = "Nathaniel (LG human fighter)"
	moddb["ninde_eet"].description = "Ninde (NE elven necromancer)"
	moddb["orelios"].description = "Orelios (monk)"
	moddb["sirene"].description = "Sirene (tiefling paladin of Ilmater)"
	moddb["star"].description = "Silver Star (NE elf assassin)"
	moddb["thebeaurinlegacy"].description = "The Beaurin Legacy (NE elven Enchanter/Thief)"
	moddb["theundying"].description = "The Undying (CE elven berserker, LN elven fighter/mage)"
	moddb["vienxay"].description = "Vienxay (NE elf shadow-mage)"
	moddb["vynd"].description = "Vynd (evil drow assassin)"
	moddb["white"].description = "White (CN human barbarian)"
#»»
	# Tweak mod urls ««
	for (k, v) in (
	"a7#improvedshamanicdance" => "github:Argent77/A7-ImprovedShamanicDance",
	"a7#improvedarcher" => "github:Argent77/A7-ImprovedArcher",
	"a7-chaossorcerer" => "github:Argent77/A7-ChaosSorcerer",
	"hq_soundclips_bg2ee" => "github:Argent77/HQ-SoundClips-BG2EE",
	"djinnicompanion" => "github:Argent77/DjinniCompanion",
	"recoloredbuttons" => "github:Argent77/A7-recoloredbuttons",
	"lightingpack" => "github:Argent77/A7-LightingPackEE",
	"a7-banteraccelerator" => "github:Argent77/A7-BanterAccelerator",
	"a7-golemconstruction" => "github:Argent77/A7-GolemConstruction",
	"hiddengameplayoptions" => "github:Argent77/A7-HiddenGameplayOptions",
	"orelios" => "github:PaulaMigrate/Orelios",
	"slandor" => "github:SpellholdStudios/The_Minotaur_and_Lilacor", # SIC!
	"bank_of_baldurs_gate" => "github:SpellholdStudios/Bank_of_Baldurs_Gate",
	"star" => "github:SpellholdStudios/Silver_Star_NPC",
	"fadingpromises" => "github:SpellholdStudios/Fading_Promises",
	"lucy" => "github:SpellholdStudios/Lucy_the_Wyvern",
	"bg1npc" => "github:Gibberlings3/BG1NPC",
	"underrep" => "github:Pocket-Plane-Group/Under-Respresented_Items", #SIC!
	"banterpack" => "github:Pocket-Plane-Group/Banter_Pack",
	"dc" => "github:Pocket-Plane-Group/Dungeon_Crawl",
	"kelsey" => "github:Pocket-Plane-Group/Kelsey",
	"keto" => "github:Pocket-Plane-Group/Keto",
	"tiax" => "github:Pocket-Plane-Group/Tiax_for_BGII",
	"reunion" => "github:Pocket-Plane-Group/Reunion",
	"backbrynnlaw" => "github:Pocket-Plane-Group/Back_to_Brynnlaw",
	"coran" => "github:Pocket-Plane-Group/Coran_for_BGII",
	"sellswords" => "github:Pocket-Plane-Group/Sellswords",
	"npcflirt" => "github:Pocket-Plane-Group/NPC_Flirt_Packs",
	"xanbg1friend" => "github:Pocket-Plane-Group/Xan_BG1_Friendship",
	"xpmod" => "github:Pocket-Plane-Group/D0XPmod",
	"wildmage" => "github:BGforgeNet/bg2-wildmage",
	"spiritwalker" => "github:thisisulb/SpiritwalkerKit",
	"evandra" => "github:MattyGroove/Evandra",
	"npc_strongholds" => "https://www.gibberlings3.net/files/file/959-npc-strongholds/",
	"bridgesblock" => "weaselmods:bridges-block",
	"willowisp" => "weaselmods:will-of-the-wisps",
	"tangledisle" => "weaselmods:tangled-oak-isle",
	"ooze" => "weaselmods:oozes-lounge",
	"southernedge" => "weaselmods:southern-edge",
	"totdg" => "weaselmods:tales-of-the-deep-gardens",
	"innershade" => "weaselmods:innershade",
	"yvette" => "weaselmods:yvette-romance",
	"foundling" => "weaselmods:foundling-between-the-shades",
	"whitequeen" => "weaselmods:the-white-queen",
	"isnf" => "weaselmods:i-shall-never-forget",
	"eilistraee" => "weaselmods:eilistraees-song",
	"skiecost" => "weaselmods:skie-the-cost-of-one-girls-soul",
	"sotsc" => "weaselmods:shades-of-the-sword-coast",
	"white" => "weaselmods:white-npc",
	"verrsza" => "weaselmods:verrsza-bg1ee",
	"viconiarevamped" => "weaselmods:viconia-revamped",
	"weasels!" => "weaselmods:weasels",
	"yoshimoromance" => "weaselmods:yoshimo-romance",
	"wilsonchronicles" => "weaselmods:wilson-chronicles",
	"hephernaanbg2" => "weaselmods:hephernaan-bg2",
	"petsy" => "weaselmods:petsy",
	"varshoon" => "weaselmods:varshoon",
	"quayle" => "weaselmods:quayle-bg2",
	"nathaniel" => "http://www.shsforums.net/files/download/734-nathaniel/",
	"faren" => "http://www.shsforums.net/files/download/1125-faren/",
	"npckit" => "https://www.gibberlings3.net/files/file/793-npc-kitpack",
	"sagaman" => "https://www.blackwyrmlair.net/~rabain/SagaMaster/Ulrien-SagaMaster.zip",
	); moddb[k].url = v; end
	#»»
	# Tweak mod classes ««
	printlog("modifying mod classes")
	moddb["dlcmerger"].class="DlcMerger"
	moddb["ascension"].class="BigQuests"
	moddb["azengaard"].class="Quests"
	moddb["bg1aerie"].class="NPC-Related"
	moddb["butchery"].class="Quests"
	moddb["bwfixpack"].class="Fixes"
	moddb["chattyimoen"].class="NPC-Related"
	moddb["d0questpack"].class="Quests"
	moddb["eet_end"].class="Final"
	moddb["eetact2"].class="Quests"
	moddb["faiths_and_powers"].class="Kits"
	moddb["fnp_multiclass"].class="Tweak"
	moddb["hammers"].class="Items"
	moddb["imnesvale"].class="Quests"
	moddb["impasylum"].class="Quests"
	moddb["item_rev"].class="Items"
	moddb["kelsey"].class="NPC"
	moddb["paintbg"].class="UI"
	moddb["ntotsc"].class="Quests"
	moddb["rr"].class="Tweak"
	moddb["spell_rev"].class="Spells"
	moddb["stratagems"].class="Late"
	moddb["tdd"].class="BigQuests"
	moddb["the_horde"].class="Quests"
	moddb["turnabout"].class="Quests"
	moddb["vienxay"].class="NPC"
	moddb["kivan"].class="NPC-Related"
	moddb["wtpfamiliars"].class="Tweak"
	# »»
		# Tweak mod readme««
	printlog("hardcoding mod readme")
	moddb["bggoeet"].readme = "https://www.shsforums.net/topic/56410-what-is-it/"
	moddb["bom"].readme = "https://sorcerers.net/Games/BG2/bomip-docs/index.html"
	moddb["faiths_and_powers"].readme = "https://www.gibberlings3.net/forums/topic/30792-unearthed-arcana-presents-faiths-powers-gods-of-the-realms/"
	moddb["tnt"].readme = "https://github.com/BGforgeNet/bg2-tweaks-and-tricks/tree/master/docs"
	moddb["epicthieving"].readme = "https://forums.beamdog.com/discussion/74158/v3-5-epic-thieving-more-benefits-from-high-thieving-skills"
	moddb["3ed"].readme = "https://github.com/Holic75/Baldurs-gate-dnd-3.5#readme"
	moddb["wildmage"].readme = "https://github.com/BGforgeNet/bg2-wildmage"
	moddb["artisanskitpack"].readme = "https://artisans-corner.com/the-artisans-kitpack/"
	moddb["monasticorders"].readme = "https://forums.beamdog.com/discussion/18620/mod-beta-monastic-orders-of-faerun"
	moddb["mercenary"].readme = "https://forums.beamdog.com/discussion/68151/fighter-kit-mercenary-v3-1-iwdee-eet-bgee-bg2ee"
	moddb["kale"].readme = "https://forums.beamdog.com/discussion/74630/v1-67-kale-a-halfling-barbarian-for-bg-ee-sod-and-eet"
	moddb["karatur"].readme = "https://forums.beamdog.com/discussion/15959/mod-twas-a-slow-boat-from-kara-tur-queststorenew-items-release-90/p1"
	moddb["vaulteet"].readme = "https://www.angelfire.com/rpg2/azenmod/ReadMe-Vault.htm"
	moddb["verrsza"].readme = "https://forums.beamdog.com/discussion/60614/mod-verrsza-npc-for-bgee-and-sod"
	moddb["ninde_eet"].readme = "https://spellholdstudios.github.io/readmes/ninde-readme-english.html"
	moddb["vampiretales"].readme = "https://mods.chosenofmystra.net/vampiretales/"
	moddb["unique_items"].readme = "https://forums.beamdog.com/discussion/47005/bgee-sod-item-replacement-fun-pack-v2-1-completed"
	moddb["npc_strongholds"].readme = "https://gibberlings3.github.io/Documentation/readmes/readme-npc_strongholds.html"
	#»»
	moddb["arcanearcher"].archive = "arcanearcher.zip"
	printlog("setting mod component properties")
	function createcomponent!(m::Mod, k)
		isempty(k) && return m.compat
		comp = m.components
		j = findfirst(c->c.id == k, comp)
		!isnothing(j) && return comp[j]
		push!(comp, ModComponent(k, ""))
		return last(comp)
	end
	function setmod!(id, list...)
		m = findmod(id; moddb)
		for (i, kv) in list;
			if i isa Integer # this describes a component
				i = string(i)
				c = createcomponent!(m, i)
				for (k, v) in pairs(kv)
					if k == :path
						!isempty(c.path) && printwarn("warning, '$id:$i'.path is not empty")
						c.path = v
					else
						push!(getfield(c,Symbol(k)), [v;]...)
					end
				end
			else # this describes a compatibility group
				c = ModCompat(i, get(kv,:after,[]), get(kv,:before,[]))
				if isempty(i)
					m.compat[1] = c
				else
					push!(m.compat, c)
				end
			end
		end
	end
	setmod!("7c-yoshi",
		5 => (path=["NPC", "Yoshimo"],),
		10	 => (path=["NPC", "Yoshimo"],),
	)
	setmod!("a7#improvedarcher",
		0 => (path=["Classes", "Ranger"],),
		10 => (path=["Classes", "Fighter"],),
		20 => (path=["Classes", "Fighter"],),
		30 => (path=["Classes", "Fighter"],),
		100 => (path=["Items", "Ammunition"],),
	)
	setmod!("a7#improvedshamanicdance",
		0 => (path=["Classes", "Shaman"],),
		10 => (path=["Classes", "Shaman"],),
		20 => (path=["Classes", "Shaman"],),
		30 => (path=["Classes", "Shaman"],),
		40 => (path=["Items"],),
	)
	setmod!("ajantisbg1",
		0 => (path = ["NPC", "Ajantis"],),
		1 => (path = ["NPC", "Ajantis"],),
	)
	setmod!("ajantisbg2",
		0 => (path = ["NPC", "Ajantis"],),
		1 => (path = ["NPC", "Ajantis"],),
		3 => (path = ["NPC", "Ajantis"],),
		41 => (path = ["NPC", "Ajantis"],),
		42 => (path = ["NPC", "Ajantis"],),
		43 => (path = ["NPC", "Ajantis"],),
		44 => (path = ["NPC", "Ajantis"],),
		5 => (path = ["NPC", "Ajantis"],),
	)
	setmod!("alora", 0 => (path=["NPC", "Alora"],))
	setmod!("animalcompanions",
		0 => (path=["Classes", "Ranger"],),
	)
	setmod!("arcanearcher", 0 => (path=["Classes", "Ranger"],))
	setmod!("ariena", "" => (after = ["Kido"],))
	setmod!("artisanskitpack",
		"" => (after=["emily", "skitianpcs", "ajantisbg1", "ajantisbg2", ],),
		1 => (path=["Classes", "Restrictions"],),
		2 => (path=["Classes", "Restrictions"],),
		3 => (path=["Classes", "Restrictions"],),
		1100 => (path=["Classes", "Fighter"],),
		1003 => (path=["Classes", "Fighter"],),
		1006 => (path=["Classes", "Fighter"],),
		1004 => (path=["Classes", "Fighter"],),
		1005 => (path=["Classes", "Fighter"],),
		1007 => (path=["Classes", "Fighter"],),
		1002 => (path=["Classes", "Fighter"],),
		1000 => (path=["Classes", "Fighter"],),
		1001 => (path=["Classes", "Fighter"],),
		1102 => (path=["New NPC"],),
		1103 => (path=["New NPC"],),
		1104 => (path=["New NPC"],),
		1105 => (path=["New NPC"],),
		1101 => (path=["NPC", "Khalid"],),
		2000 => (path=["Classes", "Ranger"],),
		2010 => (path=["Classes", "Ranger"],),
		2011 => (path=["Classes", "Ranger"],),
		2012 => (path=["Classes", "Ranger"],),
		2001 => (path=["NPC", "Minsc"],),
		2002 => (path=["Classes", "Ranger"],),
		3000 => (path=["Classes", "Paladin"],),
		3010 => (path=["Classes", "Paladin"],),
		3003 => (path=["Classes", "Paladin"],),
		3011 => (path=["Classes", "Paladin"],),
		3004 => (path=["Classes", "Paladin"],),
		3001 => (path=["Classes", "Paladin"],),
		3101 => (path=["NPC", "Ajantis"],),
		3002 => (path=["Classes", "Paladin"],),
		3005 => (path=["Classes", "Paladin"],),
		5001 => (path=["Classes", "Druid"],),
		5101 => (path=["NPC", "Cernd"],),
		5002 => (path=["Classes", "Druid"],),
		5003 => (path=["Classes", "Mage"],),
		7001 => (path=["Classes", "Thief"],),
		7101 => (path=["NPC", "Imoen"],),
		7002 => (path=["Classes", "Thief"],),
		7003 => (path=["Classes", "Thief"],),
		7203 => (path=["Classes", "Thief"],),
		7102 => (path=["NPC", "Imoen"],),
		7004 => (path=["Classes", "Thief"],),
		7103 => (path=["NPC", "Montaron"],),
		7005 => (path=["Classes", "Thief"],),
		8001 => (path=["Classes", "Sorcerer"],),
		8002 => (path=["Classes", "Sorcerer"],),
		8003 => (path=["Classes", "Sorcerer"],),
		9001 => (path=["Classes", "Shaman"],),
		9101 => (path=["NPC", "M'khiin"],),
		10001 => (path=["Classes", "Monk"],),
		20000 => (path=["Classes", "Multiclass"],),
		20001 => (path=["Classes", "Multiclass"],),
	)
	setmod!("ascension",
		0 => (path=["Story", "ToB"],),
		10 => (path=["Story", "ToB"],),
		20 => (path=["NPC", "Sarevok"],),
		30 => (path=["NPC", "Imoen"],),
		40 => (path=["Skills", "Bhaalspawn"],),
		50 => (path=["Skills", "Bhaalspawn"],),
		60 => (path=["NPC"],),
		61 => (path=["NPC"],),
# 		1000 => (path=["Story", "ToB"],), # FIXME, should have Tactics instead
		2200 => (path=["NPC"],),
		2000 => (path=["Cosmetic", "Portraits"],),
		2100 => (path=["Cosmetic", "Portraits"],),
		2300 => (path=["Cosmetic", "Portraits"],),
		2400 => (path=["Story", "ToB"],),
	)
	setmod!("atweaks",
		"" => (after = ["rr", "stratagems"],),
		100 => (path = ["Skills", "Infravision"],),
		101 => (path = ["Creatures"],),
		102 => (exclusive="shammr.itm", path = ["Items", "Summoned weapons"],),
 103 => (exclusive=["spwi406.spl", "spwi602.spl"], path = ["Spells", "Abjuration"],),
 104 => (exclusive = ["spwi105.spl", "spin937.spl"],path=["Spells", "Alteration"],),
 105 => (exclusive = ["spwi402.spl"], path=["Spells", "Alteration"],),
 110 => (path = ["Items"],),
 115 => (path = ["Skills", "Scribing"],),
 117 => (path = ["Skills", "Scribing"],),
 120 => (path = ["Classes", "Paladin"],),
 125 => (path = ["Skills", "Charm animal"],),
 130 => (path = ["Tables", "Races"],),
#  135 => (path = ["Tables", "Races"],),
 140 => (path = ["Tables", "Races"],),
 150 => (path = ["Creatures", "Fiends"],),
 152 => (path = ["Creatures", "Fiends"],),
 153 => (path = ["Creatures", "Fiends"],),
 155 => (path = ["Creatures", "Fiends"],),
 156 => (path = ["Creatures", "Fiends"],),
 160 => (path = ["Creatures", "Undead"],),
 180 => (path = ["Creatures", "Fiends"],),
 185 => (path = ["Creatures"],),
 186 => (exclusive = "sppr410.spl", path = ["Spells", "Conjuration"],),
 190 => (path = ["Creatures", "Elementals"],),
 191 => (path = ["Creatures", "Elementals"],),
 200 => (path = ["Spells", "Abjuration"],),
 201 => (path = ["Classes", "Fighter"],),
 202 => (exclusive = ["spin101.spl", "spin104.spl", "spin102.spl", "spin105.spl", "spin106.spl"], path=["Skills", "Bhaalspawn"],),
 203 => (path = ["Skills", "Shapeshifting"],),
 204 => (path = ["Spells", "Illusion"],),
 205 => (path = ["Spells", "Illusion"],),
 210 => (path = ["Creatures"],),
 211 => (exclusive = "sppr409.spl", path = ["Spells", "Abjuration"],), # death ward
 212 => (exclusive = "sppr209.spl", path = ["Spells", "Divination"],), # know alignment
 213 => (path = ["Tables", "Saving throws"],),
 216 => (path = ["Classes", "Bard"],),
 217 => (path = ["Classes", "Bard"],),
 218 => (path = ["Skills", "Bhaalspawn"],),
 220 => (path = ["Classes", "Thief"],),
 230 => (path = ["Classes", "Bard"],),
 239 => (path = ["Classes", "Cleric"],),
 241 => (path = ["Skills", "Bhaalspawn"],),
 261 => (path = ["Tables", "Bonus XP"],),
 262 => (path = ["Tables", "Bonus XP"],),
 270 => (path = ["Tables", "XP", "Quest"],),
 300 => (exclusive = ["spdimndr.bam", "eff_m09.wav"], path = ["Cosmetic", "Spells"],),
 301 => (path = ["Items"],),
 302 => (path = ["Items"],),
 310 => (path = ["Cosmetic", "Sprites"],),
 315 => (path = ["Cosmetic", "Sound"],),
 322 => (exclusive = ["spdimndr.bam", "eff_m09.wav"], path = ["Cosmetic", "Spells"],),
 323 => (exclusive = ["spwi402.spl"], path = ["Cosmetic", "Spells"]),
 324 => (exclusive = ["spdimndr.bam", "eff_m09.wav"], path=["Cosmetic", "Spells"]),
 500 => (path = ["Items", "Containers"],),
 502 => (path = ["Items", "Containers"],),
 510 => (path = ["Items", "Stores"],),
 999 => (path = ["Cosmetic", "Icons"],),
	)
	setmod!("banterpack",
		0 => (path=["NPC", "Banter"],),
		1 => (path=["NPC", "Banter"],),
		2 => (path=["NPC", "Banter"],),
		3 => (path=["NPC", "Banter"],),
	)
	setmod!("bg1aerie", 0 => (path = ["NPC", "Aerie"],))
	setmod!("bg1npc",
		 0 => (path=["NPC"],),
		 10 => (path=["NPC"],),
		 20 => (path=["NPC", "Ajantis"],),
		 21 => (path=["NPC", "Ajantis"],),
		 22 => (path=["NPC", "Ajantis"],),
		 23 => (path=["NPC", "Ajantis"],),
		 24 => (path=["NPC", "Ajantis"],),
		 30 => (path=["NPC", "Branwen"],),
		 31 => (path=["NPC", "Branwen"],),
		 32 => (path=["NPC", "Branwen"],),
		 33 => (path=["NPC", "Branwen"],),
		 34 => (path=["NPC", "Branwen"],),
		 40 => (path=["NPC", "Coran"],),
		 41 => (path=["NPC", "Coran"],),
		 42 => (path=["NPC", "Coran"],),
		 43 => (path=["NPC", "Coran"],),
		 44 => (path=["NPC", "Coran"],),
		 50 => (path=["NPC", "Dynaheir"],),
		 51 => (path=["NPC", "Dynaheir"],),
		 52 => (path=["NPC", "Dynaheir"],),
		 53 => (path=["NPC", "Dynaheir"],),
		 54 => (path=["NPC", "Dynaheir"],),
		 60 => (path=["NPC", "Shar-Teel"],),
		 61 => (path=["NPC", "Shar-Teel"],),
		 62 => (path=["NPC", "Shar-Teel"],),
		 63 => (path=["NPC", "Shar-Teel"],),
		 64 => (path=["NPC", "Shar-Teel"],),
		 70 => (path=["NPC", "Xan"],),
		 71 => (path=["NPC", "Xan"],),
		 72 => (path=["NPC", "Xan"],),
		 73 => (path=["NPC", "Xan"],),
		 74 => (path=["NPC", "Xan"],),
		 80 => (path=["NPC"],),
		 90 => (path=["NPC", "Wait"],),
		 100 => (path=["NPC", "Banter"],),
		 110 => (path=["NPC", "Banter"],),
		 111 => (path=["NPC", "Banter"],),
		 112 => (path=["NPC", "Banter"],),
		 113 => (path=["NPC", "Banter"],),
		 114 => (path=["NPC", "Banter"],),
		 120 => (path=["Skills", "Reputation"],),
		 130 => (path=["NPC", "Sarevok"],),
		 131 => (path=["NPC", "Sarevok"],),
		 240 => (path=["NPC", "Kivan"],),
		 241 => (path=["NPC", "Kivan"],),
		 150 => (path=["NPC", "Kivan"],),
		 155 => (path=["NPC", "Coran"],),
		 160 => (path=["Cosmetic", "Portraits"],),
		 200 => (path=["NPC"],),
	),
	setmod!("branwen", 0 => (path = ["NPC", "Branwen"],))
	setmod!("c#endlessbg1",
		0 => (path=["Story", "BG1", "Ending"],),
		1 => (path=["Story", "BG1", "Ending"],),
		2 => (path=["Story", "BG1", "Ending"],),
		3 => (path=["Story", "BG1", "Ending"],),
		4 => (path=["Story", "BG1", "Ending"],),
		5 => (path=["Story", "BG1", "Ending"],),
		6 => (path=["Story", "BG1", "Ending"],),
		7 => (path=["Story", "BG1", "Ending"],),
		8 => (path=["Story", "BG1", "Ending"],),
		9 => (path=["Story", "BG1", "Ending"],),
		10 => (path=["Story", "BG1", "Ending"],),
		11 => (path=["Story", "BG1", "Ending"],),
		12 => (path=["Story", "BG1", "Ending"],),
		13 => (path=["Story", "BG1", "Ending"],),
		14 => (path=["Story", "SoD"],),
		15 => (path=["Story", "SoD"],),
		16 => (path=["Story", "SoD"],),
	)
	setmod!("c#sodboabri",
		0 => (path=["Story", "SoD"],),
		1 => (path=["Story", "SoD"],),
		2 => (path=["Story", "SoD"],),
		3 => (path=["Story", "SoD"],),
	)
	setmod!("c0warlock", 0 => (path = ["Classes"],))
	setmod!("cdtweaks",
		"" => (after = ["tomeandblood", "bg1npc", "thecalling", "item_rev", "divine_remix"],),
		10 => (path = ["Cosmetic", "Sprites"],),
		20 => (path = ["NPC", "Imoen", "Appearance"],),
		30 => (path = ["NPC", "Nalia", "Appearance"],),
		40 => (path = ["NPC", "Viconia", "Appearance"],),
		50 => (path = ["Cosmetic", "Sprites"],),
		60 => (path = ["Cosmetic", "Sprites"],),
		70 => (path = ["Cosmetic", "Sprites"],),
		72 => (path = ["Cosmetic", "Sprites"],),
		90 => (path = ["Cosmetic", "Portraits"],),
		100 => (path = ["Cosmetic", "Sprites"],),
		110 => (path = ["Cosmetic", "Icons"],),
		140 => (path = ["Cosmetic", "Sound"],),
		160 => (path = ["Cosmetic", "Sprites"],),
		170 => (path = ["Cosmetic", "Icons"],),
		171 => (path = ["Cosmetic", "Icons"],),
		180 => (path = ["Items", "Containers"],),
		181 => (path = ["Items", "Containers"],),
		182 => (path = ["Items", "Containers"],),
		190 => (path = ["Cosmetic", "Sprites"],),
		191 => (path = ["Cosmetic", "Sprites"],),
		192 => (path = ["Cosmetic", "Sprites"],),
		193 => (path = ["Cosmetic", "Sprites"],),
		194 => (path = ["Cosmetic", "Sprites"],),
		195 => (path = ["Cosmetic", "Sprites"],),
		200 => (path = ["Cosmetic", "Sprites"],),
		3150 => (path = ["Cosmetic", "Sprites"],),
		3151 => (path = ["Cosmetic", "Sprites"],),
		2010 => (path = ["Cosmetic", "Portraits"],),
		1010 => (path = ["NPC"],),
		1020 => (path = ["NPC"],),
		1030 => (path = ["Story", "BG1"],),
		1035 => (path = ["Story", "BG1"],),
		1036 => (path = ["Story", "BG1"],),
		1040 => (exclusive = ["amncen1.cre", "amng1.cre" ], path=["Creatures"],), #(Etc.) Amn guards
		1050 => (path = ["Items"],),
		1060 => (path = ["Items"],),
		1075 => (path = ["NPC", "Wait"],),
		1080 => (path = ["Items", "Containers"],),
		1100 => (path = ["Cosmetic", "Maps"],),
		1101 => (path = ["Cosmetic", "Maps"],),
		1110 => (path = ["Cosmetic", "Maps"],),
		1120 => (path = ["Items", "Stores"],),
		1130 => (path = ["Skills", "Reputation"],), # reputation reset...
		1130 => (exclusive = "reputation_reset",),
		1150 => (exclusive = ["wwbearwe.cre", "wwbear.cre", "sppr604.spl", "spcl643.spl", "spcl644.spl", "anisum04.2da", "anisum05.2da"],
			path = ["Skills", "Shapeshifting"],),
		1160 => (path = ["Story", "BG2", "Stronghold"],),
		1161 => (path = ["Story", "BG2", "Stronghold"],),
		1180 => (path = ["NPC", "Edwin"],),
		1200 => (path = ["NPC", "Imoen"],),
		1220 => (path = ["Items", "Upgrades"],),
		1225 => (path = ["Items", "Upgrades"],),
		1226 => (path = ["Items", "Upgrades"],),
		1227 => (path = ["Items", "Upgrades"],),
		1230 => (path = ["Items", "Upgrades"],),
		1251 => (path = ["NPC", "Alora"],),
		1252 => (path = ["NPC", "Eldoth"],),
		1253 => (path = ["NPC", "Quayle"],),
		1254 => (path = ["NPC", "Shar-Teel"],),
		1255 => (path = ["NPC", "Tiax"],),
		1256 => (path = ["NPC", "Viconia"],),
		1260 => (path = ["Skills", "Reputation"],),
		1270 => (path = ["Story", "BG1"],),
		1340 => (path = ["Story", "BG2", "Stronghold"],),
		1341 => (path = ["Story", "BG2", "Stronghold"],),
		1342 => (path = ["Story", "BG2", "Stronghold"],),
		1343 => (path = ["Story", "BG2", "Stronghold"],),
		1344 => (path = ["Story", "BG2", "Stronghold"],),
		1345 => (path = ["Story", "BG2", "Stronghold"],),
		1346 => (path = ["Story", "BG2", "Stronghold"],),
		1347 => (path = ["Story", "BG2", "Stronghold"],),
		2020 => (path = ["Items", "Weapons"],),
		2030 => (path = ["Items", "Weapons"],),
		2035 => (path = ["Items", "Weapons"],),
		2040 => (exclusive = "universal_clubs", path=["Fighting", "Proficiencies"]),
		2060 => (exclusive = "universal_fighting_styles",
			path=["Fighting", "Fighting styles"]),
		2080 => (exclusive = "lunumab.2da", path=["Tables", "HLA"]),
		2090 => (exclusive= ["xplevel.2da", "hpclass.2da",], path=["Tables", "XP"]),
		2091 => (exclusive= "xplevel.2da", path=["Tables", "XP"]),
		2092 => (exclusive= "xplevel.2da", path=["Tables", "XP"]),
		2100 => (exclusive = "armored_thieving", path=["Skills", "Thieving"],),
		2120 => (exclusive = "armored_spellcasting", path=["Skills", "Casting"],),
		2140 => (path=["Classes", "Restrictions"],),
		2150 => (exclusive = "protection_items", path=["Items", "Protection"],),
		2151 => (exclusive = "protection_items", path=["Items", "Protection"],),
		2152 => (exclusive = "protection_items", path=["Items", "Protection"],),
		2160 => (exclusive="weapprof.2da/types", path=["Fighting", "Proficiencies"]),
		2161 => (exclusive="weapprof.2da/types", path=["Fighting", "Proficiencies"]),
		2162 => (exclusive="weapprof.2da/types", path=["Fighting", "Proficiencies"]),
		2163 => (exclusive="weapprof.2da/types", path=["Fighting", "Proficiencies"]),
		2164 => (exclusive="weapprof.2da/types", path=["Fighting", "Proficiencies"]),
		2170 => (path = ["Items", "Scrolls"],),
		2190 => (path = ["Items", "Stores"],),
		2191 => (path = ["Items", "Stores"],),
		2192 => (path = ["Items", "Stores"],),
		2200 => (path = ["Fighting", "Proficiencies"],),
		2210 => (exclusive="wspecial.2da/speed", path=["Fighting", "Proficiencies"]),
		2211 => (exclusive="wspecial.2da/speed", path=["Fighting", "Proficiencies"]),
		2220 => (path = ["Items", "Summoned weapons"],),
		2230 => (path = ["Items", "Magic weapons"],),
		2240 => (exclusive = "thac0.2da", path=["Tables", "THAC0"]),
		2250 => (exclusive = "mxsplsrc.2da", path=["Tables", "Spell slots"]),
		2260 => (exclusive = "mxsplwiz.2da", path=["Tables", "Spell slots"]),
		2261 => (exclusive = "mxsplwiz.2da", path=["Tables", "Spell slots"]),
		2270 => (exclusive = "mxsplbrd.2da", path=["Tables", "Spell slots"]),
		2271 => (exclusive = "mxsplbrd.2da", path=["Tables", "Spell slots"]),
		2280 => (exclusive = "mxsplclr.2da", path=["Tables", "Spell slots"]),
		2281 => (exclusive = "mxsplclr.2da", path=["Tables", "Spell slots"]),
		2290 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2291 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2292 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2293 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2294 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2295 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2296 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2297 => (exclusive = "mxspldrd.2da", path=["Tables", "Spell slots"]),
		2540 => (path = ["Story", "BG2", "Stronghold"],),
		2580 => (path = ["Tables", "Spell slots"],),
		2581 => (path = ["Tables", "Spell slots"],),
		2300 => (exclusive = ["lufmc.2da", "lufmt.2da"], path=["Tables", "HLA"]),
		2310 => (exclusive = "spell_save_penalties",),
		2311 => (exclusive = "spell_save_penalties",),
		2312 => (exclusive = "spell_save_penalties",),
		2340 => (exclusive = "summlimt.2da/celestial",),
		2350 => (path = ["Classes", "Restrictions"],),
		2351 => (path = ["Classes", "Restrictions"],),
		2353 => (path = ["Classes", "Restrictions"],),
		2357 => (path = ["Classes", "Restrictions"],),
		2358 => (path = ["Classes", "Restrictions"],),
		2360 => (path = ["Classes", "Restrictions"],),
		2370 => (exclusive = "clsrcreq.2da", path=["Classes", "Restrictions"],),
		2371 => (exclusive = "clsrcreq.2da", path=["Classes", "Restrictions"],),
		2372 => (exclusive = "clsrcreq.2da", path=["Classes", "Restrictions"],),
		2380 => (path = ["Classes", "Restrictions"],),
		2390 => (exclusive = "mxsplpal.2da", path=["Tables", "Spell slots"]),
		2391 => (exclusive = "mxsplpal.2da", path=["Tables", "Spell slots"]),
		2400 => (exclusive = "mxsplran.2da", path=["Tables", "Spell slots"]),
		2401 => (exclusive = "mxsplran.2da", path=["Tables", "Spell slots"]),
		2410 => (exclusive = "druid_alignment", path=["Classes", "Druid"],),
		2420 => (exclusive = "multiclass_cleric_weapons", path=["Classes", "Cleric"],),
		2430 => (exclusive = "multiclass_druid_weapons", path=["Classes", "Druid"],),
		2431 => (exclusive = "multiclass_druid_weapons", path=["Classes", "Druid"],),
		2440 => (exclusive = "clswpbon.2da", path=["Fighting", "Proficiencies"]),
		2450 => (path=["Classes", "Restrictions"],),
		2500 => (path = ["Tables", "Abilities"],),
		2510 => (path=["Items", "Scrolls"],),
		2520 => (path=["Items"],),
		2530 => (exclusive = "lightning_bolt", path=["Spells", "Evocation"],),
		2550 => (path=["Classes", "Restrictions"],),
		2551 => (path=["Classes", "Restrictions"],),
		2552 => (path=["Classes", "Restrictions"],),
		2560 => (path=["Classes", "Monk"],),
		2590 => (path=["Skills", "Backstab"],),
		2999 => (path=["Tables", "HP"],),
		3000 => (exclusive = "hpclass.2da", path=["Tables", "HP"]),
		3001 => (exclusive = "hpclass.2da", path=["Tables", "HP"]),
		3002 => (exclusive = "hpclass.2da", path=["Tables", "HP"]),
		3008 => (exclusive = "hpclass.2da", path=["Tables", "HP"]),
		3020 => (path=["Skills", "Lore"],),
		3030 => (path=["Skills", "Scribing"],),
		3031 => (path=["Skills", "Scribing"],),
		3040 => (exclusive = "container_capacity", path=["Items", "Containers"],),
		3050 => (path=["Spells", "Healing"],),
		3070 => (exclusive = "store_prices", path=["Items", "Stores"],),
		3071 => (exclusive = "store_prices", path=["Items", "Stores"],),
		3072 => (exclusive = "store_prices", path=["Items", "Stores"],),
		3073 => (exclusive = "store_prices", path=["Items", "Stores"],),
		3080 => (exclusive = "ammo_stacking", path=["Items", "Stacking"],),
		3081 => (exclusive = "ammo_stacking", path=["Items", "Stacking"],),
		3082 => (exclusive = "ammo_stacking", path=["Items", "Stacking"],),
		3083 => (exclusive = "ammo_stacking", path=["Items", "Stacking"],),
		3090 => (exclusive = "gem_stacking", path=["Items", "Stacking"],),
		3091 => (exclusive = "gem_stacking", path=["Items", "Stacking"],),
		3092 => (exclusive = "gem_stacking", path=["Items", "Stacking"],),
		3093 => (exclusive = "gem_stacking", path=["Items", "Stacking"],),
		3100 => (exclusive = "potion_stacking", path=["Items", "Stacking"],),
		3101 => (exclusive = "potion_stacking", path=["Items", "Stacking"],),
		3102 => (exclusive = "potion_stacking", path=["Items", "Stacking"],),
		3103 => (exclusive = "potion_stacking", path=["Items", "Stacking"],),
		3110 => (exclusive = "scroll_stacking", path=["Items", "Stacking"],),
		3111 => (exclusive = "scroll_stacking", path=["Items", "Stacking"],),
		3112 => (exclusive = "scroll_stacking", path=["Items", "Stacking"],),
		3113 => (exclusive = "scroll_stacking", path=["Items", "Stacking"],),
		3120 => (path = ["NPC", "Happy patch"],),
		3121 => (path = ["NPC", "Happy patch"],),
		3122 => (path = ["NPC", "Happy patch"],),
		3123 => (path = ["NPC", "Happy patch"],),
		3124 => (path = ["NPC", "Happy patch"],),
		3125 => (path = ["NPC", "Happy patch"],),
		3130 => (path = ["Skills", "Thieving"],),
		3140 => (path = ["Story", "BG1"],),
		3141 => (path = ["Story", "BG1"],),
		3160 => (path = ["Items"],),
		3170 => (path = ["Cosmetic", "Sprites"],),
		3200 => (path = ["Items", "Stores"],),
		3205 => (path = ["Items", "Stores"],),
		3230 => (path = ["Items", "Upgrades"],),
		3320 => (path = ["Items", "Stores"],),
		4000 => (path = ["Skills", "Reputation"],),
		4010 => (path = ["NPC"],),
		4020 => (path = ["NPC"],),
		4025 => (path = ["NPC"],),
		4030 => (path = ["NPC", "Edwin"],),
		4031 => (path = ["NPC", "Edwin"],),
		4040 => (path = ["NPC", "Jaheira"],),
		4041 => (path = ["NPC", "Jaheira"],),
		4050 => (path = ["NPC", "Jaheira"],),
		4060 => (path = ["NPC", "Minsc"],),
		4061 => (path = ["NPC", "Minsc"],),
		4070 => (path = ["NPC", "Viconia"],),
		4071 => (path = ["NPC", "Viconia"],),
		4080 => (path = ["NPC", "Khalid"],),
		4090 => (path = ["NPC", "Montaron"],),
		4100 => (path = ["NPC", "Korgan"],),
		4110 => (exclusive = "kagain.cre", path=["NPC", "Kagain"],),
		4120 => (exclusive = "coran.cre", path=["NPC", "Coran"],),
		4130 => (path = ["NPC", "Xan"],),
		4131 => (path = ["NPC", "Dynaheir"],),
		4132 => (path = ["NPC", "Xzar"],),
		4133 => (path = ["NPC", "Edwin"],),
		4150 => (path = ["NPC", "Minsc"],),
		4160 => (path = ["NPC", "Yeslick"],),
		4170 => (path = ["NPC", "Shar-Teel"],),
		4180 => (path = ["Items"],),
	)
	setmod!("cdlore",
		10 => (path=["Skills", "Lore"],),
		20 => (path=["Skills", "Lore"],),
		30 => (path=["Skills", "Lore"],),
		40 => (path=["Skills", "Lore"],),
	)
	setmod!("celestials", 0 => (path=["Creatures"],),)
	setmod!("cernd", 0 => (path = ["NPC", "Cernd"],))
	setmod!("charlatan",
		0 => (path=["Classes", "Bard"],),
		1 => (path=["NPC", "Eldoth"],),
	)
	setmod!("chattyimoen",
		0 => (path = ["NPC", "Imoen"],),
		1 => (path = ["NPC", "Imoen"],),
		2 => (path = ["NPC", "Imoen"],),
		3 => (path = ["NPC", "Imoen"],),
		4 => (path = ["NPC", "Imoen"],),
	)
	setmod!("convinientammunition", 0 => (path=["Items", "Ammunition"],),)
	setmod!("cowledmenace",
		"" => (after = ["eet"], depends = ["eet"],),
	)
	setmod!("corwineet", 0 => (path = ["NPC", "Corwin"],))
	setmod!("crossmodbg2",
		"" => (after = [ "adrian", "ajantisbg1", "ajantisbg2", "amber", "angelo", "aranw", "arath_eet", "aurenaseph", "branwen", "c#solaufein", "coran", "coranbgfriend", "dace_eet", "fade", "faren", "foundling", "gahesh", "gavin", "haerdalisromance", "haldamir", "hephernaanbg2", "hubelpot", "isra", "iylos", "kelsey", "keto", "khalidbg2", "kido", "kindrek", "kivan", "longerroadee", "luxleysoa", "neh'taniel", "ninde_eet", "petsy", "quayle", "saerileth", "sarahtob", "skiecost", "solaufein", "tashia", "thebeaurinlegacy", "tiax", "tsujatha", "varshoon", "verrsza", "willowisp", "xan", "xanbg1friend", "xulaye_eet", "yasraena", "yoshimosremorse", "yvette", ],),
	)
	setmod!("d0questpack",
		"" => (after = ["kelsey", "keto", "ub" ],),
		0 => (exclusive = "AI",),
		5 => (exclusive = ["c6kach.cre", "ar0300.bcs", "ar0300.are", "aran.cre", "maevar.cre", "mvguard1.cre", "mvpries.cre", "aran02.cre", "arnfgt03.cre", "arnfgt04.cre", "renal.cre", "thief1.cre", "tassa.cre", "c6tanov.bcs", "vvtanov.cre",], path=["Story", "BG2"],),
   6 => (exclusive = ["sw1h50.itm"], path=["Story", "BG2"],),
		8 => (exclusive = ["ar0812.are", "ar1515.bcs", "thumb.cre", ], path=["Story", "BG2"],),
		9 => (exclusive = ["potn33.itm", "potn38.itm", "spwi106.spl", "spwi815.spl",],path=["Story", "BG2"],),
  10 => (exclusive = ["hellself.eff", "spin755.spl", "spin751.spl", "sphl004.spl", "spin753.spl", "hellself.cre", "sphl003.spl", "sphl005.spl", "hellgen2.cre", "sphl001.spl", "sphl002.spl", "cutc7g.bcs", "spin749.spl", "spin747.spl"], path=["Story", "BG2", "Ending"],),
  11 => (exclusive = ["amtarc01.cre", "amtcap01.cre", "amtcle01.cre", "amtgen01.cre", "amtmag01.cre", "amtpik01.cre"], path=["Areas", "BG2", "Oasis"],),
		12 => (exclusive = ["Oasis", "ar6300.are", ], path=["Areas", "BG2", "Oasis"],),
		13 => (exclusive = ["ppsanik.cre", "pirmur02.cre", "pirmur05.cre",], path=["Story", "BG2", "Brynnlaw"],),
#   15 => (exclusive = ["suinvis.cre", "sumound.cre", "sudryad.cre", "sufake.itm", "sutear.itm", "sutear.bam", "sufake.bam"], path=["Quests", "BG2"],),
#   16 => (exclusive = ["besamen.cre", "baisera.cre"], path=["Quests", "BG2"],),
# 		17 => (exclusive = ["kamir.cre",], path=["Quests", "BG2"],),
# 		19 => (exclusive = ["ar2402.are",], path=["Quests", "BG2"],),
#   20 => (exclusive = ["spin671.spl"], path=["Quests", "BG2"],),
# 		21 => (exclusive = ["ar0530.are", "ar0530.bcs",], path=["Quests", "BG2"],),
 401 => (exclusive = ["sukiss1.cre", "sukissk.wav", "sumist.cre", "suspyim.cre", "reddeath.bcs",],),
	)
	setmod!("d0tweak",
		"11" => (after = ["rr"],),
		11 => (path=["Cosmetic", "Sprites"],), # ioun stones...
		17 => (path = ["Skills", "Lore"],),
		18 => (path = ["Skills", "Backstab"],),
	)
	setmod!("d5_random_tweaks",
		"" => (after = ["spell_rev", "item_rev"],),
		60 => (path=["NPC"],),
			1151 => (path=["Spells", "Evocation"],),
			1152 => (path=["Spells", "Evocation"],),
			2451 => (path=["Spells", "Conjuration"],),
			1451 => (path=["Spells", "Evocation"],),
			3050 => (path=["Items", "Wands"],),
# 		 2212 => (exclusive = ["spwi323.spl"],),
# 		 2914 => (exclusive = "spwi914.spl",), # black blade of disaster
# 		2105 => (exclusive = "spwi105.spl",), # Color Spray (not exclusive)
#  2811 => (exclusive = ["spwi706.spl"],),
#  3030 => (exclusive = ["b_pfire.pro", "pfirea.bam", "pfirex.bam", "#prfire.vvc", "#eff_p45.wav", "#are_p03.wav"],),
		 1202 => (exclusive = "sppr202.spl",), # barkskin
		 1202 => (path = ["Spells", "Alteration"],),
		 1207 => (exclusive = ["sppr207.spl"], path=["Spells", "Alteration"],), # goodberry
		 1212 => (exclusive = "sppr212.spl", path=["Spells", "Healing"],), # slow poison
		 1251 => (exclusive = "sppr251.spl", path=["Spells", "New spells"],), # alicorn lance
		 1323 => (exclusive = ["sppr350.spl"], path=["Spells", "New spells"],), # clarity/exaltation
		 1351 => (exclusive = ["moonbla.itm"],), # moonblade
		 1351 => (path = ["Items", "Summoned weapons"],),
		 1404 => (exclusive = "sppr404.spl", path=["Spells", "Healing"],), # neutralize poison
		 1505 => (path=["Spells", "Divination"],),
		 1603 => (exclusive = "sppr603.spl", path=["Spells", "Evocation"],), # blade barrier
		 1609 => (path=["Spells", "Evocation"],),
		 1611 => (exclusive = "sppr611.spl", path=["Spells", "Alteration"],), # wondrous recall
		 1613 => (path=["Spells", "Abjuration"],),
		 1614 => (exclusive = "sorb.itm", path=["Items", "Summoned weapons"],), # sol's searing orb
		 1707 => (path=["Spells", "Evocation"],),
		 1710 => (exclusive = "sppr710.spl", path=["Spells", "Conjuration"],), # holy word
		2105 => (exclusive = "spwi105.spl", path=["Spells", "Alteration"],), # Color Spray (not exclusive)
		 2108 => (exclusive = "spwi108.spl", path=["Spells", "Abjuration"],),
# 		 2112 => (exclusive = "spwi112.spl", path=["Spells", "Evocation"]), # magic missile
		2116 => (path = ["Spells", "Enchantment"],),
		 2201 => (exclusive = ["7eyes.2da"], path = ["Spells", "Illusion"],), #blur
		 2209 => (exclusive = "spwi209.spl", path=["Spells", "Enchantment"],),
		 2212 => (exclusive = "spwi212.spl",), # mirror image
		2212 => (path = ["Spells", "Illusion"],),
		 2213 => (exclusive = "spwi213.spl",), # stinking cloud
		2213 => (path = ["Spells", "Conjuration"],),
		 2215 => (exclusive = "spwi215.spl",), # web
		 2215 => (path = ["Spells", "Conjuration"],),
		 2217 => (exclusive = "spwi217.spl",), # aganazzar's missiles
		 2217 => (path = ["Spells", "Evocation"],),
		 2224 => (exclusive = "spwi224.spl",), # glitterdust
		 2224 => (path = ["Spells", "Conjuration"],),
		 2251 => (exclusive = "cdideca.itm",), # decastave
		 2251 => (path = ["Items", "Summoned weapons"],),
		 2305 => (exclusive = ["msectype.2da", "spwi312.spl", "potn14.spl", "spwi305.spl"],), # haste/slow
		 2305 => (path = ["Spells", "Alteration"],),
		 2321 => (exclusive="spwi321.spl", path=["Spells", "Abjuration"],),
		 2324 => (exclusive = ["spwi324.spl", "spwi234d.spl", "spwi720.spl"],), # hold/control undead
		 2324 => (path = ["Spells", "Necromancy"],),
		 2413 => (exclusive = ["spwi413a.spl","spwi413d.spl"],), # otiluke
		 2413 => (path = ["Spells", "Alteration"],),
		 2418 => (exclusive = ["spwi418.spl", "spwi403.spl"],), # fire shields
		 2418 => (path = ["Spells", "Abjuration"],),
		 2451 => (exclusive = ["shades.2da"],), # shades/shadow monsters
		 2518 => (exclusive = "spwi518.spl",), # phantom blade
		 2518 => (path = ["Items", "Summoned weapons"],),
		 2523 => (exclusive = "spwi523.spl", path=["Spells", "Evocation"],), # sunfire
		 2708 => (exclusive = "spwi708.spl", path=["Spells", "Abjuration"],), # mantle
		 2711 => (path=["Spells", "Enchantment"],),
		 2714 => (path=["Spells", "Evocation"],),
		 2808 => (exclusive = "sppr603.spl",), # moment of prescience
		 2811 => (exclusive = ["spwi811.spl", "scrl9f.itm"], path=["Spells", "Conjuration"],), # symbol:fear
 2914 => (exclusive = ["spwi914.spl", "spwi806.spl"], path=["Spells", "Necromancy"],),
 2915 => (exclusive = ["spwi806.spl"], path=["Spells", "Conjuration"],),
		 2916 => (exclusive = "spwi916.spl", path=["Spells", "Alteration"],), # shapechange
		 2923 => (exclusive = "plangood.cre", path=["Creatures"],), # planetar
		 3000 => (path = ["Items", "Summoned weapons"],),
		 3010 => (exclusive = "ring36.itm", path=["Items"],), # ring of danger sense
 3020 => (exclusive = ["7eyes.2da", "brac18.itm"], path=["Items"],),
		 3030 => (exclusive = ["pfirea.bam", "pfirex.bam"], path=["Items"],), # exploding weapons
 3040 => (exclusive = ["leat14.itm", "eff_e02.wav"], path=["Items"],),
		 3060 => (exclusive = "boot12.spl", path=["Items"],), # cloak of the gargoyle
		 3070 => (exclusive = ["hamm06.itm", "hamm06a.itm", "hamm06b.itm"], path=["Items"],),
		 3080 => (exclusive = ["misc3n.itm", "misc3o.itm"], path=["Items"],), # instruments
		 3091 => (exclusive = "misc89.itm", path=["NPC", "Edwin"],), # edwin's amulet
		 3092 => (exclusive = "misc89.itm", path=["NPC", "Edwin"],), # edwin's amulet
		 3093 => (exclusive = "misc89.itm", path=["NPC", "Edwin"],), # edwin's amulet
		 3100 => (exclusive = "clck15.itm", path=["Items"],), # cloak of the weave
		5010 => (path = ["Spells", "Healing"],),
		4010 => (path = ["Spells", "Necromancy"],),
		4031 => (path = ["Creatures", "Dragons"],),
		4032 => (path = ["Creatures", "Dragons"],),
		4033 => (path = ["Creatures", "Dragons"],),
	)
	setmod!("deitiesoffaerun",
		"" => (conflicts=["faiths_and_powers", "spell_rev",],
			after=["eet"], before=["eet_end"],),
	)
	setmod!("divine_remix",
		10 => (path = ["Spells", "New spells"],),
		11 => (path = ["Spells", "New spells"],),
		50 => (path = ["Classes", "Cleric"],),
		51 => (path = ["Classes", "Cleric"],),
		52 => (path = ["Classes", "Druid"],),
		53 => (path = ["Classes", "Druid"],),
		54 => (path = ["Classes", "Paladin"],),
		55 => (path = ["Classes", "Paladin"],),
		56 => (path = ["Classes", "Ranger"],),
		57 => (path = ["Classes", "Ranger"],),
		100 => (exclusive = "mxsplprs.2da", path = ["Tables", "Spell slots"],),
		103 => (path = ["Classes", "Cleric"],),
		106 => (path = ["Classes", "Cleric"],),
		107 => (path = ["Classes", "Cleric"],),
		109 => (path = ["Classes", "Cleric"],),
		112 => (path = ["Classes", "Cleric"],),
		115 => (path = ["Classes", "Cleric"],),
		118 => (path = ["Classes", "Cleric"],),
		121 => (path = ["Classes", "Cleric"],),
		124 => (path = ["Classes", "Cleric"],),
		127 => (path = ["Classes", "Cleric"],),
		130 => (path = ["Classes", "Cleric"],),
		200 => (path = ["Tables", "Spell slots"],),
		203 => (path = ["Classes", "Druid"],),
		403 => (path = ["Classes", "Ranger"],),
		406 => (path = ["Classes", "Ranger"],),
		409 => (path = ["Classes", "Ranger"],),
		412 => (path = ["Classes", "Ranger"],),
		415 => (path = ["Classes", "Ranger"],),
		600 => (path = ["NPC", "Branwen"],),
		605 => (path = ["NPC", "Jaheira"],),
		610 => (path = ["NPC", "Viconia"],),
		1000 => (path = ["Spells", "Sphere system"],),
	)
	setmod!("dw_lanthorn",
		10 => (path = ["Story", "BG2"],),
		20 => (path = ["Story", "BG2"],),
	)
	setmod!("dr8_hotfix", "" => (after=["divine_remix"],),
		0 => (path=["Classes", "Cleric"],)
	)
	setmod!("eet", "" => (after = [
	# https://k4thos.github.io/EET-Compatibility-List/EET-Compatibility-List.html
	# better: EET/tbl/compatibility.tbl
	"dlcmerger", "bgeetextpack", "sodrus", "bg1aerie", "bg1npcmusic", "bg1ub", "darkhorizonsbgee", "drake", "drizztsaga", "garrick-tt", "k9roughworld", "saradas_magic", "k9sharteelnpc", "sirene", "tenyathermidor", "soa", "karatur", "verrsza", "white", "bgsodde",
# "margarita",
	], before = ["bg2ee_ga", "bg2eer"],),
	 0 => (path = ["EET",],))
	setmod!("dlcmerger",
# 		"" => (before=collect(filter(≠("dlcmerger"), moddb["eet"].compat[1].after)),),
		1 => (path = ["EET"],),
		2 => (path = ["EET"],),
		3 => (path = ["EET"],),
	)
	setmod!("bg2eetrans", 0 => (path = ["EET",],),)
	setmod!("bgsodde", 0 => (path = ["EET",],),)
	setmod!("eet_end",
		0 => (path = ["EET",],),
		1 => (path = ["EET",],),
	)
	setmod!("eet_fix", 0=>(path=["EET"],),)
	setmod!("eet_swo", 0=>(path=["Story","BG1", "Ending"],),)
	setmod!("eet_tweaks",
		1000 => (path = ["NPC", "Edwin", "Appearance"],),
		1001 => (path = ["NPC", "Edwin", "Appearance"],),
		1010 => (path = ["NPC", "Imoen", "Appearance"],),
		1011 => (path = ["NPC", "Imoen", "Appearance"],),
		1012 => (path = ["NPC", "Imoen", "Appearance"],),
		1020 => (path = ["NPC", "Jaheira", "Appearance"],),
		1021 => (path = ["NPC", "Jaheira", "Appearance"],),
		1030 => (path = ["NPC", "Minsc", "Appearance"],),
		1031 => (path = ["NPC", "Minsc", "Appearance"],),
		1040 => (path = ["NPC", "Viconia", "Appearance"],),
		1041 => (path = ["NPC", "Viconia", "Appearance"],),
		1042 => (path = ["NPC", "Viconia", "Appearance"],),
		1050 => (path = ["NPC"],),
		1060 => (path = ["Cosmetic", "Sound"],),
		2000 => (exclusive = "xplevel.2da", path=["Tables", "XP"],),
		2001 => (exclusive = "xplevel.2da", path=["Tables", "XP"],),
		2002 => (exclusive = "xplevel.2da", path=["Tables", "XP"],),
		2003 => (exclusive = "xplevel.2da", path=["Tables", "XP"],),
		2010 => (path=["Tables", "XP", "Cap"],),
		2011 => (path=["Tables", "XP", "Cap"],),
		2012 => (path=["Tables", "XP", "Cap"],),
		2013 => (path=["Tables", "XP", "Cap"],),
		2020 => (path=["Tables", "XP", "Cap"],),
		2021 => (path=["Tables", "XP", "Cap"],),
		2022 => (path=["Tables", "XP", "Cap"],),
		2023 => (path=["Tables", "XP", "Cap"],),
		2030 => (path=["Tables", "XP", "Cap"],),
		2031 => (path=["Tables", "XP", "Cap"],),
		2032 => (path=["Tables", "XP", "Cap"],),
		2033 => (path=["Tables", "XP", "Cap"],),
		2040 => (exclusive = "xpbonus.2da", path=["Tables", "Bonus XP"]),
		2041 => (exclusive = "xpbonus.2da", path=["Tables", "Bonus XP"]),
		2042 => (exclusive = "xpbonus.2da", path=["Tables", "Bonus XP"]),
		2043 => (exclusive = "xpbonus.2da", path=["Tables", "Bonus XP"]),
		2044 => (exclusive = "xpbonus.2da", path=["Tables", "Bonus XP"]),
		2050 => (path = ["Tables", "XP", "Quests"],),
		2051 => (path = ["Tables", "XP", "Quests"],),
		2052 => (path = ["Tables", "XP", "Quests"],),
		2053 => (path = ["Tables", "XP", "Quests"],),
		2054 => (path = ["Tables", "XP", "Quests"],),
		2055 => (path = ["Tables", "XP", "Quests"],),
		2056 => (path = ["Tables", "XP", "Quests"],),
		2060 => (path = ["Tables", "XP", "Quests"],),
		2061 => (path = ["Tables", "XP", "Quests"],),
		2062 => (path = ["Tables", "XP", "Quests"],),
		2063 => (path = ["Tables", "XP", "Quests"],),
		2064 => (path = ["Tables", "XP", "Quests"],),
		2065 => (path = ["Tables", "XP", "Quests"],),
		2066 => (path = ["Tables", "XP", "Quests"],),
		2070 => (path = ["Tables", "XP", "Quests"],),
		2080 => (path = ["Tables", "XP", "Quests"],),
		3010 => (path = ["Items", "Scrolls"],),
		3020 => (path = ["Skills", "Familiar"],),
		3021 => (path = ["Skills", "Familiar"],),
		3022 => (path = ["Skills", "Familiar"],),
		4050 => (path = ["Items",],),
		4040 => (path = ["Story", "BG2"],),
		4060 => (path = ["Items"],),
		4070 => (path = ["Items"],),
	)
	setmod!("eetact2",
		100 => (exclusive = ["ar0601.are", "ily1.cre", "ilyhamm.itm", ],),
		120 => (exclusive = ["torgal.cre"],),
		130 => (exclusive = ["sahamb01.bcs", "sahcpt01.cre", "sahgrd01.cre"],),
		140 => (exclusive = ["bodhi2.cre", "bodtan.cre",],),
		150 => (exclusive = ["ar2900.bcs", "helljon.dlg"],),
		160 => (exclusive = ["ar0907.bcs", "hlkoshi.cre", "hlolaf.cre", "hlmafer.cre", "hlstal.cre", "hlketta.cre", "hlsion.cre", "hlketta.bcs",],),
		170 => (exclusive = ["ar1008.bcs", "hlshang.cre", "hlrevan.cre", "hllayen.cre"],),
		200 => (exclusive = ["ar9391.bcs", "tsmaepet.cre", "mvguard1.cre", "mvpries.cre", "maevar.cre"],),
		210 => (exclusive = ["ar0318.are", "ar0318.bcs"],),
		230 => (exclusive = ["hldemi.cre", "ar0331.bcs", "ar0330.bcs", "archlich.cre"],),
		231 => (exclusive = ["hldemi.cre", "ar0331.bcs", "ar0330.bcs", "archlich.cre"],),
		232 => (exclusive = ["hldemi.cre", "ar0331.bcs", "ar0330.bcs"],),
		240 => (exclusive = ["ar0326.bcs"],),
		260 => (exclusive = ["spwnbeh.bcs", "spwndead.bcs", "spwndrow.bcs", "spwngol.bcs", "spwnmind.bcs", "spwnmon.bcs", "spwnorc.bcs", "spwnrak.bcs", "spwntoa.bcs", "spwntrol.bcs", "spwnvamp.bcs", "spwnwolf.bcs"],),
		270 => (exclusive = ["amncen1.cre", "ar0041.bcs", "ar0045.bcs", "ar0046.bcs",],),
		280 => (exclusive = ["ar0042.bcs", "ar0043.bcs", "ar0044.bcs",],),
		290 => (exclusive = "undead",),
		300 => (exclusive = ["flayer01.bcs", "gormind.bcs", "mindal01.bcs"],),
		310 => (exclusive = ["udsilver.itm", "spin691.spl", "spin693.spl", "dragred.bcs", "dragblac.bcs", "dhadra01.bcs"],),
		320 => (exclusive = ["behold.itm", "behdir01.bcs", "behhiv01.bcs", "behold01.bcs", "gauth01.bcs", "elderorb.bcs"],),
		330 => (exclusive = ["vampir01.bcs", "vamemi01.bcs"],),
		340 => (exclusive = ["lich01.cre", "mage10a.bcs", "bodtan.cre", "lavok01.cre",],),
		350 => (exclusive = ["goliro.itm", "golsto.itm", "goliro01.cre", "golsto01.cre", "golada01.cre",],),
		360 => (exclusive = ["nymph.bcs",],),
		370 => (exclusive = ["ar0406.bcs",],), # improved coronet
		380 => (exclusive = ["Oasis", "amtarc01.cre", "amtmag01.cre", "amtcle01.cre", "amtcap01.cre", "amtpik01.cre"], path=["Areas", "BG2", "Oasis"],),
		390 => (exclusive = ["ar1700.are",],), # small teeth
		400 => (exclusive = ["ar1800.are", "ar18arch.cre", "ar18dwaf.cre", "ar18fig.cre", "ar18mage.cre", "ar18prie.cre", "ar18skel.cre", "ar18thif.cre"],), # north forest
		410 => (exclusive = ["ar5200.are",],), # marching mountains
		420 => (exclusive = ["dempit01.cre", "telpit1.cre", "tanari.bcs", "demglab.bcs", "dempit.bcs", "mage20c.bcs",],), # improved demons
		430 => (exclusive = ["ar1900.bcs", "impshad.bcs"],),
		431 => (exclusive = ["ar1900.bcs", "impshad.bcs"],),
		432 => (exclusive = ["ar1900.bcs", "impshad.bcs"],),
		440 => (exclusive = ["giafir.itm", "ysg2.cre", "ysfire01.cre", "ysguar01.cre"],),
	)
	setmod!("epicthieving",
		0 => (path = ["Skills", "Thieving"],),
		100 => (path = ["Skills", "Thieving"],),
		200 => (path = ["Skills", "Thieving"],),
		300 => (path = ["Skills", "Thieving"],),
		400 => (path = ["Skills", "Thieving"],),
		500 => (path = ["Items", "Potions"],),
		600 => (path = ["Skills", "Thieving"],),
	)
	setmod!("faiths_and_powers",
		"" => (after = ["divine_remix", "item_rev", "iwdification", "monasticorders", "spell_rev", "tomeandblood" ],
			before = ["fnp_multiclass", "might_and_guile", "scales_of_balance", "stratagems", "cdtweaks"],),
			21 => (path = ["Spells", "Sphere system"],),
			22 => (path = ["Spells", "Sphere system"],),
			23 => (path = ["Spells", "Sphere system"],),
			24 => (path = ["Spells", "Sphere system"],),
			25 => (path = ["Spells", "Sphere system"],),
			30 => (path = ["Spells"],),
			31 => (path = ["Classes", "Cleric"],),
			33 => (path = ["Classes", "Druid"],),
			35 => (path = ["Classes", "Paladin"],),
			37 => (path = ["Classes", "Ranger"],),
			75 => (path = ["Classes", "Cleric"],),
			80 => (path = ["Spells", "Sphere system"],),
			85 => (path = ["NPC"],),
	)
	setmod!("faldornbg2",
		0 => (path=["NPC", "Faldorn"],),
		1 => (path=["NPC", "Faldorn"],),
	)
	setmod!("fnp_multiclass",
		"" => (after = ["fnp", "divine_remix", "deitiesoffaerun", "item_rev", "iwdification", "monasticorders", "spell_rev", "tomeandblood" ],
			before = ["scales_of_balance", "stratagems", "cdtweaks"],),
		91 => (path = ["Classes", "Druid"],),
		92 => (path = ["Classes", "Shaman"],),
		95 => (path = ["Classes", "Cleric"],),
		99 => (path = ["NPC"],)
	)
	setmod!("fullplate",
		1 => (path = ["Fighting", "Armor"],),
		102 => (path = ["Items", "Protection"],),
		204 => (path = ["Skills", "Backstab"],),
	)
	setmod!("haerdalis_friendship", 0 => (path = ["NPC", "Haer'dalis"],))
	setmod!("haerdalisromance", 0 => (path = ["NPC", "Haer'dalis"],))
	setmod!("hammers",
		0 => (path = ["Cosmetic", "Sprites"],),
		15 => (path = ["Cosmetic", "Sprites"],),
		25 => (path = ["Items"],),
		35 => (path = ["Items"],),
		50 => (path = ["Items"],),
	)
	setmod!("imoenfriendship", 0 => (path = ["NPC", "Imoen"],))
	setmod!("iepbanters",
		0	=> (path=["NPC"],),
		1 => (path=["NPC"],),
		2 => (path=["NPC", "Imoen"],),
		3 => (path=["NPC", "Anomen"],),
		4 => (path=["NPC"],),
		5 => (path=["NPC"],),
		6 => (path=["NPC", "Banter"],),
		7 => (path=["NPC", "Banter"],),
		8 => (path=["NPC", "Banter"],),
		9 => (path=["NPC", "Banter"],),
		10 => (path=["NPC", "Banter"],),
		11 => (path=["NPC", "Banter"],),
	)
	setmod!("ihateundead",
		0 => (path=["Classes", "Ranger"],),
		1 => (path=["Classes", "Cleric"],),
		2 => (path=["Classes", "Wizard"],),
		3 => (path=["Classes", "Paladin"],),
		4 => (path=["Classes", "Thief"],),
		5 => (path=["Classes", "Fighter"],),
		6 => (path=["Classes", "Bard"],),
		7 => (path=["Classes", "Druid"],),
		8 => (path=["NPC", "Jaheira"],),
		9 => (path=["Classes", "Monk"],),
		10 => (path=["Classes", "Shaman"],),
		11 => (path=["Classes", "Sorcerer"],),
	)
	setmod!("impasylum",
		0 => (path = ["Story", "BG2", "Brynnlaw"],),
		1 => (path = ["Story", "BG2", "Brynnlaw"],),
	)
	setmod!("item_rev",
		"" => (after = ["eet"],),
	0 => (exclusive = ["hlolth.itm", "clolth.itm", "amul01.itm", "amul01.spl", "arow01.itm", "ax1h01.itm", "blun01.itm", "bolt01.itm", "sahbolt.itm", "kuobolt.itm", "boot01.itm", "bow01.itm", "brac01.itm", "bull01.itm", "chan01.itm", "clck01.itm", "dagg01.itm", "dart01.itm", "dwblun01.itm", "dwbolt01.itm", "dwchan01.itm", "dwclck01.itm", "dwhalb01.itm", "dwplat01.itm", "dwshld01.itm", "dwsper01.itm", "dwsw1h01.itm", "dwxbow01.itm", "halb01.itm", "hamm01.itm", "helm01.itm", "amsoul01.itm", "leat01.itm", "aegis.itm", "bruenaxe.itm", "bruenpla.itm", "cattibow.itm", "catliowp.cre", "figlion.itm", "spidfgsu.cre", "figspid.itm", "bsw1h01.itm", "bersersu.cre", "bleat01.itm", "miscbc.itm", "nebdag.itm", "quiver01.itm", "reaver.itm", "korax01.itm", "nparm.itm", "npbow.itm", "npbelt.itm", "npchan.itm", "npclck.itm", "npmisc1.itm", "npstaf.itm", "npplat.itm", "keldorn.spl", "npring01.itm", "npshld.itm", "npsw01.itm", "clolth.itm", "hlolth.itm", "finsarev.itm", "plat01.itm", "rods01.itm", "rods01.spl", "shld01.itm", "slng01.itm", "sper01.itm", "staf01.itm", "smoundsu.cre", "smoundsu.itm", "sw1h01.itm", "xbow01.itm", "waflail.itm", "wawak.itm"], path=["Items"],),
	1 => (path = ["Items", "Magic weapons"],),
	2 => (path = ["Skills", "Casting"],),
	3 => (path = ["Skills", "Casting"],),
	4 => (path = ["Skills", "Casting"],),
	5 => (path = ["Skills", "Casting"],),
	1030 => (path = ["Items", "Stores"],),
	1041 => (path = ["Fighting", "Armor"],),
	1042 => (path = ["Fighting", "Armor"],),
	6 => (path = ["Fighting", "Armor"],),
	7 => (path = ["Fighting", "Armor"],),
	8 => (path = ["Fighting", "Armor"],),
	9 => (path = ["Skills", "Thieving"],),
	1101 => (path = ["Skills", "Thieving"],),
	1050 => (path = ["Fighting", "Armor"],),
	10 => (path = ["Fighting", "Armor"],),
	11 => (path = ["Fighting", "Fighting styles"],),
	12 => (path = ["Items", "Protection"],),
	13 => (path = ["Items", "Weapons"],),
	15 => (path = ["Classes", "Druid"],),
	16 => (path = ["Classes", "Druid"],),
	17 => (path = ["Items", "Weapons"],),
	1020 => (path = ["Items", "Potions"],),
	1060 => (path = ["Classes", "Fighter"],),
	1070 => (path = ["Classes", "Thief"],),
	18 => (path = ["Skills", "Backstab"],),
	19 => (path = ["Skills", "Backstab"],),
	20 => (path = ["Skills", "Backstab"],),
 1030 => (exclusive = ["chrmodst.2da", "repmodst.2da"],),
 1040 => (path = ["Fighting", "Armor"],),
 1043 => (path = ["Fighting", "Armor"],),
 1090 => (path = ["Classes", "Cleric"],),
 1091 => (path = ["Classes", "Cleric"],),
 1092 => (path = ["Classes", "Cleric"],),
 1093 => (path = ["Classes", "Cleric"],),
	)
	setmod!("itemupgrade",
		0 => (path=["Items", "Upgrades"],),
		10 => (path=["Items", "Upgrades"],),
		1 => (path=["Items", "Upgrades"],),
		11 => (path=["Items", "Upgrades"],),
		12 => (path=["Cosmetic", "Sound"],),
	)
	setmod!("iwdification",
		10 => (path=["Cosmetic", "Spells"],),
		20 => (path=["Cosmetic", "Sprites"],),
		60 => (path=["Items", "Weapons"],),
		90 => (path=["Spells", "Alteration"],),
		130 => (path=["Cosmetic", "Sprites"],),
		50 => (path=["Classes", "Bard"],),
		180 => (path=["Classes", "Bard"],),
		150 => (path=["Classes", "Bard"],),
		70 => (path=["Classes", "Druid"],),
		71 => (path=["Classes", "Druid"],),
		100 => (path=["Classes", "Paladin"],),
		160 => (path=["Tables", "Spell slots"],),
		170 => (path=["Tables", "Spell slots"],),
		120 => (path=["Skills", "Evasion"],),
		30 => (path=["Spells", "New spells"],),
		40 => (path=["Spells", "New spells"],),
		80 => (path=["Cosmetic", "Icons"],),
	)
	setmod!("janquest", 0 => (path = ["NPC", "Jan"],))
	setmod!("k9sharteelnpc", 0 => (path = ["NPC", "Shar-Teel"],))
	setmod!("keto", "" => (after = ["kelsey"],),)
	setmod!("khalidbg2",
		0 => (path=["NPC", "Khalid"],),
		1 => (path=["NPC", "Khalid"],),
	)
	setmod!("kivan",
		100 => (path = ["NPC", "Kivan"],),
		200 => (path = ["NPC", "Kivan"],),
		201 => (path = ["NPC", "Kivan"],),
		202 => (path = ["NPC", "Kivan"],),
		300 => (path = ["NPC", "Kivan"],),
	)
	setmod!("klatu",
		1000 => (path=["Items"],),
		1010 => (path=["Items"],),
		1020 => (path=["Spells", "Conjuration"],),
		1040 => (path=["NPC", "Hexxat"],),
		1050 => (path=["Creatures"],),
		2000 => (path=["Tables", "Spell slots"],),
		2010 => (path=["Tables", "Spell slots"],),
		2020 => (path=["Items", "Stores"],),
		2030 => (path=["Items", "Stores"],),
		2040 => (path=["Classes", "Mage"],),
		2050 => (path=["Spells", "Conjuration"],),
		2060 => (path=["Spells", "Alteration"],), # for haste
		2110 => (path=["Skills"],),
		2130 => (path=["Skills", "Bard song"],),
		2140 => (path=["Skills", "Casting"],),
		2150 => (path=["Skills", "Thieving"],),
		2160 => (path=["Spells", "Alteration"],),
		2170 => (path=["Items", "Stores"],),
		2180 => (path=["Items", "Stores"],),
		2200 => (path=["Skills", "Familiar"],),
		3070 => (path=["Cosmetic", "Icons"],),
	)
	setmod!("korganfriendship", 0 => (path = ["NPC", "Korgan"],))
	setmod!("lenshunt", 0 => (path = ["Story", "BG2"],))
	setmod!("leui",
		"" => (before = ["eeuitweaks", "might_and_guile", "stratagems",
		"tomeandblood", "shadowadept", "deities-of-faerun", "faiths_and_powers"],),
	)
	setmod!("mazzy", 0 => (path = ["NPC", "Mazzy"],))
	setmod!("mercenary",
		0 => (path=["Classes", "Fighter"],),
		1 => (path=["NPC", "Kagain"],),
		2 => (path=["NPC", "Korgan"],),
	)
	setmod!("metweaks",
		200 => (path=["Tables", "XP", "Quest"],),
		400 => (path=["Fighting", "Proficiencies"],),
		500 => (path=["Fighting", "Proficiencies"],),
		505 => (path=["Fighting", "Proficiencies"],),
		510 => (path=["Fighting", "Proficiencies"],),
		600 => (path=["Skills", "Dancing"],),
		1000 => (exclusive = "7eyes.2da", path=["Skills", "Deflect missile"],),
		1200 => (exclusive = "spcl922.spl", path=["Skills", "Tracking"],),
		1400 => (exclusive = "spcl311.spl", path=["Skills", "Charm animal"],),
		1500 => (exclusive = "spcl212.spl", path=["Skills", "Detect evil"],),
		1600 => (exclusive = "spcl131.spl", path=["Skills", "Magic resistance"],),
		1800 => (exclusive = "backstab.2da", path=["Skills", "Backstab"],),
		2000 => (exclusive = [ "scrl1$i.itm" for i in 1:7 ], path=["Items"],), # cursed scrolls
		2200 => (path = ["Creatures"],),
		2600 => (path = ["UI"],),
		2800 => (path = ["Story", "BG1"],),
		3000 => (path = ["Story", "BG1"],),
		3100 => (path = ["Story", "BG1"],),
		3150 => (path=["Creatures", "Fiends"], exclusive = "bdbelhif.cre",), # Belhifet
		3200 => (path=["Skills", "Infravision"],),
		3205 => (path=["Skills", "Infravision"],),
		3210 => (path=["Skills", "Infravision"],),
		3600 => (path=["Cosmetic", "Sprites"],),
		3800 => (path=["Skills", "Backstab"],),
		3805 => (path=["Skills", "Backstab"],),
		3810 => (path=["Skills", "Backstab"],),
		3815 => (path=["Skills", "Backstab"],),
		4000 => (exclusive = "weapprof.2da/styles", path=["Fighting", "Fighting styles"],),
	)
	setmod!("might_and_guile",
		"" => (before = ["refinements", "stratagems"],),
		200 => (path = ["Tables", "HLA"],),
		205 => (path = ["Classes", "Ranger"],),
		210 => (path = ["Classes", "Bard"],),
		220 => (path = ["Classes", "Restrictions"],),
		230 => (path = ["Classes", "Ranger"],),
		235 => (path = ["Classes", "Ranger"],),
		240 => (path = ["Classes", "Ranger"],),
		245 => (path = ["Classes", "Barbarian"],),
		250 => (path = ["Skills", "Rage"],),
		265 => (path = ["Classes", "Monk"],),
		275 => (path = ["Classes", "Thief"],),
		310 => (path = ["Classes", "Fighter"],),
		320 => (path = ["Classes", "Fighter"],),
		322 => (path = ["Classes", "Ranger"],),
		324 => (path = ["Classes", "Ranger"],),
		350 => (path = ["Classes", "Ranger"],),
		360 => (path = ["Classes", "Ranger"],),
		410 => (path = ["Classes", "Thief"],),
		420 => (path = ["Classes", "Thief"],),
		450 => (path = ["Classes", "Bard"],),
		460 => (path = ["Classes", "Bard"],),
		470 => (path = ["Classes", "Bard"],),
		480 => (path = ["Classes", "Bard"],),
		490 => (path = ["Classes", "Bard"],),
		499 => (path = ["Classes", "Bard"],),
	),
	setmod!("mih_eq",
		"" => (before = ["stratagems"],),
		0 => (path=["Creatures", "Undead"],),
		1 => (path=["Creatures", "Dragons"],),
		2 => (path=["Creatures", "Golems"],),
		3 => (path=["Creatures", "Undead"],),
		4 => (path=["Creatures"],),
		5 => (path=["Creatures"],),
		6 => (path=["Creatures"],),
		7 => (path=["Creatures"],),
		8 => (path=["Creatures", "Undead"],),
	)
	setmod!("mih_ip",
		0 => (path=["Items", "New"],),
		1 => (path=["Items", "New"],),
		2 => (path=["Items", "New"],),
		3 => (path=["Items", "Wands"],),
		4 => (path=["Items", "Potions"],),
		5 => (path=["Items", "Potions"],),
		6 => (path=["Items", "Potions"],),
		7 => (path=["Items"],),
		8 => (path=["Items"],),
		9 => (path=["Items"],),
		10 => (path=["Items"],),
		11 => (path=["Items"],),
		12 => (path=["Items"],),
		13 => (path=["Items"],),
		14 => (path=["Items", "Potions"],),
		15 => (path=["Items"],),
		16 => (path=["Items", "Stores"],),
	)
	setmod!("mih_sp",
		0 => (path=["Spells"],),
		1 => (path=["Spells"],),
		2 => (path=["Spells"],),
		3 => (path=["Spells"],),
		4 => (path=["Classes", "Paladin"],),
		5 => (path=["Skills", "Bhaalspawn"],),
		6 => (path=["Spells", "Conjuration"],),
		7 => (path=["Spells", "Conjuration"],),
		8 => (path=["Spells", "Abjuration"],),
		9 => (path=["Spells", "Conjuration"],),
		10 => (path=["Spells", "Evocation"],),
		11 => (path=["Spells", "Alteration"],),
		12 => (path=["Spells", "Alteration"],),
		13 => (path=["Spells"],),
		14 => (path=["Tables", "Bonus XP"],),
		15 => (path=["Items", "Stores"],),
		17 => (path=["Items", "Scrolls"],),
	)
	setmod!("militiaofficer",
		0 => (path=["Classes", "Fighter"],),
		1 => (path=["NPC", "Khalid"],),
	)
	setmod!("minscfriendship", 0 => (path = ["NPC", "Minsc"],))
	setmod!("monasticorders",
		0 => (path=["Classes", "Monk"],),
		1 => (path=["Classes", "Monk"],),
		2 => (path=["Classes", "Monk"],),
		3 => (path=["Classes", "Monk"],),
		4 => (path=["Classes", "Monk"],),
	)
	setmod!("npc_tweak",
		0 => (path = ["NPC", "Anomen"],),
		1 => (path = ["NPC", "Cernd"],),
		2 => (path = ["NPC", "Nalia"],),
	)
	setmod!("npckit",
	)
	setmod!("raduzielsuniversalspells", 0 => (path=["Spells"],))
	setmod!("rr",
		"" => (after = ["item_rev", "eetact2", "song_and_silence", "divine_remix", "tod", "spell_rev", "ctb", "d0questpack", "beyond_the_law", ],
			before = ["stratagems", "atweaks", "eet_tweaks", "virtue"],
			conflicts = ["iispellsystemadjustments"],),
		0 => (exclusive = ["weapprof.2da"], path=["Fighting", "Proficiencies"]),
		1 => (exclusive = ["clabth02.2da", "clabth04.2da", "clabth03.2da"],
			path=["Classes", "Thief"],), # assassin ability table
		2 => (exclusive = ["luth0.2da", "luth1.2da", "lumt0.2da", "luct0.2da", "luft0.2da"], path=["Tables", "HLA"],),
		3 => (exclusive = ["skillrac.2da"], path=["Skills", "Thieving"],),
		4 => (exclusive = ["clabba01.2da", "clabba04.2da", "clabba03.2da"],
			path=["Classes", "Bard"],),
		5 => (exclusive = ["luba0.2da", "luba2.2da", "luba3.2da"],
			path=["Tables", "HLA"],),
		6 => (exclusive = ["mxsplbrd.2da"], path=["Tables", "Spell slots"],),
		7 => (path=["Items"],),
		8 => (path=["Items", "Upgrades"],),
		9 => (exclusive = ["potn36.itm", "potn39.itm"], path=["Items", "Potions"],),
		10 => (exclusive = ["potn36.itm", "potn39.itm"], path=["Items", "Potions"]),
		11 => (path=["Story", "BG2"],),
		12 => (exclusive = ["c6arkan.cre", "c6arkan3.cre", "c6kach.cre", "c6yean.cre", "c6arkan.bcs", "stguard1.cre", "mook02.cre", "arkanisg.cre", "mookft01.cre", "palern.cre", "ar0300.bcs", "stguard1.bcs", "aran.cre", "gaelan.cre", "mook.cre", "booter.cre"], path=["Story", "BG2"],),
		999 => (path=["Cosmetic", "Icons"],),
	)
	setmod!("refinements",
		10 => (path=["Tables", "HLA"],),
		11 => (path=["Tables", "HLA"],),
		101 => (path=["Tables", "HLA"],),
		102 => (path=["Tables", "HLA"],),
		103 => (path=["Tables", "HLA"],),
		104 => (path=["Tables", "HLA"],),
		105 => (path=["Tables", "HLA"],),
		106 => (path=["Tables", "HLA"],),
		107 => (path=["Tables", "HLA"],),
		108 => (path=["Tables", "HLA"],),
		109 => (path=["Tables", "HLA"],),
		110 => (path=["Tables", "HLA"],),
		20 => (path=["NPC", "Imoen"],),
		21 => (path=["NPC", "Imoen"],),
		22 => (path=["NPC", "Imoen"],),
		30 => (path=["Skills", "Shapeshifting"],),
		31 => (path=["Skills", "Shapeshifting"],),
		40 => (path=["Items"],),
		50 => (path=["Classes", "Fighter"],),
		70 => (path=["Fighting", "Armor"],),
		71 => (path=["Fighting", "Armor"],),
		72 => (path=["Fighting", "Armor"],),
		73 => (path=["Fighting", "Armor"],),
		74 => (path=["Fighting", "Armor"],),
		75 => (path=["Fighting", "Armor"],),
	)
	setmod!("ruad",
		0 => (path=["Items", "Upgrades"],),
		10 => (path=["Items", "Upgrades"],),
	)
	setmod!("sarevokromance", 0 => (path = ["NPC", "Sarevok"],))
	setmod!("scales_of_balance",
		"" => (conflicts = ["kit_rev", "kitpack",],
			after = ["cdtweaks", "item_rev", "kitpack", "tomeandblood",
				"might_and_guile", "atweaks"],),
		100 => (exclusive = ["dexmod.2da", "skilldex.2da"],
			path = ["Fighting", "Armor"],),
		101 => (exclusive = "masterwork_weapons", path=["Items", "Weapons"],),
		102 => (path = ["Fighting", "Tweaks"],),
		109 => (path = ["Items", "Potions"],),
		121 => (path = ["Items", "Weapons"],),
		122 => (conflicts = ["rr"],
			exclusive=["universal_clubs", "weapprof.2da", "thac0.2da"],
			path = ["Fighting", "Proficiencies"]),
		124 => (exclusive = ["weapprof.2da/styles"],
			path = ["Fighting", "Fighting styles"],),
		125 => (path = ["Fighting", "Proficiencies"],),
		160 => (exclusive = ["savemonk.2da", "saveprs.2da", "saverog.2da",
			"savewar.2da", "savewiz.2da"],
			path = ["Tables", "Saving throws"],),
		171 => (exclusive = ["clabth01.2da"], path = ["Skills", "Evasion"],),
		172 => (exclusive = ["clabth01.2da"], path = ["Skills", "Evasion"],),
		200 => (exclusive = ["strmod.2da", "strmodex.2da", "intmod.2da", "savecndh.2da", "savecng.2da"],
			path = ["Tables", "Abilities"],),
		201 => (exclusive = ["splprot.2da","mxsplwiz.2da"],
			path = ["Tables", "Spell slots"],),
		202 => (path = ["Items", "Weapons"],),
		206 => (exclusive = ["hpconbon.2da", "hpbarb.2da", "hpcm.2da", "hpct.2da", "hpfc.2da", "hpfm.2da", "hpfmt.2da", "hpmt.2da", "hpmonk.2da", "hpprs.2da", "hpprog.2da", "hpwar.2da", "hpwiz.2da"], path = ["Tables", "HP"],),
		207 => (exclusive = ["hpconbon.2da", "hpbarb.2da", "hpcm.2da", "hpct.2da", "hpfc.2da", "hpfm.2da", "hpfmt.2da", "hpmt.2da", "hpmonk.2da", "hpprs.2da", "hpprog.2da", "hpwar.2da", "hpwiz.2da"], path = ["Tables", "HP"],),
		208 => (exclusive = ["hpconbon.2da", "hpbarb.2da", "hpcm.2da", "hpct.2da", "hpfc.2da", "hpfm.2da", "hpfmt.2da", "hpmt.2da", "hpmonk.2da", "hpprs.2da", "hpprog.2da", "hpwar.2da", "hpwiz.2da"], path = ["Tables", "HP"],),
		210 => (exclusive = ["xplevel.2da", "lunumab.2da", "mxspldru.2da",],
			path = ["Tables", "XP"],),
		1012 => (path=["Items", "Magic weapons"],),
		2121 => (path=["Tables", "XP", "Quests"],),
		2122 => (path=["Tables", "XP", "Quests"],),
		2123 => (path=["Tables", "XP", "Quests"],),
	)
	setmod!("skiecost",
		0 => (path = ["NPC", "Skie"],),
		1 => (path = ["NPC", "Skie"],),
		2 => (path = ["NPC", "Skie"],),
		3 => (path = ["NPC", "Skie"],),
		4 => (path = ["NPC", "Skie"],),
		5 => (path = ["NPC", "Skie"],),
	)
	setmod!("skills-and-abilities",
		601 => (path = ["Classes", "Ranger"],),
		602 => (path = ["Classes", "Ranger"],),
		610 => (path = ["Classes", "Fighter"],),
		11 => (path = ["Classes", "Bard"],),
		12 => (path = ["Classes", "Bard"],),
		130 => (path = ["Classes", "Bard"],),
		20 => (path = ["Classes", "Monk"],),
		70 => (path = ["Classes", "Monk"],),
		40 => (path = ["Classes", "Ranger"],),
		50 => (path = ["Classes", "Fighter"],),
		80 => (path = ["Classes", "Paladin"],),
		60 => (path = ["Classes", "Paladin"],),
		100 => (path = ["Classes", "Paladin"],),
		120 => (path = ["Classes", "Cleric"],),
		91 => (path = ["Tables", "HLA"],),
		92 => (path = ["Tables", "HLA"],),
		300 => (path=["Fighting", "Proficiencies"],),
		311 => (path=["Fighting", "Proficiencies"],),
		312 => (path=["Fighting", "Proficiencies"],),
		313 => (path=["Fighting", "Proficiencies"],),
		400 => (path=["Fighting", "Proficiencies"],),
	)
	setmod!("sodbanter", 0 => (path = ["NPC", "Banter"],),)
	setmod!("sodrtd",
		0 => (path=["Story", "SoD"],),
		10 => (path=["Story", "SoD"],),
		20 => (path=["Story", "SoD"],),
		30 => (path=["Story", "SoD"],),
		40 => (path=["Story", "SoD"],),
		50 => (path=["Story", "SoD"],),
		60 => (path=["Story", "SoD"],),
		70 => (path=["Story", "SoD"],),
		80 => (path=["Story", "SoD"],),
	)
	setmod!("song_and_silence",
		0 => (path=["Classes"],),
		1 => (path=["Items", "Stores"],),
		2 => (path=["Classes", "Bard"],),
		3 => (path=["Classes", "Bard"],),
		4 => (path=["Classes", "Bard"],),
		5 => (path=["Classes", "Bard"],),
		6 => (path=["Classes", "Thief"],),
		7 => (path=["Classes", "Thief"],),
		8 => (path=["Classes", "Thief"],),
		9 => (path=["Classes", "Thief"],),
		10 => (path=["Classes", "Thief"],),
	)
	setmod!("spell_rev",
		"" => (after = ["ub",],),
		0 => (exclusive = ["elemtype.itm","mstone.itm", "shille.itm", "shille2.itm", "shille3.itm", "spcl213.spl", "spcl721.spl", "spcl722.spl", "spdr101.spl", "spdr201.spl", "spdr301.spl", "spdr401.spl", "spdr501.spl", "spdr601.spl", "spentaai.bam", "spentaci.bam", "spin101.spl", "spin102.spl", "spin103.spl", "spin104.spl", "spin105.spl", "spin106.spl", "spin113.spl", "spin683.spl", "spin701.spl", "spin788.spl", "spin789.spl", "spmagglo.bam", "spmagglo.vvc", "sppr101.spl", "sppr102.spl", "sppr103.spl", "sppr104.spl", "sppr105.spl", "sppr106.spl", "sppr107.spl", "sppr108.spl", "sppr109.spl", "sppr110.spl", "sppr111.spl", "sppr113.spl", "sppr116.spl", "spra301.spl", "spra302.spl", "spra303.spl", "spra304.spl", "spra305.spl", "spra306.spl", "spwi977.spl", "spwi978.spl", "undtype.itm", "vermtype.itm"], path=["Spells"],),
		10 => (exclusive = ["plangood.cre", "planevil.cre", "devagood.cre", "devaevil.cre"], path=["Cosmetic", "Sprites"],),
		20 => (path = ["Spells", "Illusion"],),
		30 => (path = ["Spells", "Abjuration"],),
		40 => (path = ["Spells", "Enchantment"],),
		50 => (path = ["Spells"],),
# 		65 => (exclusive = ["spcl900.spl","spcl901.spl","spcl907.spl","spwish12.spl"],),
	)
	setmod!("spstuff",
		0 => (path = ["Classes", "Ranger"],),
		1 => (path = ["Classes", "Fighter"],),
		2 => (path = ["Classes", "Fighter"],),
		3 => (path = ["Classes", "Thief"],),
		5 => (path = ["Classes", "Druid"],),
		6 => (path = ["Classes", "Bard"],),
		4 => (path = ["Items"],),
	)
	setmod!("stratagems",
		"" => (after = ["item_rev", "d0questpack", "ascension", "refinements",
			"spell_rev", "tactics", "wheels", "eetact2"],
			before = ["cdtweaks", "eet_end"],),
		"1500:1510" => (before = ["faiths_and_powers"],),
		1500 => (path = ["Spells", "New spells"],),
		1510 => (path = ["Spells", "New spells"],),
# 		2110 => (exclusive = "spcl231.spl", path=["Classes", "Paladin"],), # inquisitor dispel
# 		2111 => (exclusive = "spcl231.spl", path=["Classes", "Paladin"],), # inquisitor dispel
		2900 => (path = ["Items"],),
		3010 => (path = ["Items", "Magic weapons"],),
		3020 => (path = ["Items", "Magic weapons"],),
		3021 => (path = ["Items", "Magic weapons"],),
		3022 => (path = ["Items", "Magic weapons"],),
		3040 => (path = ["Items", "Stores"],),
		3041 => (path = ["Items", "Stores"],),
		3500 => (path = ["Spells"],),
		3501 => (path = ["Spells"],),
		3505 => (path = ["Items", "Scrolls"],),
		3540 => (path = ["Classes", "Paladin"],),
		3541 => (path = ["Classes", "Paladin"],),
		3550 => (path = ["Spells", "Healing"],),
		3551 => (path = ["Spells", "Healing"],),
		3552 => (path = ["Spells", "Healing"],),
		3580 => (path = ["Spells", "Healing"],),
		4000 => (path = ["Creatures"],),
		4020 => (path = ["Creatures"],),
		4030 => (exclusive = ["spcl643.spl", "spcl644.spl", "spcl611.spl", "spcl612.spl"],
			path=["Skills", "Shapeshifting"],),
		4050 => (path = ["Skills", "Reputation"],),
		4051 => (path = ["Skills", "Reputation"],),
		4052 => (path = ["Skills", "Reputation"],),
		4093 => (path = ["Skills", "Reputation"],),
		4099 => (path = ["NPC"],),
		4100 => (path = ["NPC"],),
		4115 => (path = ["Skills", "Thieving"],),
# 		4120 => (exclusive = "NPC_at_inn",),
		4230 => (exclusive = ["wmart1.cre", "wmart2.cre"], path=["Items", "Stores"],), # Joluv, Deidre
		4145 => (path = ["Story", "BG1"],),
		4146 => (path = ["Story", "BG1"],),
		4150 => (path = ["Story", "BG2"],),
		4160 => (path = ["Story", "BG2", "Magic license"],),
		4161 => (path = ["Story", "BG2", "Magic license"],),
		4162 => (path = ["Story", "BG2", "Magic license"],),
		4163 => (path = ["Story", "BG2", "Magic license"],),
		4164 => (path = ["Story", "BG2", "Magic license"],),
		4170 => (path = ["Story", "BG2", "Gaelan Bayle"],),
		4171 => (path = ["Story", "BG2", "Gaelan Bayle"],),
		4172 => (path = ["Story", "BG2", "Gaelan Bayle"],),
		4173 => (path = ["Story", "BG2", "Gaelan Bayle"],),
		4174 => (path = ["Story", "BG2", "Gaelan Bayle"],),
		4190 => (path = ["Story", "ToB"],),
		4210 => (path = ["Story", "ToB"],),
		4240 => (path = ["Spells", "HLA"],),
# 		5020 => (exclusive = "cloak_of_displacement",),
# 		5030 => (exclusive = "cloak_of_mirroring",),
		5070 => (path = ["Cosmetic", "Sprites"],),
		5900 => (exclusive = "AI",),
		6200 => (exclusive = ["spidsw.cre", "spidhu.cre", "spidph.cre"], path=["Creatures"],),
		6300 => (exclusive = ["sirspell.bcs", "hama.bcs",], path=["Creatures"],),
		6310 => (exclusive = ["crawler.bcs",], path=["Creatures"],),
		6320 => (exclusive = ["lbasilsk.bcs", "gbasilsk.bcs",], path=["Creatures"],),
		6500 => (exclusive = ["golcly01.cre", "golsto01.cre", "goliro01.cre", "golice01.cre", "golbra01.cre",], path=["Creatures"],),
		6510 => (exclusive = ["smarter_demons", "demfig01.cre", "demfig02.cre", "dembal01.cre", "dembal02.cre", "demglab.cre", "demglab2.cre", "glabrez.cre", "tanari.cre", "tanari2.cre", "marilith.cre", "demsuc.cre", "pwarden.cre", "dembal01.bcs", "demglab.bcs", "tanari.bcs", "cornugon.bcs", "erinyes.bcs"], path=["Creatures", "Fiends"],),
		6520 => (exclusive = "efreet01.bcs", path=["Creatures"],),
		6540 => (exclusive = ["dragred1.itm", "firkra02.cre", "gorsal.cre", "fsdragon.cre", "dragblac.cre", "udsilver.cre", "shadra01.cre", "dragblue.cre", "dragblac.bcs", "dragbrow.bcs", "dragred.bcs"], path=["Creatures", "Dragons"],),
		6550 => (exclusive = ["beheld01.cre", "udelder.cre", "hlvaxal.cre", "hlhive.cre", "behold01.bcs", "behdir01.bcs", "gauth01.bcs"], path=["Creatures"],),
		6560 => (exclusive = ["udmaster.cre", "flayer01.bcs", "flayer02.bcs", "flayun.bcs"], path=["Creatures"],),
		6570 => (exclusive = ["gith01.bcs", "gith01.cre", "gorgit.cre"], path=["Creatures"],),
		6580 => (exclusive = ["vampir01.bcs", "nevm4.bcs", "vampm01.cre", "vampjah.cre", "lassal.cre", "c6valen.cre", "bodvam01.cre",], path=["Creatures", "Undead"],),
		6590 => (exclusive = ["finmel.bcs", "meliss01.bcs",], path=["Creatures"],),
		6800 => (depends = "ascension", exclusive = ["illasera.bcs"], path=["Creatures"],),
		6810 => (depends = "ascension", exclusive = ["jumjum.bcs", "fingrom.bcs"], path=["Creatures"],),
		6820 => (depends = "ascension", exclusive = ["yagaft.bcs", "finyaga.bcs"], path=["Creatures"],),
		6830 => (depends = "ascension", exclusive = ["abaz2.bcs", "drake.bcs"], path=["Creatures"],),
		6840 => (depends = "ascension", exclusive = ["finsend.bcs", "finsend.cre", "finiren.cre"], path=["Creatures"],),
		6850 => (depends = "ascension", exclusive = ["finbalor.bcs", "finaluf.bcs", "finmaril.bcs", "finnabas.bcs"], path=["Creatures", "Fiends"],),
		7000 => (exclusive = ["noblpa.bcs", "doppss.bcs", "zorl.bcs", "sardopp.bcs"], path=["Creatures"],),
		7010 => (exclusive = ["ronelit.cre", "ronguar.cre", "irongu.cre"], path=["Creatures"],),
		7020 => (exclusive = ["lamalh.cre", "molkar.cre", "maneir.cre", "telka.cre", "zeela.cre", "drakar.cre", "halaca.cre", "morvin.cre", "drakar.bcs", "halaca.bcs", "morvin.bcs", "molkar.bcs", "maneir.bcs", "lamalh.bcs"], path=["Creatures"],),
		7030 => (exclusive = ["kobolda.cre", "kobold.cre",], path=["Creatures"],),
		7040 => (exclusive = ["grema_d.cre", "ogrema.cre",], path=["Creatures"],),
		7050 => (exclusive = ["ichary.cre",],),
		7060 => (exclusive = ["kaishwlf.cre", "baresh.bcs", "baresh2.cre", "barwlf.cre", "daese.cre"],),
		7070 => (exclusive = ["lovem.cre", "love.bcs", "fearm.cre", "avaricem.cre", "greed.bcs", "islann.cre", "fuerne.cre", "dopdur.cre", "dopdur1.cre", "dopdop.bcs", "rook.cre", "bishop.cre",],),
		7080 => (exclusive = ["tanar.cre", "tracea.cre", "cultt1.cre"],),
		7090 => (exclusive = ["takiyah.cre", "izefia.cre", "corsone.cre", "osmadi.cre", "amaran.cre",],),
		7100 => (exclusive = "bassil.cre",),
		7110 => (exclusive = ["drasus.cre", "gentho.cre", "kysus.cre", "rezdan.cre"],),
		7130 => (exclusive = ["denak.cre", "brenda.cre", "lasala.cre", "diana.cre"],),
		7140 => (exclusive = ["shaldr.cre", "rahvin.cre", "haseo.cre", "wudei.cre"],),
		7150 => (exclusive = ["carsa.cre", "kahrk.cre"],),
		7200 => (exclusive = ["kobold7.cre", "mulahe.cre", "helpmul.cre"],),
		7210 => (exclusive = ["credus.cre", "bandit.bcs", "ardeno.cre", "taugos.cre"],),
		7220 => (exclusive = ["battho.cre",],),
		7250 => (exclusive = ["sarevo.cre", "tazok2.cre", "semaj.cre", "galdor.cre", "diarmid.cre",],),
		7900 => (exclusive = ["slythe.cre", "jenkal.cre", "tamoko.cre", "sunin.cre", "hairto.cre", "gnarl.cre", "telka.cre",],),
		8000 => (exclusive = ["duearc01.cre", "ar0602.are", "ar0603.are",],),
		8010 => (exclusive = ["shadel.bcs", "shadel.cre", "shalt01.cre", "rngsha02", ],),
		8020 => (exclusive = ["hldemi.cre", "demilich.cre"],),
		8040 => (exclusive = ["spwnbeh.bcs", "spwndead.bcs", "spwndrow.bcs", "spwngol.bcs", "spwnmind.bcs", "spwnmon.bcs", "spwnorc.bcs", "spwnrak.bcs", "spwntoa.bcs", "spwntrol.bcs", "spwnvamp.bcs", "spwnwolf.bcs"],),
		8050 => (exclusive = ["reband01.cre",],),
		8060 => (exclusive = ["torgal.cre", "torgal3.cre", "trolsi01.cre", "trolsp01.cre", "pptroll1.cre", "ar1300.are", "ar1301.are", "ar1302.are", "ar1303.are"],),
		8070 => (exclusive = ["bheye.cre", "bheye.bcs",],),
		8080 => (exclusive = ["bodhi2.cre", "ppbodhi4.cre", "chevil08.cre", "bodhi2.bcs", "bodfgt01.cre", "bodfgt02.cre"],),
		8090 => (exclusive = ["ar1515.are", "ar1512.are",],),
		8100 => (exclusive = ["ppmur.cre", "ppjon.cre",],),
		8110 => (exclusive = ["sahamb01.bcs", "sahamb02.bcs", "sahamb01.cre", "sahbar01.cre", "sahpri01.cre", "sahpr2.cre", "sahkng01.cre"],),
		8120 => (exclusive = ["ar2101.are"],),
		8140 => (exclusive = ["drow06.cre", "drow01.cre", "uddrow01.cre", "daqilue.cre", "jael01.cre", "dagmag01.cre", ],),
		8150 => (exclusive = ["gormind.cre", "gorsta01.cre", "ar3005.are", "ar3006.are", "ar3021.are"],),
		8160 => (exclusive = ["gorsku01.bcs", "golbur01.bcs", "elemimix.bcs", "hgwar01.cre", "ysfire01.cre", "giafir01.cre", "ysguar01.cre",],),
		8170 => (exclusive = ["senlich.cre", "elemogre.cre", "drofod02.cre",],),
		8180 => (exclusive = ["bazliz03.cre", "bazliz04.cre", "eyesek01.cre", "eyesnt01.bcs", "bazmonk.cre", "gorsal.bcs"],),
		8190 => (exclusive = ["smound01.cre", "ar1900.are", "bhguard2.cre", "talkni01.cre", "talmiss.cre", "druear01.cre", "elear01.cre", "elearg01.cre", "udelda.cre", "elair01.cre", "udelf.cre", "udelde.cre", "spmugg2.cre", "spmugg.cre",],),
	)
	setmod!("sword_and_fist",
		1 => (path=["Classes", "Monk"],),
		30 => (path=["Classes", "Fighter"],),
		31 => (path=["Classes", "Fighter"],),
		32 => (path=["Classes", "Fighter"],),
		33 => (path=["Classes", "Fighter"],),
		34 => (path=["Classes", "Fighter"],),
	)
	setmod!("swordsaint", 0 => (path=["Classes", "Fighter"],))
	setmod!("thalan", 0 => (path=["Items", "Upgrades"],),)
	setmod!("themed_tweaks",
		60 => (path=["Story", "SoD"],),
		70 => (path=["Story", "SoD"],),
		80 => (path=["Story", "SoD"],),
		90 => (path=["Story", "SoD"],),
		100 => (path=["Story", "SoD"],),
		140 => (path=["Story", "SoD"],),
		150 => (path=["Story", "SoD"],),
		160 => (path=["Story", "SoD"],),
		180 => (path=["Story", "BG2"],),
		200 => (path=["Story", "BG2", "Ending"],),
	)
	setmod!("therune",
		"" => (after = ["cowledmenace"],),
	)
	setmod!("thevanishingofskiesilvershield", 0 => (path=["NPC", "Eldoth"],))
	setmod!("tnt",
		"" => (after = ["ctb",],),
		0 => (exclusive = ["fmcat.cre", "fmmep.cre", "fmfae.cre", "fmfer.cre", "fmimp.cre", "fmqua.cre", "fmrab.cre", "famps.cre", "fmspd.cre", ], path=["Skills", "Familiar"],), # familiars
		1 => (path=["Skills", "Familiar"],),
		2 => (path=["Skills", "Thieving"],),
		3 => (path=["Skills", "Thieving"],),
		4 => (path=["Skills", "Thieving"],),
		5 => (path=["Skills", "Thieving"],),
		6 => (path=["Skills", "Thieving"],),
		7 => (path=["Skills", "Thieving"],),
		8 => (path=["Skills", "Thieving"],),
		9 => (path=["Skills", "Thieving"],),
		10 => (path=["Skills", "Thieving"],),
		11 => (path=["Skills", "Thieving"],),
		12 => (path=["Skills", "Thieving"],),
		13 => (path=["Skills", "Thieving"],),
		14 => (path=["Skills", "Thieving"],),
		15 => (path=["Skills", "Thieving"],),
		24 => (path=["Items", "Ammunition"],),
		25 => (path=["Items", "Ammunition"],),
		26 => (path=["Items", "Ammunition"],),
		27 => (path=["Items", "Ammunition"],),
		28 => (path=["Cosmetic", "Sprites"],),
		29 => (path=["Items"],),
		30 => (path=["Items", "Potions"],),
		31 => (path=["Items", "Wands"],),
		32 => (path=["Items", "Wands"],),
		33 => (path=["Items", "Potions"],),
		34 => (path=["Items", "Stacking"],),
		35 => (path=["Items"],),
		36 => (path=["Items"],),
		37 => (path=["Items", "Weapons"],),
		38 => (path=["Items"],),
		39 => (path=["Items"],),
		40 => (path=["Items", "Protection"],),
		41 => (path=["Skills", "Shapeshifting"],),
		42 => (path=["Skills", "Bhaalspawn"],),
		43 => (path=["Skills", "Bhaalspawn"],),
		44 => (path=["Skills", "Bhaalspawn"],),
		45 => (exclusive = "spwi609.spl", path=["Spells", "Divination"]),
		46 => (exclusive = ["spwi413a.spl","spwi413d.spl"], path=["Spells", "Alteration"],),
		47 => (path=["Spells", "Enchantment"],),
		48 => (exclusive = ["spwi515.spl", "spwi609.spl", "spcl505.spl"], path=["Spells", "Illusion"],),
		49 => (path=["Spells", "Enchantment"],),
		50 => (exclusive = "spwi703.spl", path=["Spells", "Illusion"],),
		51 => (exclusive = "spwi607.spl", path=["Spells", "Illusion"],),
		56 => (exclusive = "taerom_ankhegs", path=["Items", "Upgrades"],),
		52 => (path=["Skills", "Blade spin"],),
		53 => (exclusive = "store_prices", path=["Items", "Stores"],),
		54 => (path=["Items", "Stores"],),
		55 => (path=["Items", "Stores"],),
		57 => (path=["Items", "Stores"],),
		60 => (path=["Tables", "XP"],),
		61 => (path=["Items"],),
		62 => (path=["Fighting", "Proficiencies"],),
		63 => (path=["Classes", "Restrictions"],),
		65 => (path=["Story", "BG2", "Stronghold"],),
		66 => (path=["Story", "BG1"],),
		68 => (path=["Cosmetic", "Maps"],),
	)
	setmod!("tomeandblood",
		"" => (after = ["spell_rev"],),
		11 => (exclusive = ["spwi607.spl", "spwi703.spl", "spwi804.spl"],
			path=["Spells", "Schools"],), # simulacrum
		13 => (path=["Spells", "Illusion"],),
		14 => (exclusive = ["spwi203.spl", "spwi224.spl", "sppr309.spl", "spwi322.spl", "scrl6k.itm", "spwi224.spl", "spwi515.spl", "spwi609.spl"], path=["Spells", "Illusion"],), # invisibility etc.
		16 => (exclusive = "spwi110.spl", path=["Spells", "Identify"],), # identify
		20 => (path=["Classes", "Sorcerer"],),
		25 => (path=["Classes", "Sorcerer"],),
		31 => (path=["Classes", "Sorcerer"],),
		33 => (path=["Classes", "Sorcerer"],),
		35 => (path=["Classes", "Sorcerer"],),
		37 => (path=["Classes", "Sorcerer"],),
		40 => (path=["Classes", "Mage"],),
		48 => (exclusive = "armored_spellcasting", path=["Skills", "Casting"]),
		51 => (path=["Spells", "Metamagic"],),
		52 => (path=["Spells", "Metamagic"],),
		53 => (path=["Spells", "Metamagic"],),
		54 => (path=["Spells", "Metamagic"],),
		61 => (path=["Spells"],),
		62 => (path=["Spells"],),
		63 => (path=["Spells"],),
		66 => (exclusive = "scrl77.itm", path=["Skills", "Familiar"],),
		67 => (exclusive = "select_familiar", path=["Skills", "Familiar"],),
		68 => (path=["Skills", "Familiar"],),
		69 => (exclusive = "familiar_penalty", path=["Skills", "Familiar"],),
		71 => (exclusive = "spell_switching", path=["Classes", "Sorcerer"],),
		72 => (path=["Classes", "Sorcerer"],),
		80 => (path=["Classes", "Sorcerer"],),
		82 => (path=["Classes", "Mage"],),
		85 => (path=["Classes", "Sorcerer"],),
		92 => (path=["Classes", "Mage"],),
		93 => (path=["Classes", "Mage"],),
		1201 => (path=["Spells", "Schools"],),
		1202 => (path=["Spells", "Schools"],),
		1203 => (path=["Spells", "Schools"],),
	)
	setmod!("transitions",
		0 => (path=["Story", "BG1", "Ending",],),
		10 => (path=["Story", "BG1", "Ending",],),
		20 => (path=["Story", "BG1", "Ending",],),
		21 => (path=["Story", "BG1", "Ending",],),
		22 => (path=["Story", "BG1", "Ending",],),
		30 => (path=["Story", "BG1", "Ending",],),
		31 => (path=["Story", "BG1", "Ending",],),
		32 => (path=["Story", "BG1", "Ending",],),
		33 => (path=["Story", "BG1", "Ending",],),
		200 => (path=["Story", "BG1", "Ending",],),
		40 => (path=["Story", "BG1", "Ending",],),
		50 => (path=["Story", "BG1", "Ending",],),
		60 => (path=["Story", "BG1", "Ending",],),
		70 => (path=["Story", "BG1", "Ending",],),
		71 => (path=["Story", "BG1", "Ending",],),
		72 => (path=["Story", "BG1", "Ending",],),
		73 => (path=["Story", "BG1", "Ending",],),
		80 => (path=["Story", "BG1", "Ending",],),
		130 => (path=["Story", "SoD", "Ending",],),
		140 => (path=["Story", "BG2", "Ending",],),
	)
	setmod!("ub",
		0 => (path=["NPC", "Minsc"],),
		1 => (path=["NPC", "Valygar"],),
		3 => (path=["Story", "BG2", "Brynnlaw"],),
		13 => (path=["NPC", "Yoshimo"],),
		14 => (path=["NPC", "Anomen"],),
		19 => (path=["Skills", "Bhaalspawn"],),
		22 => (path=["Classes", "Ranger"],),
		23 => (path=["Classes", "Ranger"],),
		24 => (path=["NPC", "Sarevok"],),
	)
	setmod!("valhorn", 0 => (path=["Items", "Upgrades"],))
	setmod!("valygarfriendship", 0 => (path = ["NPC", "Valygar"],))
	setmod!("viconia", 0 => (path = ["NPC", "Viconia"],))
	setmod!("wheels",
		"" => (before = ["stratagems"],),
	),
	setmod!("wildmage",
		0 => (path=["Spells", "New spells"],),
		1 => (path=["Spells", "New spells"],),
		2 => (path=["Items"],),
		3 => (path=["Classes", "Mage"],),
		4 => (path=["Classes", "Mage"],),
		5 => (path=["Spells", "Alteration"],),
	)
	setmod!("wilsonchronicles", 0 => (path = ["NPC", "Wilson"],))
	setmod!("wtpfamiliars", 0 => (path = ["Skills", "Familiar"],))
	setmod!("xan",
		0 => (path = ["NPC", "Xan"],),
		1 => (path = ["NPC", "Xan"],),
		2 => (path = ["NPC", "Xan"],),
		3 => (path = ["NPC", "Xan"],),
		4 => (path = ["NPC", "Xan"],),
		5 => (path = ["NPC", "Xan"],),
		6 => (path = ["NPC", "Xan"],),
	)
	setmod!("xanbg1friend", 0 => (path = ["NPC", "Xan"],))
	setmod!("yeslicknpc",
		0 => (path=["NPC", "Yeslick"],),
		1 => (path=["NPC", "Yeslick"],),
	)
	setmod!("yoshimo", 0 => (path = ["NPC", "Yoshimo"],))
	setmod!("yoshimosremorse",
		0 => (path = ["NPC", "Yoshimo"],),
		1 => (path = ["NPC", "Yoshimo"],),
		2 => (path = ["NPC", "Yoshimo"],),
	)
		# extract data from already downloaded/extracted mods
		for (id, mod) in (simulate ? Iterators.take(moddb, 20) : moddb)
# 		for (id, mod) in Iterators.take(moddb, 20)
			if isextracted(mod)
				update(mod; moddb)
			end
			for f in id.*(".tar.gz", ".zip", ".7z", ".rar")
				isfile(joinpath(DOWN, f)) && (mod.archive = f; break)
			end
		end

	global moddb = moddb
	simulate && (dest = stdout)
	printsim("writing moddb $dest")
	write_moddb(dest; moddb)
end

#««1 Selection handling
function bws_selection(source = BWS_USER)#««
	printsim("reading selection from $source")
	section = ""
	selection = Dict{String,Set{String}}()
	for line in eachline(source)
		if line[1] == '[' && line[end] == ']'
			section = line[2:end-1]
			continue
		end
		if section == "Save" && occursin('=', line)
			id, components = split(line, r"\s*=\s*"; limit=2)
			push!(get(selection, lowercase(id), Set{String}()), split(components)...)
		end
	end
	return selection
end#»»
function import_bws_selection((source, dest) = BWS_USER => SELECTION)#««
	selection = bws_selection(source)
	maybe_rewrite_moddb() do
		for id in keys(selection)
			m = moddb[id]
			isextracted(m) || extract(m)
		end
	end
	save(dest; moddb, selection)
end#»»

end
isinteractive() || ModDB.import_bws_moddb()
# TODO: make a command-line option for this:
# don't do this if selection is more recent than BWS config:
# import_bws_selection()
