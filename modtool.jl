""" ModTool - BG2 modding tool

Usage:

ModTool.import_bws_moddb()
ModTool.status(all)
ModTool.edit(:stratagems)
ModTool.update_selection()
ModTool.install(all)
"""
module ModTool
# Preamble ««1
# TODO ««
# mod-specific fixes:
#  - questpack is very badly named
#  - mortis: tra/Francais -> tra/French
#  - aurora: fix .sh files
#  - spstuff: remove unicode BOM from ee.tra
#  - Keto SoAv5 vs SoAv6
#  - mih_ip fails to download
# - Generic installation order
# (https://forums.beamdog.com/discussion/34882/list-of-bg2ee-compatible-mods)
# + fix updating selection file
# + make a persistent selection file
# + store all mod properties in moddb (only English versions of texts?)
# + add a new database for computed properties (readme/release/etc.)
# - use labels like ProjectInfinity
# - use ProjectInfinity's *.ini files if available
#  - needs extracting mods before (re)computing install order
#  - including readme if needed (still prefer local readme)
# + pass install-order to help install(first)
# - github: auto-guess mod dir + name
# + use WeiDU's --list-* options
#  + list-languages
#  + list-components
#  + list-readme [MODDED]
#  + list-actions [MODDED]
# - do a complete EET installation routine
# + add the following characteristics at a component-level:
#  + before/after: declare install order
#  - depends/conflicts: declare relations
# - complete dependency & conflict checking
# - try and guess readme for individual components (IMPOSSIBLE)
#»»
using Printf
using Dates
using TOML
using JSON
using DataStructures
using HTTP
using LibGit2

const HOME=ENV["HOME"]
const GAME_LANG=(r"fran.*ais"i, r"french"i, r"english"i, r"american"i)
const TOOL_LANG=(r"english"i, r"american"i, r"fran.*ais"i, r"french"i)
const PREFIX="$HOME/jeux/ciopfs/modtool"
const MODDB="$PREFIX/moddb.toml"
const SELECTION="$PREFIX/selection"
const DOWN="$PREFIX/down" # should be in a ciopfs
const MODS="$PREFIX/mods" # should be in a ciopfs
const TEMP="$PREFIX/temp" # should be in a ciopfs

const GAMEDIR =
	(bg1="$HOME/jeux/ciopfs/bg1/game", bg2="$HOME/jeux/ciopfs/bg2/game")
const GAME_LIST = keys(GAMEDIR)
	
function init()# setup global variables and filesystem
	global global_moddb = read_moddb(MODDB)
	printlog("read $(length(global_moddb)) mods in global database")
	global global_selection = read_selection(SELECTION; verbose=false)
	printlog("read selection status for $(length(global_selection)) mods; "*
	"$(sum(length.(values(global_selection)))) components selected")
	for d in (DOWN, MODS, TEMP) ispath(d) || mkdir(d); end
end

function maybe_rewrite_moddb(f; file=MODDB)
	global moddb_changed = false
	f()
	moddb_changed && write_moddb()
end
function __init__()
	isdefined(Main, :modtool_no_init) || init()
# 	isinteractive() && init()
end

@inline fixutf8(s::AbstractString)= isvalid(s) ? s : String(Char.(codeunits(s)))
@inline printlog(s...) = println(s...)
@inline printsim(s...) = println("\e[35;1m", s..., "\e[m")
@inline printerr(s...) = println("\e[31;1m", s..., "\e[m")
@inline printwarn(s...) = println("\e[33;1m", s..., "\e[m")
@inline printask(s...) = println("\e[36;1m", s..., "\e[m")

# Data structures ««1

mutable struct ModComponent
	id::String
	name::String
	group::String
	subgroup::String
	# compatibility properties:
	after::Vector{String}
	before::Vector{String}
	conflicts::Vector{String}
	depends::Vector{String}
	exclusive::Vector{String}
	path::Vector{String} # e.g. ["Weapons&Armor", "Proficiencies"]
	@inline ModComponent(i, t, g="", s="") =
		new(string(i), t, g, s, [], [], [], [], [], [])
	@inline ModComponent(d::AbstractDict) =
		new((get(d, k,"") for k in ("id", "name", "group", "subgroup"))...,
		 (get(d,k,[]) for k in ("after", "before", "conflicts", "depends", "exclusive", "path"))...)
end
@inline Base.isempty(c::ModComponent) =
	all(isempty(getfield(c, i)) for i in 1:fieldcount(ModComponent))
@inline db_prop(c::ModComponent, pairs...) =
	Dict(pairs..., (string(s) => getfield(c,  s)
	for s in fieldnames(ModComponent) if !isempty(getfield(c, s)))...)
@inline description(c::ModComponent) =
	let n = isempty(c.name) ? "<unknown>" : c.name
	isempty(c.subgroup) ? n : c.subgroup*'/'*n
	end
mutable struct Mod
	# data stored in moddb file:
	id::String
	url::String
	description::String
	archive::String
	class::String # install rank
	lastupdate::Date
# 	properties::Dict{String,ComponentProperties}
	# this data exists only once the mod is extracted:
	readme::String # can exceptionally be hardcoded
	tp2::String
	languages::Vector{String}
	compat::ModComponent # holds compatibility (etc.) for whole mod
	components::Vector{ModComponent} # sorted as in tp2 file
	game_lang::Int # starts at zero (WeiDU indexing)
	tool_lang::Int

	@inline Mod(;id, url="", description="", class="", archive="", tp2="",
		lastupdate=1970, languages = String[], readme="", components=[]) = begin
		new(lowercase(id), url, description, archive, class, Date(lastupdate),
			readme, tp2, languages, ModComponent("", ""),
			[ ModComponent(k,v) for (k,v) in pairs(components)], 0, 0)
		end
end
# Mod classes for install order#««
# (https://forums.beamdog.com/discussion/34882/list-of-bg2ee-compatible-mods)
# (this is somewhat different from EE Mod Setup's ordering)
const MOD_CLASSES = split(replace("""
	DlcMerger
	Initial
	Early
	Fixes
	BigQuests
	Quests
	Quest
	SmallQuests
	NPC
	SmallNPC
	NPC-Related
	Spells
	Items
	Kits
	Tweak # merges “tactical”, “rules” and “AI”
	Tweaks
	AI
	Sound
	Late
	UI
	Final
	BADCLASS
""", r"#[^\n]*\n" =>"\n"))# »»
@inline modarchive(m::Mod) = m.id*match(r"\.[^.]*$", m.url).match
@inline modgame(m::Mod; moddb=global_moddb) =
	m.id ∈ moddb["eet"].compat.after ? :bg1 : :bg2
@inline modcomponents(m::Mod) = (m.components)
@inline function modcomponent!(m::Mod, i, c)
	i = something(i, length(m.components)+1)
	resize!(m.components, max(length(m.components), i))
	m.components[i] = c
end

# Mod DB handling ««1
@inline ifhaskey(f, d, k) = (x = get(d, k, nothing); isnothing(x) || f(x))
@inline addmods!(moddb, mods...) = for m in mods; moddb[m.id] = m; end
const mod_fields=(:id,:url,:class,:description,:archive,:readme,:languages,:tp2)
function merge_moddb(filename = MODDB; moddb = global_moddb)
	dict = TOML.parsefile(filename)
	for (id, d) in dict
		m = get!(moddb, id, Mod(;id))
		for k in mod_fields
			ifhaskey(d, string(k)) do x; setfield!(m, k, x); end
		end
		ifhaskey(d, "lastupdate") do x; m.lastupdate = Date(x); end
		ifhaskey(d, "compat") do x; m.compat = ModComponent(x); end
		ifhaskey(d, "components") do x; for (k,prop) in x
			# using dict constructor for `ModComponent`:
			modcomponent!(m, parse(Int, k), ModComponent(prop))
		end end
	end
	return moddb
end
@inline read_moddb(filename) = merge_moddb(filename; moddb=Dict{String,Mod}())
function write_moddb(io::IO; moddb=global_moddb)
	TOML.print(io, moddb; sorted=true) do m
		d = Dict{String,Any}()
		for k in mod_fields
			x = getfield(m, k); isempty(x) || (d[string(k)] = x)
		end
		d["lastupdate"] = m.lastupdate
		isempty(m.compat) || (d["compat"] = db_prop(m.compat))
		!isempty(m.components) && (d["components"] = Dict(
			string(i)=>db_prop(c) for (i,c) in pairs(m.components) if !isempty(c)))
		d
	end
end
function write_moddb(filename::AbstractString = MODDB; moddb=global_moddb)
	printlog("writing moddb: $filename")
	mktemp(TEMP) do path, io # try this in a temporary file first
		write_moddb(io; moddb)
		close(io); read_moddb(path) # catch errors...
		mv(path, filename; force=true)
	end
end

@inline function findmod(id::Union{Symbol,String}; moddb = global_moddb)
	k = get(moddb, lowercase(string(id)), nothing)
	isnothing(k) || return k
	error("mod '$id' not found")
end
@inline findmod(pattern::Regex; moddb = global_moddb) =
	[ id for id in keys(moddb) if occursin(pattern, id) ]
@inline function Base.setproperty!(m::Mod, k::Symbol, v)
	x = getfield(m, k)
	v == x && return
	global moddb_changed = true
	setfield!(m, k, convert(typeof(x), v))
end
@inline function findcomponent(mod, k)
	k = lowercase(string(k)); for c in mod.components; c.id == k && return c; end
# 	error("mod '$(mod.id)': component '$k' not found")
end

"Returns the dependency matrix for these mods, encoded as
(arrowsfrom = dict(mod1 => mod2, ...), arrowsto = dict(mod1 => n1, ...))"
function dependencies(list ;moddb=global_moddb)
	arrowsfrom = Dict{String,Set{String}}(); arrowsto = copy(arrowsfrom)
	function connect((b, a),)
		fb = get!(arrowsfrom, b, Set{String}())
		a ∈ fb && return
		push!(fb, a); push!(get!(arrowsto,a, Set{String}()), b)
	end
	for id in list
		for c in modcomponents(findmod(id;moddb))
			# special: empty key indicates a dependency for the whole mod
			isempty(c.id) || continue
			isempty(c.after) && isempty(c.before) && continue
			for a in c.after; a ∈ list && connect(a => id); end
			for b in c.before; b ∈ list && connect(id => b); end
		end
	end
	return (arrowsfrom = arrowsfrom, arrowsto = arrowsto)
end
function comp_isless((id1, k1), (id2, k2), order)
	id1 == id2 && return isless(parse(Int, k1), parse(Int, k2))
	return isless(findfirst(==(id1), order), findfirst(==(id2), order))
end
@inline modclass(m::Mod) =
	let k = (findfirst(==(m.class), MOD_CLASSES))
	isnothing(k) && error("mod \"$(m.id)\": bad mod class \"$(m.class)\"")
	k; end
function sortkey1(id, moddb, dep)
	# returns (class(id), max class(before id), date(id))
	c0 = modclass(moddb[id])
	prev = get(dep.arrowsto, id, [])
	c1 = maximum(modclass(moddb[x]) for x in prev; init = -1)
	(c0, c1, moddb[id].lastupdate)
end
@inline sortkey1(moddb, dep) = id -> sortkey1(id, moddb, dep)
"Returns a list of mod IDs in install order, given all dependencies"
function installorder(list; moddb=global_moddb)
	# use Kahn's algorithm for topological sorting
	# (this should be average-time quasi-linear with the use of a heap)
	#
	# bias it to the right (most mods want to be installed late...)
	ret = sizehint!(String[], length(list))
	todo = Set(collect(list))
	dep = dependencies(list;moddb)
	ord = Base.Order.By(sortkey1(moddb, dep), Base.Order.Reverse)
	available = [ id for id in list if isempty(get(dep.arrowsfrom, id, [])) ]
	heapify!(available, ord)
			
	while !isempty(todo)
		# loop invariant: arrowsfrom[y] = count of x ∈ todo such that x->y
		@assert !isempty(available) "Circular dependency found: $(todo)"
		x = heappop!(available, ord)
		push!(ret, x); delete!(todo, x)
		for y in get(dep.arrowsto, x, String[])
			delete!(dep.arrowsfrom[y], x)
			isempty(dep.arrowsfrom[y]) && heappush!(available, y, ord)
		end
	end
	reverse!(ret)
end
function check_conflicts(;selection = global_selection, moddb=global_moddb)
	owners = Dict{String, Vector{Tuple{String, String}}}()
	# check exclusivity
	for (id, clist) in selection
		mod = findmod(id; moddb)
		for c in modcomponents(mod)
			c.id ∈ clist || continue
			for e in c.exclusive
				push!(get!(owners, e, valtype(owners)([])), (id, c.id))
			end
		end
	end
	for (x, v) in owners; length(v) ≤ 1 && continue
		printwarn("tag \"$x\" is owned by the following components:")
		for (id, c) in v; println("  ", id, ":", c); end
	end
end


# Pre-TP2 functions: download, extract, status ««1
function updateurl(mod::Mod)
	printlog("get latest release for $(mod.url)")
	repo = mod.url[8:end]
	api = "https://api.github.com/repos/$repo/releases/latest";
	req = HTTP.get(api; status_exception=false)
# 	proc = run(pipeline(ignorestatus(`wget https://api.github.com/repos/$repo/releases/latest -O -`), stdout=fd))
	req.status ≠ 200 && (printwarn("failed to download $api"); return nothing)
	for line in split(String(req.body), '\n')
		m = match(r"\"browser_download_url\"\s*:\s*\"([^\"]*\.(zip|rar|7z|tar\.gz)*)\"", line)
		m ≠ nothing && (mod.archive=basename(m.captures[1]);
		return m.captures[1])
		m = match(r"\"tarball_url\"\s*:\s*\"([^\"]*)\"", line)
		m ≠ nothing && (mod.archive=mod.id*".tar.gz"; return m.captures[1])
	end
end
function download(mod::Mod; down=DOWN, mods=MODS, simulate=false)
	mod.url ∈ ("Manual", "") && return true # do nothing
	url = mod.url
	printsim("downloading $(mod.id) from $url")
	if startswith(mod.url, "github:")
		if isempty(mod.archive)
			url = updateurl(mod)
		elseif isfile(mod.archive)
			return true
		else
			url = "https://github.com/$(mod.url[8:end])/releases/download/latest/$(mod.archive)"
			printlog("  $(mod.id): computing new url=$url")
		end
	end
	if isnothing(url)
		printlog("  $(mod.id): release not found, cloning from github")
		url = mod.url
		mod.archive = mod.id*".tar.gz"
		archive = joinpath(down, mod.archive)
		moddir = joinpath(mods, mod.id)
		(isdir(mod.id) && !isempty(readdir(mod.id))) ||
			mktempdir(TEMP) do(dir); cd(dir) do
			close(LibGit2.clone("https://github.com/"*mod.url[8:end], "."))
			files = [ f for f in readdir(".") if endswith(f, r"\.tp2"i) ]
			push!(files, mod.id)
			run(`tar cvzf $archive $files`)
			for f in files; mv(f, joinpath(mods, f); force=true); end
		end end
	else
		archive = joinpath(down, mod.archive)
		while !isfile(archive) || iszero(filesize(archive))
			printsim("download $url to $archive")
			simulate && return true
			req = HTTP.get(url; status_exception = false)
			req.status == 200 &&
				(open(archive, "w") do io; write(io, req.body); end; break)
			printask("Failed to download $url\n(o)ther address, (a)bort, local (f)ile")
			action = readline()
			if action[1] == 'o' || startswith(action, r"https?://")
				url = replace(action, r"^o\s*"i => "")
			elseif action[1] == 'f'
				file = replace(action, r"^f\s*"i => "")
				ispath(file) && cp(file, archive)
			else
				return false
			end
		end
	end
	mod.url = url
	true
end
@inline isextracted(mod::Mod) = isdir(joinpath(MODS, mod.id))
function do_extract(mod::Mod; down=DOWN, mods=MODS, simulate=false)
	isextracted(mod) && return true
	lowercase(mod.archive) == "manual" && return true
	download(mod; down=DOWN) || return false
	archive = joinpath(down, mod.archive)
	mktempdir(TEMP) do(dir); cd(dir) do
		printsim("extract $archive to $dir")
		simulate && return true
		# extract archive in tmp directory««
		if endswith(mod.archive, ".zip")
			run(ignorestatus(`unzip $archive`))
		elseif endswith(mod.archive, ".7z")
			run(`7zr x $archive`)
		elseif endswith(mod.archive, ".rar")
			run(`unrar x $archive`)
		elseif endswith(mod.archive, r"\.tar\..*")
			run(`tar xf $archive`)
		else
			error("unknown archive format: ", mod.archive)
		end #»»
		write_moddb()
		# if this contains only one directory, move its contents to tmp dir««
		files = readdir()
		if length(files) == 1 && isdir(first(files))
			subdir = first(files)
			if subdir != mod.id
				for f in readdir(subdir); mv(joinpath(subdir, f), joinpath(".", f)); end
				rm(subdir)
			end
		end #»»
		# move tp2 file to $id/$id.tp2
		# skip this: mods expect their tp2 file in a particular place
# 		for f in ("setup-$(mod.id).tp2", "$(mod.id).tp2")
# 			isfile(f) &&
# 				mv(f, joinpath(mod.id, "setup-$(mod.id).tp2"); force=true)
# 		end
		# SPECIAL
		# - patch SCS to have inquisitor dispel at (level+5) instead of (1.5*level)
		@assert isdir(mod.id)
		mod.id == "stratagems" &&
			run(`sed -i -e 's,(3\*ability_true_level)/2,(ability_true_level+5),' stratagems/spell/inquisitor.tpa`)
		#  - d0questpack has a badly named directory
		mod.id == "d0questpack" &&
			(mv("questpack", mod.id); symlink(mod.id, "questpack"))
		# move extracted files to mods directory
		for file in readdir(); mv(file,  joinpath(mods, file); force=true); end
	end
	# end
# 	catch e
# 		if e isa SystemError; printerr(e)
# 		else; rethrow(e); end
	end
	return true
end
function status(mod::Mod; selection=global_selection, selected=nothing)
	sel = !isempty(get(selection, mod.id, Int[]))
	selected ∈ (sel, nothing) || return
	mod.id ∈ ("eet", "stratagems") && print("\e[34m")
	@printf("%c%c%c %7s %-22s %s\n",
		isfile(joinpath(DOWN, mod.archive)) ? 'd' : '.',
		isextracted(mod) ? 'x' : '.', sel ? 's' : '.',
		Dates.format(mod.lastupdate, "yyyy-mm"),
		mod.id, mod.description)
	print("\e[m")
end
# TP2 data extraction ««1
function lang_score(langname, pref_lang=GAME_LANG)
	for (i, pref) in pairs(pref_lang)
		occursin(pref, langname) && return i
	end
	return typemax(Int)
end
function extract(m)
	do_extract(m) && update(m)
end
@inline extract(m::Mod) = do_extract(m) && update(m)
"""    update(mod)

Assuming that this mod is extracted, update all mod info available from
extracted files: tp2 file, readme, languages, list of mod components.
"""
function update(m::Mod)
	id = m.id
	printsim("updating mod $id...")
	cd(MODS) do
	if isempty(m.tp2)
		printlog("  $id: determine tp2")
		for path in ("$id.tp2", "$id/$id.tp2", "$id/setup-$id.tp2", "setup-$id.tp2",
			"setup-$id.exe", "$id/setup-$id.exe")
			ispath(path) && (m.tp2 = path; break)
		end
		isempty(m.tp2) && printerr("no TP2 file found")
	end
	if isempty(m.languages)
		printlog("  $id: determine languages")
		m.game_lang = m.tool_lang = 0
		# m.languages
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-languages $(m.tp2)`)
			x = match(r"^(\d+):(.*)$", line); isnothing(x) && continue
			@assert parse(Int, x.captures[1]) == length(m.languages)
			push!(m.languages, fixutf8(x.captures[2]))
		end
		if !isempty(m.languages)
			m.game_lang = argmin([lang_score(l, GAME_LANG) for l in m.languages]) - 1
			m.tool_lang = argmin([lang_score(l, TOOL_LANG) for l in m.languages]) - 1
		end
	end#»»
	if all(isempty, c.name for c in m.components)
		printlog("  $id: determine components")
		prop = Dict(c.id => c for c in m.components) # store properties
		empty!(m.components) # we will store them in WeiDU-order
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-components-json $(m.tp2) $(m.tool_lang)`)
			startswith(line, "[{") || continue
			for x in JSON.parse(line)
				k = string(x["number"])
				c = get(prop, k, ModComponent(k, ""))
				c.name, c.group, c.subgroup =
					fixutf8.((x["name"], get(x["group"], 1, ""), get(x,"subgroup","")))
				push!(m.components, c)
				delete!(prop, k)
			end
		end
		for k in keys(prop); isempty(k) && continue
			found = false
			for c in m.components; c.id == k && (found=true; break); end
			found || printwarn("\e[31;1m '$(m.id):$k' not found\e[m")
		end
	end#»»
	if isempty(m.readme) # no hardcoded readme provided
		printlog("  $id: determine readme")
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-readme $(m.tp2) $(m.tool_lang)`)
			x = match(r"^R (.*)", line); isnothing(x) && continue
			isfile(x.captures[1]) && (m.readme = joinpath(MODS, x.captures[1]); break)
		end
	end
	if isempty(m.readme) # try and guess...
		printlog("  $id: try to guess readme")
		readmes = [ joinpath(root, f)
			for (root, _, files) in walkdir(joinpath(MODS, id))
			for f in files if occursin(r"readme"i, f) ]
		!isempty(readmes) && 
			(m.readme = joinpath(MODS, id, 
				readmes[argmin([ lang_score(f, TOOL_LANG) for f in readmes ])]))
	end
	if isempty(m.readme) && startswith(m.url, "https://github.com")
		printlog("  $id: using github readme")
		x = match(r"https://github.com/[^/]*/[^/]*", m.url)
		m.readme = x.match
	end
	end # cd(MODS)
	m.lastupdate = Date(unix2datetime(maximum(mtime(joinpath(root, f))
		for (root,_,files) in walkdir(joinpath(MODS, m.id))
		for f in files if endswith(f, r"\.tp[2ah]"i); init=0)))
	true
end
function readme(mod::Mod)
	if startswith(mod.readme, "https://")
		printlog("showing online documentation '$(mod.readme)'")
		run(`w3m $(mod.readme)`)
		return
	end
	extract(mod) || return false
	printlog("showing readme file '$(mod.readme)'")
	if endswith(mod.readme, r".html?"i)
		if occursin('#', mod.readme)
		# cannot call w3m directly, it fails with mesh sign in filename
		# don't run -dump — there might be links to supplemental info
			cd(dirname(mod.readme)) do
				run(pipeline(`cat $(mod.readme)`, `w3m -T text/html`))
			end
		else # to preserve links etc., run interactive w3m:
			run(`w3m $(mod.readme)`)
		end
	elseif endswith(mod.readme, r"(.txt|.md)"i)
		run(`view $(mod.readme)`)
	elseif endswith(mod.readme, r".docx"i)
		run(pipeline(`docx2txt $(mod.readme) -`, `less`))
	elseif endswith(mod.readme, r".pdf"i)
		run(pipeline(`pdftotext $(mod.readme) -`, `less`))
	elseif isempty(mod.readme)
		printlog("(no readme file found for this mod)")
	else
		error("unknown file format: $(mod.readme)")
	end
end
@inline tp2(mod::Mod) = extract(mod) && run(`view $(joinpath(MODS, mod.tp2))`)

# Mod components ««1
function weidu_installed(dirs...)
	status = Dict{String, Vector{String}}()
	for d in dirs; f = joinpath(d, "weidu.log")
		ispath(f) || continue
		for line in eachline(f)
			m = match(r"^~(.*)~\s+#(\d+)\s+#(\d+)\s*", line)
			isnothing(m) && continue
			(tp2, lang, comp) = m.captures
			id = lowercase(tp2[1:end-4])
			k = findlast('/', id); !isnothing(k) && (id = id[k+1:end])
			startswith(id, "setup-") && (id = id[7:end])
			push!(get!(status, id, String[]), comp)
		end
	end
	status
end
function printcomp(io::IO, m, c::ModComponent; selection, installed, moddb)
	@printf(io, "%c%c%c `%s:%s` %s\n",
		c.id ∈ get(selection, m.id, []) ? 's' : '.',
		c.id ∉ get(installed, m.id, []) ? '.' : modgame(m; moddb)==:bg1 ? '1' : '2',
		isempty(c.path) ? '.' : 'p',
		m.id, c.id, description(c))
end
function merge_selection(io; selection=global_selection, verbose=true)
	to_add = Dict{String,Set{String}}()
	to_del = Dict{String,Set{String}}()
	for line in eachline(io)
		m = match(r"^([.s])[.12][.p]\s+`([^`:]*):(\d+)`\s+", line)
		isnothing(m) && continue
		id, k, selected = m.captures[2], m.captures[3], (m.captures[1] == "s")
		if selected
			set = get!(selection, id, Set{String}())
			verbose && (k ∉ set) && push!(get!(to_add, id, Set{String}()), k)
			push!(set, k)
		else
			set = get!(selection, id, Set{String}())
			verbose && (k ∈ set) && push!(get!(to_del, id, Set{String}()), k)
			delete!(set, k)
		end
	end
	isempty(to_add) || printlog("Added:")
	for (k, v) in to_add; printlog("\e[32m+", k, ": ", join(v, ", "), "\e[m");end
	isempty(to_del) || printlog("Removed:")
	for (k, v) in to_del; printlog("\e[31m-", k, ": ", join(v, ", "), "\e[m");end
	return selection
end
@inline read_selection(filename; kwargs...) =
	merge_selection(filename; selection=Dict{String,Set{String}}(), kwargs...)
const selection_preamble = raw"""
" ««5 This file is to be read using the vim editor, launched as
"  vim +'so %' <name of the file>
" The following lines map some useful keys for helping modifying the selection:
" - space key (de)selects a component
" - this uses tree folding (open with zo, close with zf)
"
se fencs=utf8 cms= ft= fdm=marker fmr=««,»»
sy match modComp /\`[^\`]*\`/|hi link modComp Constant
sy match modAdd /^s\.[.p] .*$/ contains=modComp |hi link modAdd DiffAdd
sy match modDel /^\.[12][.p] .*$/ contains=modComp |hi link modDel DiffDelete
sy match modSel /^s[12][.p] .*$/|hi link modSel DiffChange
sy match modGrp /^# .*$/|hi link modGrp ModeMsg
sy match modSub /^## .*$/|hi link modSub Title
nmap <buffer> <silent> <Space> :s/^s/¤/e<cr>:s/^\./s/e<cr>:s/^¤/./e<cr>
finish "»»5 """

function component_editor(filename; selection)
# 	run(`vim $filename`)
	run(`vim -c 'so %' $filename`)
	merge_selection(filename; selection)
end
"""    edit(mod; kwargs...)
Prepares a text file containing the description and selection status of
all the components of this mod, launches an editor allowing the user
to interactively modify this selection, and integrates the resulting changes
in the selection.

The relevant lines in the text file are of the following form:

    s.. `modname:id` description

The first character is either `s` (for a selected component)
or `.` (for a deselected component).

The second character indicates the current installation status for
this component: it is either `.` (uninstalled), `1` (installed in bg1)
or `2` (installed in bg2).

The third character is either `p` (for a fully pathed component)
or `.`.

\\\``modname:id`\\\` indicates the mod and component number.

The editor (`vim`) is configured so that the space bar toggles the selection
status of each component.
"""
function edit(mod::Mod; selection=global_selection,
		gamedir = GAMEDIR[modgame(mod)], moddb=global_moddb, edit=true)
# 	extract(mod) || return
	installed = weidu_installed(gamedir)
	filename = joinpath(TEMP, "selection-"*mod.id*".txt")
	io = edit ? open(filename, "w") : stdout
	edit && println(io, selection_preamble)
	g = "\0"; h = ""
	for c in modcomponents(mod)
		g ≠ c.group && (g = c.group; println(io, "# ", g, "«"*"«1"))
		h ≠ c.subgroup &&
			(h = c.subgroup; println(io,"## ",h, isempty(h) ? "»"*"»2" : "«"*"«2"))
		printcomp(io, mod, c; selection, installed, moddb)
	end
	edit || return
	close(io)
	component_editor(filename; selection)
end
@inline components(mod::Mod; kwargs...)= edit(mod; edit=false, kwargs...)
struct ComponentTree
	alternatives::Vector{NTuple{2,String}}
	children::Dict{String,ComponentTree}
	@inline ComponentTree() = new([], Dict())
end#»»
findbranch(root::ComponentTree, path) = isempty(path) ? root :
	findbranch(get!(root.children, first(path), ComponentTree()), path[2:end])
function build_tree(;moddb=global_moddb)
	root = ComponentTree()
	for (id, m) in moddb, c in modcomponents(m)
		isempty(c.path) && continue
		push!(findbranch(root, c.path).alternatives, (id, c.id))
	end
	root
end
function display_tree(io::IO, root, level=1;
		moddb=global_moddb, selection=global_selection, installed, order)
	for (id, k) in sort(collect(root.alternatives);
			lt=(x,y)->comp_isless(x,y,order))
		m = findmod(id; moddb)
# 		extract(m);
		c = findcomponent(m, k)
		isnothing(c) && (printerr("'$id:$k' not found"); continue)
		printcomp(io, m, c; selection, installed, moddb)
	end
	for (s, c) in sort(collect(root.children); by=first)
		println(io, '#'^level, ' ', s, " ««", level)
		display_tree(io, c, level+1; selection, moddb, installed, order)
	end
end
function write_selection(io::IO; selection=global_selection,
		moddb=global_moddb, installed = weidu_installed(GAMEDIR...),
		order = installorder(keys(moddb); moddb))
	println(io, selection_preamble)
	# First: sorted components
	display_tree(io, build_tree(;moddb); selection, moddb, installed, order)
	# Then unsorted
	println(io, "# Individual, unsorted components «"*"«1")
	i = ""
	for id in order
		m = moddb[id]; g = ""; h = ""
# 		isempty(get(selection, id, ())) || extract(m)
		for c in modcomponents(m)
			isempty(c.path) || continue
			id ≠ i && (i=id; println(io, "## ", i, " «","«2"))
			g ≠ c.group && (g = c.group; println(io, "### ", g, "«","«3"))
			h ≠ c.subgroup &&
				(h=c.subgroup; println(io,"#### ",h,isempty(h) ? "»"*"»4" : "«"*"«4"))
			printcomp(io, m, c; selection, installed, moddb)
		end
	end
end
function write_selection(filename::AbstractString; kwargs...)
	printlog("writing selection (new format): $filename")
	open(filename, "w") do io; write_selection(io; kwargs...); end
end
"""    edit()

Edits the global component selection database.

This is built of two parts:
 - the first part groups all “pathed” components, i.e. those which are
   classified by game feature;
 - the second part lists all remaining unclassified components.
"""
function edit(;file=SELECTION, selection=global_selection,
		moddb=global_moddb, installed = weidu_installed(GAMEDIR...),
	order = installorder(keys(moddb); moddb))
	open(file, "w") do io
		write_selection(io; selection, moddb, installed, order)
	end
	component_editor(file; selection)
end

# Mod installation ««1
function install(mod; simulate=false, uninstall=false,
		selection=global_selection, gamedirs=GAMEDIR,
		order = installorder(keys(selection)))
	extract(mod) || return
	id = mod.id

	gamedir = gamedirs[modgame(mod)]
	current = get(weidu_installed(gamedir), mod.id, String[])
	installed = weidu_installed(gamedirs...)
	warned = false
	for k in order; k == id && break
		isempty(symdiff(get(installed, k, []), get(selection, k, []))) && continue
		println("$k: $(get(installed, k, [])) => $(get(selection, k, []))")
		printwarn("warning: mod '$k' should probably be installed before '$id'")
		warned = true
	end
	if warned
		printask("proceed anyway? (yn)"); r = readline()
		lowercase(first(r)) == 'y' || return
	end
	selected = uninstall ? Set(String[]) : get!(selection, id, Set(String[]))

	to_add, to_del = String[], String[] # compute set difference in weidu order:
	for c in mod.components; k = c.id
		if k ∈ selected; k ∉ current && push!(to_add, k)
		else; k ∈ current && push!(to_del, k); end
	end
	isempty(to_add) && isempty(to_del) && (printlog("nothing to do"); return)
	if !isempty(to_add) # create symlink
		for file in ("$id.tp2", "setup-$id.tp2", "$id")
			target = joinpath(MODS, file); ispath(target) || continue
			link = joinpath(gamedir, file); ispath(link) && continue
			if simulate
				printsim("create $link -> $(relpath(link, target))")
			else
				symlink(relpath(target, gamedir), link)
			end
		end
	end
	cd(gamedir) do
		cmd = `weinstall $(mod.id) --language $(mod.game_lang) --skip-at-view --noautoupdate --no-exit-pause --force-install-list $to_add --force-uninstall-list $to_del`
# 		id == "eet" && (cmd = `weinstall eet --skip-at-view --noautoupdate --no-exit-pause --force-install-list 0 $(gamedirs.bg1)`)
		printsim(cmd)
		simulate && return
		run(ignorestatus(cmd))
	end
	now_installed = get(weidu_installed(gamedir), id, String[])
	not_installed = setdiff(selected, now_installed)
	if !isempty(not_installed)
		printwarn("warning: the following components were not installed:")
		for k in not_installed
			c = findcomponent(mod, k)
			printwarn(isnothing(c) ? "$k (not found)" : "$k ($(description(c)))")
		end
		printask("update selection?")
		r = lowercase(get(readline(), 1, 'n'))
		r == 'y' && (selection[id] = Set(now_installed); update_selection())
	end
end
function uninstall(mod; selection = global_selection, kwargs...)
	delete!(selection, mod.id)
	install(mod)
	update_selection()
end
# Global routines««1
function do_all(f; class=nothing, pause=false, limit=typemax(Int),
		selected = (f == status ? nothing : true),
		selection = global_selection, moddb=global_moddb,
		kwargs...)
	i = 0; c = ""
	l = installorder(f == status ? keys(moddb) :
		[id for (id,l) in selection if !isempty(l)]; moddb)
	for id in l
		mod = findmod(id; moddb)
		pause && (printlog("-- paused before: $f $id--"); readline())
		i+= 1; (i > limit) && break
		# do nothing if this is the wrong mod class:
		mod.class ≠ class ≠ nothing && continue
		# do nothing if we process only `selected` mods and this one is not
		# selected:
		# FIXME: we should act if mod is selected OR installed
		s = !isempty(get(selection, mod.id, Int[]))
		selected ∈ (s, nothing) || continue 
		if f == status
			(mod.class ≠ c) && (c = mod.class; println("\e[1m$c\e[m"))
		else
			printlog("\e[1m($n/$(length(global_selection)) $id)\e[m")
		end
		f(mod; selected, kwargs...)
	end
end
"returns the first n mods (in install order) with non-installed components"
function nextmods(n; selection=global_selection,
		installed = weidu_installed(GAMEDIR...),
		order = installorder(keys(filter(!isempty, global_selection))))
	ret = []
	for k in order
		isempty(symdiff(selection[k], get(installed, k, []))) && continue
		push!(ret, k)
		length(ret) ≥ n && break
	end
	ret
end
function do_first(f, n=1; moddb=global_moddb, selection=global_selection,
		kwargs...)
	order = installorder(keys(filter(!isempty, selection)))
	for id in nextmods(n; selection, order)
		while true
			printask("call function $f($id) ? (ynqrc)")
			r = lowercase(get(readline(), 1, 'n'))
			r == 'r' && (readme(moddb[id]); continue)
			r == 'c' && (edit(moddb[id]); continue) 
			r == 'q' && return
			r == 'y' && f(moddb[id]; order, kwargs...)
			break
		end
	end
end
list1 = (:download, :extract, :install, :uninstall, :status, :update)
for f in list1; @eval begin
	@inline $f(; kwargs...) = do_all($f; kwargs...)
	@inline $f(n::Integer; kwargs...) = do_first($f, n; kwargs...)
end end
# interactive commands are allowed only *one* mod (but can be a symbol)
list2 = (list1..., :readme, :tp2, :components, :status, :edit)
for f in list2; @eval begin
	@inline $f(id::Union{String,Symbol}; kwargs...) = $f(findmod(id); kwargs...)
end end

# ««1
end

M=ModTool
R=isdefined(M, :global_moddb) ? M.global_moddb["rr"] : nothing
C=isnothing(R) ? R : R.components
# vim: fdm=syntax fdl=1:
