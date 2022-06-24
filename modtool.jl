"""
ModTool - BG2 modding tool

Usage:

ModTool.import_bws_moddb()
ModTool.status(all)
ModTool.components!(:stratagems)
ModTool.update_selection()
ModTool.install(all)
"""
module ModTool
# Generic installation order
# (https://forums.beamdog.com/discussion/34882/list-of-bg2ee-compatible-mods)
# FIXME: readme metweaks
# + fix updating selection file
# + allow # on right side of =
# + mod subcomponents
# + better detection of mod components
# + include a “date” field
# - add a "release" field for github mods
# - add a new database for computed properties (readme/release/etc.)
# - use WeiDU's --list-* options
#  + list-languages
#  + list-components
#  + list-readme [MODDED]
#  + list-actions [MODDED]
# + remove useless mod archive field
#   (but preserve on importation from bws)
# + import_moddb should try to guess the date if possible
#  (or at least extract() for already-extracted mods should do it)
# + `uninstall`
#  + read only the BG2 install order file
#  + remove `games` field
#  + auto determine `PreEET` category
# + include tp2data in `Mod`
# - do a complete EET installation routine
# + add the following characteristics at a component-level:
#  + before/after: declare install order
#  - depends/conflicts: declare relations
# - complete dependency & conflict checking
# - try and guess readme for individual components (IMPOSSIBLE)
# + organize (some) components in a tree-like structure and edit choices
#  - review sorting: (install-order × weidu-order))
# - separate mod db generation
#  - write a `init()` function
# - don't write empty components in moddb
using Printf
using Dates
using TOML
using JSON
using DataStructures
using HTTP
using LibGit2
# using REPL.TerminalMenus # whiptail is better
# TODO: write a tree-menu

const HOME=ENV["HOME"]
const PREF_LANG=(r"fran.*ais"i, r"french"i, r"english"i, r"american"i)
const PREFIX="$HOME/jeux/ciopfs/modtool"
const MODDB="$PREFIX/moddb.toml"
const MODDB_SEPARATOR="\t"
const SELECTION="$PREFIX/selection"
const DOWN="$PREFIX/down" # should be in a ciopfs
const MODS="$PREFIX/mods" # should be in a ciopfs
const TEMP="$PREFIX/temp" # should be in a ciopfs
const INI="$PREFIX/ini" # should be in a ciopfs

const GAMEDIR =
	(bg1="$HOME/jeux/ciopfs/bg1/game", bg2="$HOME/jeux/ciopfs/bg2/game")
const GAME_LIST = keys(GAMEDIR)
	
const BWS_ROOT="$HOME/jeux/ciopfs/EE-Mod-Setup-Master"
const BWS_MODDB="$BWS_ROOT/App/Config/BG2EE/Mod.ini"
# const BWS_BG1IO="$BWS_ROOT/App/Config/BG1EE/InstallOrder.ini"
const BWS_BG2IO="$BWS_ROOT/App/Config/BG2EE/InstallOrder.ini"
const BWS_MODDB="$BWS_ROOT/App/Config/BG2EE/Mod.ini"
const BWS_SELECT="$BWS_ROOT/App/Config/User.ini"

function __init__()# setup global variables and filesystem ««
	global global_moddb = read_moddb(MODDB)
	printlog("read $(length(global_moddb)) mods in global database")
# 	global global_weidu_log = map(x->joinpath(x, "weidu.log"), GAMEDIR)
	global global_selection = read_selection(SELECTION)
	for d in (DOWN, MODS, TEMP) ispath(d) || mkdir(d); end
end#»»

@inline printlog(s...) = println(s...)
@inline printsim(s...) = println("\e[35;1m", s..., "\e[m")
@inline printerr(s...) = println("\e[31;1m", s..., "\e[m")
@inline printwarn(s...) = println("\e[33;1m", s..., "\e[m")

# Data structures ««1

mutable struct ModComponent#««
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
	@inline ModComponent(i, d::AbstractDict) =
		new(i, (get(d, k,"") for k in ("name", "group", "subgroup"))...,
		 (get(d,k,[]) for k in ("after", "before", "conflicts", "depends", "exclusive", "path"))...)
end#»»
@inline db_prop(c::ModComponent) = Dict(string(s) => getfield(c,  s)
	for s in (:after, :before, :conflicts, :depends, :exclusive, :path)
	if !isempty(getfield(c, s)))
@inline description(c::ModComponent) =
	isempty(c.subgroup) ? c.name : c.subgroup*'/'*c.name
mutable struct Mod#««
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
	sel_lang::Int # starts at zero (WeiDU indexing)
	components::Vector{ModComponent} # sorted as in tp2 file

	@inline Mod(;id, url, description, class, archive="", lastupdate=1970,
		tp2 = "", languages = String[], readme="", components=[]) = begin
		new(lowercase(id), url, description, archive, class, Date(lastupdate),
			readme, tp2, languages, 0,
			[ ModComponent(k,v) for (k,v) in pairs(components)])
		end
end#»»
# Mod classes for install order#««
# (https://forums.beamdog.com/discussion/34882/list-of-bg2ee-compatible-mods)
# (this is somewhat different from EE Mod Setup's ordering)
const MOD_CLASSES = split(replace("""
	DlcMerger
	PreEET
	EET
	Initial
	Early
	Fixes
	GUI
	BigQuests
	Quests
	SmallQuests
	NPCs
	SmallNPCs
	NPCTweaks
	Spells
	Items
	Kits
	Tweaks # merges “tactical”, “rules” and “AI”
	Late
	Final
	BADCLASS
""", r"#[^\n]*\n" =>"\n"))# »»
@inline modclass(class::Integer) = class
@inline modclass(class::AbstractString) =
	let k = (findfirst(==(class), MOD_CLASSES))
	isnothing(k) && error("bad mod class \"$class\"")
	k; end
@inline modarchive(m::Mod) = m.id*match(r"\.[^.]*$", m.url).match
const EET_class = modclass("EET")
@inline modgame(m::Mod) =
	m.id ∈ global_moddb["eet"].components[1].after ? :bg1 : :bg2
@inline modcomponents(m::Mod) = (m.components)

"Returns the dependency matrix for these mods, encoded as
(arrowsfrom = dict(mod1 => mod2, ...), arrowsto = dict(mod1 => n1, ...))
(only the *number* of incoming arrows is needed for topological sorting)"
function dependencies(list ;moddb=global_moddb)#««
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
end#»»
function comp_isless((id1, k1), (id2, k2), order)#««
	id1 == id2 && return isless(parse(Int, k1), parse(Int, k2))
	return isless(findfirst(==(id1), order), findfirst(==(id2), order))
end#»»
function sortkey1(id, moddb, dep)
	# returns (class(id), max class(before id), date(id))
	c0 = modclass(moddb[id].class)
	prev = get(dep.arrowsto, id, [])
	c1 = maximum(modclass(moddb[x].class) for x in prev; init = -1)
	(c0, c1, moddb[id].lastupdate)
end
@inline sortkey1(moddb, dep) = id -> sortkey1(id, moddb, dep)
"Returns a list of mod IDs in install order, given all dependencies"
function installorder(list; moddb=global_moddb)#««
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
end #»»
function check_conflicts(;selection = global_selection, moddb=global_moddb)#««
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
		println("tag \"$x\" is owned by the following components:")
		for (id, c) in v; println("  ", id, ":", c); end
	end
end#»»

# Mod DB handling ««1
@inline addmods!(moddb, mods...) = for m in mods; moddb[m.id] = m; end
function read_moddb(io::IO)#««
	dict = TOML.parse(io); return Dict(lowercase(id) =>
		Mod(id=lowercase(id),
			url=d["url"],
			description=d["description"],
			class=d["class"],
			archive=get(d, "archive", ""),
			lastupdate=get(d, "lastupdate", "1970"),
			readme=get(d, "readme", ""),
			components=get(d, "components", Dict()),
		) for (id, d) in dict)
end#»»
function write_moddb(io::IO; moddb=global_moddb)#««
	TOML.print(io, moddb) do m
		d = Dict("url" => m.url, "lastupdate" => m.lastupdate,
		"description" => m.description, "class" => m.class)
		isempty(m.archive) || (d["archive"] = m.archive)
		startswith(m.readme, "https://") && (d["readme"] = m.readme)
		isempty(m.components) ||
			(d["components"] = Dict(c.id=>db_prop(c) for c in m.components))
		d
	end
end#»»
@inline write_moddb(filename::AbstractString = MODDB; kwargs...) =
	open(filename, "w") do io write_moddb(io; kwargs...); end
@inline read_moddb(filename::AbstractString = MODDB; kwargs...) =
	open(filename, "r") do io read_moddb(io; kwargs...); end

@inline function findmod(id::Union{Symbol,String}; moddb = global_moddb)
	k = get(moddb, lowercase(string(id)), nothing)
	isnothing(k) || return k
# 	for mod in moddb; mod.id == lowercase(string(id)) && return mod; end
	error("mod '$id' not found")
end
@inline findmod(pattern::Regex; moddb = global_moddb) =
	[ id for id in keys(moddb) if occursin(pattern, id) ]
@inline function findcomponent(mod, k)
	k = lowercase(string(k)); for c in mod.components; c.id == k && return c; end
# 	error("mod '$(mod.id)': component '$k' not found")
end


# Pre-TP2 functions: download, extract, status ««1
function updateurl(mod::Mod)#««
	printlog("get latest release for $(mod.url)")
	repo = mod.url[8:end]
	req = HTTP.get("https://api.github.com/repos/$repo/releases/latest";
		status_exception=false)
# 	proc = run(pipeline(ignorestatus(`wget https://api.github.com/repos/$repo/releases/latest -O -`), stdout=fd))
	if req.status == 200
		for line in split(String(req.body), '\n')
			m = match(r"\"browser_download_url\"\s*:\s*\"([^\"]*\.(zip|rar|7z|tar\.gz)*)\"", line)
			m ≠ nothing && (mod.archive=basename(m.captures[1]);
			return m.captures[1])
			m = match(r"\"tarball_url\"\s*:\s*\"([^\"]*)\"", line)
			m ≠ nothing && (mod.archive=mod.id*".tar.gz"; return m.captures[1])
		end
	else
		return nothing
	end
end#»»
function download(mod::Mod; down=DOWN, mods=MODS, simulate=false)#««
	mod.url ∈ ("Manual", "") && return true # do nothing
	url = mod.url
	changed = false
	if startswith(mod.url, "github:")
		if isempty(mod.archive)
			url = updateurl(mod)
			changed = true
		elseif isfile(mod.archive)
			return true
		else
			url = "https://github.com/$(mod.url[8:end])/releases/download/latest/$(mod.archive)"
		end
	end
	if isnothing(url)
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
		changed = true
	else
		archive = joinpath(down, mod.archive)
		while !isfile(archive) || iszero(filesize(archive))
			printsim("download $url to $archive")
			simulate && return true
			req = HTTP.get(url; status_exception = false)
			req.status == 200 &&
				(open(archive, "w") do io; write(io, req.body); end; break)
			println("Failed to download $url\n(o)ther address, (a)bort, local (f)ile")
			action = readline()
			if action[1] == 'o' || startswith(action, r"https?://")
				changed = true
				url = replace(action, r"^o\s*"i => "")
			elseif action[1] == 'f'
				file = replace(action, r"^f\s*"i => "")
				ispath(file) && cp(file, archive)
			else
				return false
			end
		end
	end
	if changed
		mod.url = url
		printlog("mod url updated to new url or archive, rewrite mod db")
		write_moddb()
	end
	true
end#»»
function do_extract(mod::Mod; down=DOWN, mods=MODS, simulate=false)#««
	lowercase(mod.archive) == "manual" && return true
	download(mod; down=DOWN) || return false
	archive = joinpath(down, mod.archive)
	isdir(joinpath(mods, mod.id)) || try mktempdir(TEMP) do(dir); cd(dir) do
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
		end#»»
		write_moddb()
		# if this contains only one directory, move its contents to tmp dir««
		files = readdir()
		if length(files) == 1 && isdir(first(files))
			subdir = first(files)
			if subdir != String(mod.id)
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
	end end
	catch e
		if e isa SystemError; printerr(e)
		else; rethrow(e); end
	end
	lastupdate!(mod; mods) && write_moddb()
	return true
end#»»
function lastupdate!(mod::Mod; mods=MODS)
	# we keep the date of the newest .tp2, .tpa, .tph file:
	d = Date(unix2datetime(maximum(mtime(joinpath(root, f))
		for (root,_,files) in walkdir(joinpath(mods, mod.id))
		for f in files if endswith(f, r"\.tp[2ah]"i); init=0)))
	d == mod.lastupdate && return false
	mod.lastupdate = d
	printlog("$(mod.id): recomputed last update to $d")
	return true
end
function status(mod::Mod; selection=global_selection, selected=nothing)#««
	sel = !isempty(get(selection, mod.id, Int[]))
	selected ∈ (sel, nothing) || return
	mod.id ∈ ("eet", "stratagems") && print("\e[34m")
	@printf("%c%c%c %7s %-22s %s\n",
		isfile(joinpath(DOWN, mod.archive)) ? 'd' : '.',
		ispath(joinpath(MODS, mod.id)) ? 'x' : '.',
		sel ? 's' : '.',
		Dates.format(mod.lastupdate, "yyyy-mm"),
		mod.id, mod.description)
	print("\e[m")
end#»»
# TP2 data extraction ««1
function lang_score(langname, pref_lang=PREF_LANG)#««
	for (i, pref) in pairs(pref_lang)
		occursin(pref, langname) && return i
	end
	return typemax(Int)
end#»»
function extract(m)#««
	do_extract(m) || return false
	id = m.id
	if isempty(m.tp2)#««
		for path in ("$id.tp2", "$id/$id.tp2", "$id/setup-$id.tp2", "setup-$id.tp2",
			"setup-$id.exe", "$id/setup-$id.exe")
			ispath(joinpath(MODS, path)) && (m.tp2 = path; break)
		end
		isempty(m.tp2) && error("no TP2 file found")
	end#»»
	cd(MODS) do
	if isempty(m.languages)#««
		m.sel_lang = 0
		# m.languages
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-languages $(m.tp2)`)
			x = match(r"^(\d+):(.*)$", line); isnothing(x) && continue
			@assert parse(Int, x.captures[1]) == length(m.languages)
			push!(m.languages, x.captures[2])
		end
		isempty(m.languages) ||
			(m.sel_lang = argmin([lang_score(l, PREF_LANG) for l in m.languages]) - 1)
	end#»»
	if all(isempty, c.name for c in m.components)#««
		printlog("reading components for '$id'")
		prop = Dict(c.id => c for c in m.components) # store properties
		empty!(m.components) # we will store them in WeiDU-order
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-components-json $(m.tp2) $(m.sel_lang)`)
			startswith(line, "[{") || continue
			for x in JSON.parse(line)
				k = string(x["number"])
				c = get(prop, k, ModComponent(k, ""))
				c.name, c.group, c.subgroup =
					x["name"], get(x["group"], 1, ""), get(x,"subgroup","")
				push!(m.components, c)
			end
		end
		for k in keys(prop); isempty(k) && continue
			found = false
			for c in m.components; c.id == k && (found=true; break); end
			found || println("\e[31;1m '$(m.id):$k' not found\e[m")
		end
	end#»»
	if isempty(m.readme) # no hardcoded readme provided««
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-readme $(m.tp2) $(m.sel_lang)`)
			x = match(r"^R (.*)", line); isnothing(x) && continue
			isfile(x.captures[1]) && (m.readme = joinpath(MODS, x.captures[1]); break)
		end
	end#»»
	if isempty(m.readme) # try and guess...««
		readmes = [ joinpath(root, f)
			for (root, _, files) in walkdir(joinpath(MODS, id))
			for f in files if occursin(r"readme"i, f) ]
		!isempty(readmes) && 
			(m.readme = joinpath(MODS, id, 
				readmes[argmin([ lang_score(f, PREF_LANG) for f in readmes ])]))
	end#»»
	if isempty(m.readme) && startswith(m.url, "https://github.com")#««
		x = match(r"https://github.com/[^/]*/[^/]*", m.url)
		m.readme = x.match
	end#»»
	end # cd(mods)
	true
end#»»
function readme(mod::Mod)#««
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
end#»»
@inline tp2(mod::Mod) = extract(mod) && run(`view $(joinpath(MODS, mod.tp2))`)

# Selection files handling««1
function read_selection_line(line)#««
	r = r"^((\s*)([^=#][^=]*[^= ])(\s*=\s*)(|[^# ]|[^# ][^#]*[^# ]))?((\s*#.*)?)$"
	m = match(r, line)
	@assert m≠nothing && length(m.captures)>=5 "Invalid selection line: $line"
	return (lhs = m.captures[3], eq = m.captures[4], rhs = m.captures[5],
		indent = m.captures[2], comment = m.captures[6])
end#»»
function read_selection(filename=SELECTION)#««««
# 	selection = Tuple{Symbol,Vector{Int}}[]
	selection = Dict{String, Vector{String}}()
	for line in eachline(filename)
		cap = read_selection_line(line); isnothing(cap.lhs) && continue
		id = lowercase(cap.lhs)
		@assert !haskey(selection, id)
		selection[id] = split(cap.rhs)
	end
	return selection
end#»»»»
function write_selection(selection=global_selection, filename=SELECTION)#««
	nm = length(selection)
	nc = sum(length(c) for (_, c) in selection)
	printlog("writing selection ($nm mods, $nc components) to $filename")
	open(filename, "w") do io
		for (k, c) in selection; println(io, k, '=', join(c, ' ')); end
	end
end#»»
"tries to update the selection file while preserving comments etc."
function update_selection(selection=global_selection, filename=SELECTION)#««
	nm = length(selection)
	nc = sum(length(c) for (_, c) in selection)
	printlog("updating selection ($nm mods, $nc components) in $filename")
	todo = Set(keys(selection))
	text = ""
	nchanged = 0
	for line in eachline(filename)
		cap = read_selection_line(line)
		if !isnothing(cap.lhs)
			id, components = lowercase(cap.lhs), split(cap.rhs)
			s = get(selection, id, Int[])
			line = cap.indent*cap.lhs*cap.eq* join(s, ' ')*cap.comment
			Set(s) ≠ Set(components) && (nchanged+= 1)
			delete!(todo, id)
		end
		text*= line*'\n'
	end
	for id in todo; isempty(selection[id]) && continue
		text*= id*'='*join(selection[id], ' ')*'\n'
	end
	printlog("modified $nchanged lines, added $(length(todo)) lines")
	cp(filename, filename*'~'; force=true)
	open(filename, "w") do io; write(io, text); end
	run(ignorestatus(`diff --color=always $(filename*'~') $filename`))
end#»»
# Mod components ««1
function weidu_status(dirs...)#««
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
end#»»
function modstatus(tp2file, dir)#««
	weidu_log = joinpath(dir, "weidu.log")
	status = String[]
	ispath(weidu_log) || return status
	for line in eachline(weidu_log)
		line = replace(line, r"\s*//.*" => "")
		isempty(line) && continue
		(tp2, lang, comp, rest...) = split(line, ' ')
		tp2 = lowercase(tp2[2:prevind(tp2, length(tp2))])
		tp2 == tp2dir && push!(status, comp[2:end])
	end
	return status
end#»»
function printcomp(io::IO, m, c::ModComponent; selection, installed)#««
	@printf(io, "%c%c%c `%s:%s` %s\n",
		c.id ∈ get(selection, m.id, []) ? 's' : '.',
		c.id ∉ get(installed, m.id, []) ? '.' : modgame(m) == :bg1 ? '1' : '2',
		isempty(c.path) ? '.' : 'p',
		m.id, c.id, description(c))
end#»»
function readcomp(io::IO; selection=global_selection)#««
	newsel = Dict{String, Dict{String,Bool}}()
	for line in eachline(io)
		m = match(r"^([.s])[.12][.p]\s+`([^`:]*):(\d+)`\s+", line)
		isnothing(m) && continue
		get!(newsel, m.captures[2], Dict{String,Bool}())[m.captures[3]] =
			(m.captures[1] == "s")
	end
	global S=newsel
	to_add = Dict{String,Vector{String}}()
	to_del = Dict{String,Vector{String}}()
	for (id, d) in newsel
		old = get!(selection, id, String[])
		for (k, v) in d
			if v && (k ∉ old)
				push!(get!(to_add, id, String[]), k)
				push!(old, k)
			elseif !v && (k ∈ old)
				push!(get!(to_del, id, String[]), k)
				filter!(!=(k), old)
			end
		end
	end
	printlog("Added:")
	for (k, v) in to_add; printlog("\e[32m+", k, ": ", join(v, ", "), "\e[m");end
	printlog("Removed:")
	for (k, v) in to_del; printlog("\e[31m-", k, ": ", join(v, ", "), "\e[m");end
end#»»
function component_editor(filename; selection)#««
	run(`vim -c 'se ft= fdm=marker fmr=««,»»|
sy match modComp /\`[^\`]*\`/|hi link modComp Constant|
sy match modAdd /^s\.[.p] .*$/ contains=modComp |hi link modAdd DiffAdd|
sy match modDel /^\.[12][.p] .*$/ contains=modComp |hi link modDel DiffDelete|
sy match modSel /^s[12][.p] .*$/|hi link modSel DiffChange|
sy match modGrp /^# .*$/|hi link modGrp ModeMsg|
sy match modSub /^## .*$/|hi link modSub Title|
	nmap <buffer> <silent> <Space> :s/^s/¤/e<cr>:s/^\./s/e<cr>:s/^¤/./e<cr>'
	$filename`)
	open(filename, "r") do io; readcomp(io; selection); end
end#»»
function components!(mod::Mod; selection=global_selection,#««
		gamedir = GAMEDIR[modgame(mod)], edit=true)
	extract(mod) || return
	installed = weidu_status(gamedir)
	filename = joinpath(TEMP, "selection-"*mod.id*".txt")
	io = edit ? open(filename, "w") : stdout
	g = ""; h = ""
	for c in modcomponents(mod)
		g ≠ c.group && (g = c.group; println(io, "# ", g, "«"*"«1"))
		h ≠ c.subgroup &&
			(h = c.subgroup; println(io,"## ",h, isempty(h) ? "»"*"»2" : "«"*"«2"))
		printcomp(io, mod, c; selection, installed)
	end
	edit && (close(io); component_editor(filename; selection))
end#»»
@inline components(mod::Mod; kwargs...)= components!(mod; edit=false, kwargs...)
struct ComponentTree#««
	alternatives::Vector{NTuple{2,String}}
	children::Dict{String,ComponentTree}
	@inline ComponentTree() = new([], Dict())
end#»»
findbranch(root::ComponentTree, path) = isempty(path) ? root :
	findbranch(get!(root.children, first(path), ComponentTree()), path[2:end])
function build_tree(;moddb=global_moddb)#««
	root = ComponentTree()
	for (id, m) in moddb, c in modcomponents(m)
		isempty(c.path) && continue
		push!(findbranch(root, c.path).alternatives, (id, c.id))
	end
	root
end#»»
function display_tree(io::IO, root, level=1;#««
		moddb=global_moddb, selection=global_selection, installed, order)
	for (id, k) in sort(collect(root.alternatives);
			lt=(x,y)->comp_isless(x,y,order))
		m = findmod(id; moddb); extract(m); c = findcomponent(m, k)
		isnothing(c) && (printerr("'$id:$k' not found"); continue)
		printcomp(io, m, c; selection, installed)
	end
	for (s, c) in sort(collect(root.children); by=first)
		println(io, '#'^level, ' ', s, " «",'«', level)
		display_tree(io, c, level+1; selection, moddb, installed, order)
	end
end#»»
function components!(;selection=global_selection,moddb=global_moddb)#««
	installed = weidu_status(GAMEDIR...)
	order = installorder(keys(moddb); moddb)
	f = joinpath(TEMP, "config")
	open(f, "w") do io
		display_tree(io, build_tree(;moddb); selection, moddb, installed, order)
		println(io, "# Individual, unsorted components «"*"«1")
		lastid = ""
		for id in order
			m = moddb[id]
			g = ""; h = ""
			!isempty(get(selection, id, [])) && extract(m)
			for c in modcomponents(m)
				isempty(c.path) || continue
				id ≠ lastid && (extract(m); lastid=id; println(io, "## $id «"*"«2"))
				g ≠ c.group && (g = c.group; println(io, "### ", g, "«"*"«3"))
				h ≠ c.subgroup &&
					(h=c.subgroup; println(io,"#### ",h,isempty(h) ? "»"*"»4" : "«"*"«4"))
				printcomp(io, m, c; selection, installed)
			end
		end
	end
	component_editor(f; selection)
end#»»

# Mod installation ««1
function install(mod; simulate=false, uninstall=false, #««««
		selection=global_selection, gamedirs=GAMEDIR)
	extract(mod) || return
	id = mod.id

	gamedir = gamedirs[modgame(mod)]
	current = get(weidu_status(gamedir), mod.id, String[])
	order = installorder(keys(selection))
	installed = weidu_status(gamedirs...)
	warned = false
	for k in order; k == id && break
		isempty(symdiff(get(installed, k, []), get(selection, k, []))) && continue
		println("$k: $(get(installed, k, [])) => $(get(selection, k, []))")
		printwarn("warning: mod '$k' should probably be installed before '$id'")
		warned = true
	end
	if warned
		printwarn("proceed anyway? (yn)"); r = readline()
		lowercase(first(r)) == 'y' || return
	end
	selected = uninstall ? Int[] : get!(selection, id, Int[])

	to_add = string.(setdiff(selected, current))
	to_del = string.(setdiff(current, selected))
	isempty(to_add) && isempty(to_del) && (printlog("nothing to do"); return)
	if !uninstall # create symlink««
		for file in ("$id.tp2", "setup-$id.tp2", "$id")
			target = joinpath(MODS, file); ispath(target) || continue
			link = joinpath(gamedir, file); ispath(link) && continue
			if simulate
				printsim("create $link -> $(relpath(link, target))")
			else
				symlink(relpath(target, gamedir), link)
			end
		end
	end#»»
	cd(gamedir) do#««
		cmd = `weinstall $(mod.id) --language $(mod.sel_lang) --skip-at-view --noautoupdate --no-exit-pause --force-install-list $to_add --force-uninstall-list $to_del`
		printsim(cmd)
		simulate && return
		run(cmd)
	end#»»
end#»»»»
function uninstall(mod; selection = global_selection, kwargs...)
	delete!(selection, mod.id)
	install(mod)
	update_selection()
end
# Global routines««1
function do_all(f; class=nothing, pause=false, limit=typemax(Int),#««
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
end#»»
"returns the first mod (in install order) with non-installed component"
function nextmod(; selection=global_selection, moddb=global_moddb,#««
		installed = weidu_status(GAMEDIR...))
	order = installorder(keys(filter(!isempty, global_selection)))
	k = findfirst(x->!isempty(symdiff(selection[x], get(installed, x, []))),
		order)
	isnothing(k) ? k : order[k]
end#»»
function do_first(f; moddb=global_moddb, kwargs...)#««
	id = nextmod(; moddb)
	println("call function $f($id) ? (yn)"); r = readline()
	lowercase(r[1]) == 'y' || return
	f(moddb[id]; kwargs...)
end#»»
list1 = (:download, :extract, :install, :uninstall, :show_components, :status)
for f in list1; @eval begin
	@inline $f(; kwargs...) = do_all($f; kwargs...)
	@inline $f(::typeof(first); kwargs...) = do_first($f; kwargs...)
end end
# interactive commands are allowed only *one* mod (but can be a symbol)
list2 = (list1..., :readme, :tp2, :components!, :components, :status)
for f in list2; @eval begin
	@inline $f(id::Union{String,Symbol}; kwargs...) = $f(findmod(id); kwargs...)
end end

# BWS mod db handling ««1
function bws_moddb(source = BWS_MODDB, orderfile=BWS_BG2IO)#««
	moddb = Dict{String,Mod}()
	dict = Dict{String,String}()
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
	for line in eachline(orderfile)
		v = split(line, ";")
		first(v) ∈ ("MUC", "STD", "SUB") || continue
		id = lowercase(v[2]); m = get(moddb, id, nothing)
		isnothing(m) && continue
		m.class = ["Initial", "Fixes", "BigQuests", "Quests", "SmallQuests",
			"NPCs", "SmallNPCs", "NPCTweaks", "Tweaks", "Tweaks", "Items",
			"Tweaks", "Kits", "GUI"][1+parse(Int, v[4])]
	end
	moddb
end#»»
function mod_exclusives(mod::Mod; except = String[])#««
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
end#»»
function import_bws_moddb((source, dest) = BWS_MODDB => MODDB,#««
		orderfile = BWS_BG2IO)
	printlog("reading BWS mod database")
	moddb = bws_moddb(source, orderfile)
	printlog("adding new mods")
	function mkmod(id, url, description, class, archive = "")#««
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
		return Mod(;id, url, description, class, archive)
	end#»»
	addmods!(moddb,
	# Argent77 ««3
	mkmod("a7-convenienteenpcs", "github:Argent77/A7-NoEENPCs",
		"Convenient EE NPCs", "NPCTweaks"),
	mkmod("extraexpandedencounters", "https://forums.beamdog.com/uploads/editor/ta/auj7pwad39pd.zip", "Extra Expanded Encounters", "Quests"),
	# ArtemiusI ««3
	mkmod("housetweaks", "github:ArtemiusI/House-Rule-Tweaks",
		"House Rule Tweaks", "Tweaks"),
	# Spellhold Studios ««3
	mkmod("saradas_magic", "http://www.mediafire.com/download/rg5cyypji1om22o/Saradas%2520Magic%2520ENG%2520V_1.1.zip", "Saradas Magic", "Items"),
	mkmod("saradas_magic_2", "github:SpellholdStudios/Saradas_Magic_for_BG2", "Saradas BG2", "Spells"),
	mkmod("beyond_the_law", "github:SpellholdStudios/Beyond_the_Law", "Beyond the Law", "NPCs"),
	mkmod("kiara-zaiya", "github:SpellholdStudios/Kiara_Zaiya", "Kiara Zaiya NPCs", "NPCs"),
	mkmod("iylos", "github:SpellholdStudios/Iylos", "Iylos (ToB monk)", "NPCs"),
	mkmod("firkraag", "github:SpellholdStudios/Super_Firkraag", "Super Firkraag", "Quests"),
	mkmod("ruad", "github:SpellholdStudios/RuadRofhessaItemUpgrade", "Ruad Ro'fhessa Item Upgrade", "Items"),
	mkmod("bolsa", "github:SpellholdStudios/Bolsa", "Bolsa (merchant)", "Items"),
# 	next 2 mods are broken in EE games:
# 	mkmod("RPG-KP", "github:SpellholdStudios/RPG_Dungeon_Kit_Pack",
# 		"RPG Dungeon kit pack", "Kits"),
# 	mkmod("RItemPack", "github:SpellholdStudios/RPG_Dungeon_Item_Pack",
# 		"RPG Dungeon item pack", "Items"),
	# Gibberlings3 ««3
	mkmod("wheels", "github:Gibberlings3/WheelsOfProphecy", "Wheels of Prophecy", "Quests"),
	mkmod("forgotten-armament", "github:Gibberlings3/Forgotten-Armament", "Forgotten Armament", "Items"),
	mkmod("skills-and-abilities", "github:Gibberlings3/Skills-and-Abilities", "Skills and Abilities", "Kits"),
	mkmod("c#sodboabri", "github:Gibberlings3/The_Boareskyr_Bridge_Scene", "The Boareskyr Bridge Scene", "Quests"),
	mkmod("sodrtd", "github:Gibberlings3/Road_To_Discovery_for_SoD", "Road to Discovery", "Quests"),
	mkmod("c#endlessbg1", "github:Gibberlings3/EndlessBG1", "Endless BG1", "Quests"),
	mkmod("dw_lanthorn", "github:Gibberlings3/Restored-Rhynn-Lanthorn", "Restored Rhynn Lanthorn quest", "Quests"),
	mkmod("druidsor", "github:Gibberlings3/Geomantic_Sorcerer", "Geomantic Sorcerer", "Kits"),
	mkmod("valhorn", "github:Gibberlings3/Improved_Horns_of_Valhalla", "Improved Horn of Valhalla", "Items"),
	mkmod("transitions", "github:Gibberlings3/transitions", "Transitions", "Fixes"),
	# Gitjas ««3
	mkmod("c#brandock", "github:Gitjas/Brandock_the_Mage", "Brandock the Mage", "NPCs"),
	mkmod("c#solaufein", "github:Gitjas/Solaufeins_Rescue_NPC", "Solaufein's Rescue", "NPCs"),
	mkmod("c#brage", "github:Gitjas/Brages_Redemption", "Brage's Redemption", "NPCs"),
	# Github misc. ««3
	mkmod("d0tweak", "github:Pocket-Plane-Group/D0Tweak", "Ding0 Tweak Pack", "Tweaks"),
	mkmod("rttitempack", "github:GwendolyneFreddy/ReturnToTrademeet_ItemPack",  "Return to Trademeet item pack", "Items"),
	mkmod("nanstein", "github:GwendolyneFreddy/Nanstein", "Nanstein item upgrade", "Items"),
	mkmod("iwditempack", "github:GwendolyneFreddy/IWD_Item_Pack", "IWD item pack", "Items"),
	mkmod("aurora", "github:Sampsca/Auroras-Shoes-and-Boots", "Aurora's shoes and boots", "Items"),
	mkmod("monasticorders", "github:aquadrizzt/MonasticOrders", "Monastic Orders", "Kits"),
	mkmod("deitiesoffaerun", "github:Raduziel/Deities-Of-Faerun", "Deities of Faerun", "Kits"),
	mkmod("tnt", "github:BGforgeNet/bg2-tweaks-and-tricks", "Tweaks and Tricks", "Tweaks"),
	mkmod("mih_sp", "github:AngelGryph/MadeInHeaven_SpellPack", "Made In Heaven: Spell Pack", "Spells"),
	mkmod("mih_eq", "github:AngelGryph/MadeInHeaven_EncountersAndQuests", "Made In Heaven: Encounters and Quests", "Quests"),
	# Weasel mods ««3
	mkmod("thevanishingofskiesilvershield", "weaselmods:the-vanishing-of-skie-silvershield", "The vanishing of Skie Silvershield", "NPCs"),
	mkmod("bristlelick", "weaselmods:bristlelick", "Bristlelick (gnoll NPC)", "NPCs"),
	mkmod("walahnan", "weaselmods:walahnan", "Walahnan (gnome chronomancer)", "NPCs"),
	mkmod("ofheirloomsandclasses", "weaselmods:of-heirlooms-and-classes", "Of Heirlooms and Classes", "Items"),
	mkmod("faldornbg2", "weaselmods:faldorn-bg2ee", "Faldorn BG2", "NPCs"),
	mkmod("khalidbg2", "weaselmods:khalid-bg2", "Khalid BG2", "NPCs"),
	mkmod("gahesh", "weaselmods:gahesh", "Gahesh (LG half-orc sorcerer)", "NPCs"),
	# Misc. ««3
	mkmod("mortis", "http://download1648.mediafire.com/7plvylpx6xbg/lspfz2ctae51735/Mortis+Mini+Mod+2.33.zip", "Mortis Mini Mod", "Items"),
	mkmod("unique_items", "https://forums.beamdog.com/uploads/editor/az/70fwogntemm8.zip", "BGEE/SOD Item replacement fun pack", "Items"),
	mkmod("arcanearcher", "www.shsforums.net/files/download/994-arcane-archer/", "Arcane Archer", "Kits"),
	#»»3
	)
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
	"nathaniel" => "http://dl.spellholdstudios.net/nathaniel",
	"faren" => "http://dl.spellholdstudios.net/faren",
	"npckit" => "https://www.gibberlings3.net/files/file/793-npc-kitpack",
	"sagaman" => "http://www.blackwyrmlair.net/~rabain/SagaMaster/Ulrien-SagaMaster.zip",
	); moddb[k].url = v; end
	#»»
	# Tweak mod classes ««
	printlog("modifying mod classes")
	delete!(moddb, "weidu")
	delete!(moddb, "weidu64")
	moddb["dlcmerger"].class="DlcMerger"
	moddb["ascension"].class="BigQuests"
	moddb["azengaard"].class="Quests"
	moddb["bg1aerie"].class="SmallNPCs"
	moddb["bg2eetrans"].class="Initial"
	moddb["butchery"].class="Quests"
	moddb["bwfixpack"].class="Fixes"
	moddb["d0questpack"].class="Quests"
	moddb["eet_end"].class="Final"
	moddb["eetact2"].class="Quests"
	moddb["faiths_and_powers"].class="Kits"
	moddb["fnp_multiclass"].class="Tweaks"
	moddb["hammers"].class="Items"
	moddb["imnesvale"].class="Quests"
	moddb["impasylum"].class="Quests"
	moddb["item_rev"].class="Items"
	moddb["paintbg"].class="GUI"
	moddb["rr"].class="Tweaks"
	moddb["spell_rev"].class="Spells"
	moddb["stratagems"].class="Final"
	moddb["tdd"].class="BigQuests"
	moddb["the_horde"].class="Quests"
	moddb["turnabout"].class="Quests"
	moddb["vienxay"].class="NPCs"
	# »»
		# Tweak mod readme««
	printlog("hardcoding mod readme")
	moddb["faiths_and_powers"].readme = "https://www.gibberlings3.net/forums/topic/30792-unearthed-arcana-presents-faiths-powers-gods-of-the-realms/"
	moddb["tnt"].readme = "https://github.com/BGforgeNet/bg2-tweaks-and-tricks/tree/master/docs"
	moddb["epicthieving"].readme = "https://forums.beamdog.com/discussion/74158/v3-5-epic-thieving-more-benefits-from-high-thieving-skills"
	moddb["3ed"].readme = "https://github.com/Holic75/Baldurs-gate-dnd-3.5#readme"
	moddb["wildmage"].readme = "https://github.com/BGforgeNet/bg2-wildmage"
	moddb["artisanskitpack"].readme = "https://artisans-corner.com/the-artisans-kitpack/"
	moddb["monasticorders"].readme = "https://forums.beamdog.com/discussion/18620/mod-beta-monastic-orders-of-faerun"
	moddb["mercenary"].readme = "https://forums.beamdog.com/discussion/68151/fighter-kit-mercenary-v3-1-iwdee-eet-bgee-bg2ee"
	moddb["karatur"].readme = "https://forums.beamdog.com/discussion/15959/mod-twas-a-slow-boat-from-kara-tur-queststorenew-items-release-90/p1"
	moddb["verrsza"].readme = "https://forums.beamdog.com/discussion/60614/mod-verrsza-npc-for-bgee-and-sod"
	#»»
	moddb["arcanearcher"].archive = "arcanearcher.zip"
	printlog("setting mod component properties")
	function setmod!(id, list...)#««
		m = findmod(id; moddb)
		for (i, kv) in list; i = string(i)
			j = findfirst(c->c.id == i, m.components)
			isnothing(j) &&
				(push!(m.components, ModComponent(i, "")); j=length(m.components))
			c = m.components[j]
			for (k, v) in pairs(kv)
				k == :path && !isempty(c.path) && printlog("warning, '$id:$i'.path is not empty")
				k != :path && (v = unique!(sort!([v;])))
				push!(getfield(c, Symbol(k)), v...)
			end
		end
	end#»»
	setmod!("a7#improvedarcher",#««
		0 => (path=["Classes", "Ranger"],),
		10 => (path=["Classes", "Fighter"],),
		20 => (path=["Classes", "Fighter"],),
		30 => (path=["Classes", "Fighter"],),
		100 => (path=["Items", "Ammunition"],),
	)#»»
	setmod!("animalcompanions",#««
		0 => (path=["Classes", "Ranger"],),
	)#»»
	setmod!("artisanskitpack",#««
		"" => (after=["Emily"],),
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
	)#»»
	setmod!("atweaks",#««
		"" => (after = ["rr", "stratagems"],),
		100 => (path = ["Skills", "Infravision"],),
		101 => (path = ["Creatures"],),
		102 => (exclusive="shammr.itm", path = ["Items", "Summoned weapons"],),
# 		301 => (exclusive = "npchan.itm",), # Corthala armor
# 		302 => (exclusive = "wa2robe.itm",), # Vecna robe
# 		500 => (exclusive = "container_capacity",),
# 		502 => (exclusive = "container_capacity",),
# 		 101 => (exclusive = ["fl#idim3.eff"],)
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
	)#»»
	setmod!("bg2eetrans",#««
		"" => (after=["eet"],),
	)#»»
	setmod!("cdtweaks",#««
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
		1075 => (path = ["NPC"],),
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
	)#»»
	setmod!("cdlore",#««
		10 => (path=["Skills", "Lore"],),
		20 => (path=["Skills", "Lore"],),
		30 => (path=["Skills", "Lore"],),
		40 => (path=["Skills", "Lore"],),
	)#»»
	setmod!("charlatan",#««
		0 => (path=["Classes", "Bard"],),
		1 => (path=["NPC", "Eldoth"],),
	)#»»
	setmod!("cowledmenace",#««
		"" => (after = "eet", depends = "eet",),
	)#»»
	setmod!("d0questpack", #««
		"" => (after = ["kelsey", "keto", "ub" ],),
		0 => (exclusive = "AI",),
		5 => (exclusive = ["c6kach.cre", "ar0300.bcs", "ar0300.are", "aran.cre", "maevar.cre", "mvguard1.cre", "mvpries.cre", "aran02.cre", "arnfgt03.cre", "arnfgt04.cre", "renal.cre", "thief1.cre", "tassa.cre", "c6tanov.bcs", "vvtanov.cre",], path=["Story", "BG2"],),
   6 => (exclusive = ["sw1h50.itm"], path=["Story", "BG2"],),
		8 => (exclusive = ["ar0812.are", "ar1515.bcs", "thumb.cre", ], path=["Story", "BG2"],),
		9 => (exclusive = ["potn33.itm", "potn38.itm", "spwi106.spl", "spwi815.spl",],path=["Story", "BG2"],),
  10 => (exclusive = ["hellself.eff", "spin755.spl", "spin751.spl", "sphl004.spl", "spin753.spl", "hellself.cre", "sphl003.spl", "sphl005.spl", "hellgen2.cre", "sphl001.spl", "sphl002.spl", "cutc7g.bcs", "spin749.spl", "spin747.spl"], path=["Areas", "BG2", "Ending"],),
  11 => (exclusive = ["amtarc01.cre", "amtcap01.cre", "amtcle01.cre", "amtgen01.cre", "amtmag01.cre", "amtpik01.cre"], path=["Areas", "BG2", "Oasis"],),
		12 => (exclusive = ["Oasis", "ar6300.are", ], path=["Areas", "BG2", "Oasis"],),
		13 => (exclusive = ["ppsanik.cre", "pirmur02.cre", "pirmur05.cre",], path=["Story", "BG2"],),
  15 => (exclusive = ["suinvis.cre", "sumound.cre", "sudryad.cre", "sufake.itm", "sutear.itm", "sutear.bam", "sufake.bam"], path=["Quests", "BG2"],),
  16 => (exclusive = ["besamen.cre", "baisera.cre"], path=["Quests", "BG2"],),
		17 => (exclusive = ["kamir.cre",], path=["Quests", "BG2"],),
		19 => (exclusive = ["ar2402.are",], path=["Quests", "BG2"],),
  20 => (exclusive = ["spin671.spl"], path=["Quests", "BG2"],),
		21 => (exclusive = ["ar0530.are", "ar0530.bcs",], path=["Quests", "BG2"],),
 401 => (exclusive = ["sukiss1.cre", "sukissk.wav", "sumist.cre", "suspyim.cre", "reddeath.bcs",],),
	)#»»
	setmod!("d0tweak",#««
		11 => (after = ["rr",], path=["Cosmetic", "Sprites"],), # ioun stones...
		17 => (path = ["Skills", "Lore"],),
		18 => (path = ["Skills", "Backstab"],),
	)#»»
	setmod!("d5_random_tweaks", #««
		"" => (after = ["spell_rev", "item_rev"],),
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
	) #»»
	setmod!("deitiesoffaerun",#««
		"" => (conflicts=["faiths_and_powers", "spell_rev",],
			after=["eet"], before=["eet_end"],),
	)#»»
	setmod!("divine_remix",#««
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
	)#»»
	setmod!("eet", "" => (after = [#««
	# https://k4thos.github.io/EET-Compatibility-List/EET-Compatibility-List.html
	# better: EET/tbl/compatibility.tbl
	"dlcmerger", "bgeetextpack", "sodrus", "bg1aerie", "bg1npc", "bg1npcmusic", "bg1ub", "darkhorizonsbgee", "drake", "drizztsaga", "garrick-tt", "k9roughworld", "saradas_magic", "k9sharteelnpc", "sirene", "tenyathermidor", "soa", "karatur", "verrsza", "white", "bgsodde",
# "margarita",
	], before = ["bg2eetrans", "bg2ee_ga", "bg2eer"],),)
	setmod!("dlcmerger", "" => (before = filter(≠("dlcmerger"),
		moddb["eet"].components[1].after),))
	#»»
	setmod!("epicthieving",#««
		0 => (path = ["Skills", "Thieving"],),
		100 => (path = ["Skills", "Thieving"],),
		200 => (path = ["Skills", "Thieving"],),
		300 => (path = ["Skills", "Thieving"],),
		400 => (path = ["Skills", "Thieving"],),
		500 => (path = ["Items", "Potions"],),
		600 => (path = ["Skills", "Thieving"],),
	)#»»
	setmod!("faiths_and_powers",#««
		"" => (after = ["divine_remix", "item_rev", "iwdification", "monasticorders", "spell_rev", "tomeandblood" ],
			before = ["fnp_multiclass", "might_and_guile", "scales_of_balance", "stratagems", "cdtweaks"],),
			21 => (path = ["Spells", "Sphere system"],),
			22 => (path = ["Spells", "Sphere system"],),
			23 => (path = ["Spells", "Sphere system"],),
			24 => (path = ["Spells", "Sphere system"],),
			25 => (path = ["Spells", "Sphere system"],),
			31 => (path = ["Classes", "Cleric"],),
			33 => (path = ["Classes", "Druid"],),
			35 => (path = ["Classes", "Paladin"],),
			37 => (path = ["Classes", "Ranger"],),
			75 => (path = ["Classes", "Cleric"],),
			80 => (path = ["Spells", "Sphere system"],),
			85 => (path = ["NPC"],),
	)#»»
	setmod!("fnp_multiclass",#««
		"" => (after = ["fnp", "divine_remix", "deitiesoffaerun", "item_rev", "iwdification", "monasticorders", "spell_rev", "tomeandblood" ],
			before = ["scales_of_balance", "stratagems", "cdtweaks"],),
	)#»»
	setmod!("eet_tweaks",#««
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
	)#»»
	setmod!("eetact2",#««
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
	)#»»
	setmod!("item_rev",#««
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
	)#»»
	setmod!("iwdification",#««
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
		120 => (path=["Skills", "Evaion"],),
		30 => (path=["Spells", "New spells"],),
		40 => (path=["Spells", "New spells"],),
		80 => (path=["Cosmetic", "Icons"],),
	)#»»
	setmod!("keto", "" => (after = ["kelsey"],),)
	setmod!("klatu",#««
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
	)#»»
	setmod!("mercenary",#««
		0 => (path=["Classes", "Fighter"],),
		1 => (path=["NPC", "Kagain"],),
		1 => (path=["NPC", "Korgan"],),
	)#»»
	setmod!("metweaks",#««
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
	)#»»
	setmod!("might_and_guile",#««
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
	),#»»
	setmod!("mih_eq",#««
		"" => (before = ["stratagems"],),
	)#»»
	setmod!("monasticorders",#««
		0 => (path=["Classes", "Monk"],),
		1 => (path=["Classes", "Monk"],),
		2 => (path=["Classes", "Monk"],),
		3 => (path=["Classes", "Monk"],),
		4 => (path=["Classes", "Monk"],),
	)#»»
	setmod!("npckit",#««
	)#»»
	setmod!("rr",#««
		"" => (after = ["refinements", "item_rev", "eetact2", "song_and_silence", "divine_remix", "tod", "spell_rev", "ctb", "d0questpack", "beyond_the_law", ],
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
		12 => (exclusive = ["c6arkan.cre", "c6arkan3.cre", "c6kach.cre", "c6yean.cre", "c6arkan.bcs", "stguard1.cre", "mook02.cre", "arkanisg.cre", "mookft01.cre", "palern.cre", "ar0300.bcs", "stguard1.bcs", "aran.cre", "gaelan.cre", "mook.cre", "booter.cre"],),
	)#»»
	setmod!("refinements",#««
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
	)#»»
	setmod!("scales_of_balance",#««
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
	)#»»
	setmod!("song_and_silence",#««
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
	)#»»
	setmod!("spell_rev",#««
		"" => (after = ["ub",],),
		0 => (exclusive = ["elemtype.itm","mstone.itm", "shille.itm", "shille2.itm", "shille3.itm", "spcl213.spl", "spcl721.spl", "spcl722.spl", "spdr101.spl", "spdr201.spl", "spdr301.spl", "spdr401.spl", "spdr501.spl", "spdr601.spl", "spentaai.bam", "spentaci.bam", "spin101.spl", "spin102.spl", "spin103.spl", "spin104.spl", "spin105.spl", "spin106.spl", "spin113.spl", "spin683.spl", "spin701.spl", "spin788.spl", "spin789.spl", "spmagglo.bam", "spmagglo.vvc", "sppr101.spl", "sppr102.spl", "sppr103.spl", "sppr104.spl", "sppr105.spl", "sppr106.spl", "sppr107.spl", "sppr108.spl", "sppr109.spl", "sppr110.spl", "sppr111.spl", "sppr113.spl", "sppr116.spl", "spra301.spl", "spra302.spl", "spra303.spl", "spra304.spl", "spra305.spl", "spra306.spl", "spwi977.spl", "spwi978.spl", "undtype.itm", "vermtype.itm"], path=["Spells"],),
		10 => (exclusive = ["plangood.cre", "planevil.cre", "devagood.cre", "devaevil.cre"], path=["Cosmetic", "Sprites"],),
		20 => (path = ["Spells", "Illusion"],),
		30 => (path = ["Spells", "Abjuration"],),
		40 => (path = ["Spells", "Enchantment"],),
		50 => (path = ["Spells"],),
# 		65 => (exclusive = ["spcl900.spl","spcl901.spl","spcl907.spl","spwish12.spl"],),
	)#»»
	setmod!("spstuff",#««
		0 => (path = ["Classes", "Ranger"],),
		1 => (path = ["Classes", "Fighter"],),
		2 => (path = ["Classes", "Fighter"],),
		3 => (path = ["Classes", "Thief"],),
		5 => (path = ["Classes", "Druid"],),
		6 => (path = ["Classes", "Bard"],),
		4 => (path = ["Items"],),
	)#»»
	setmod!("stratagems",#««
		"" => (after = ["item_rev", "d0questpack", "ascension", "refinements",
			"spell_rev", "tactics", "wheels", "eetact2"],
			before = ["cdtweaks", "eet_end"],),
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
	)#»»
	setmod!("sword_and_fist",#««
		1 => (path=["Classes", "Monk"],),
		30 => (path=["Classes", "Fighter"],),
		31 => (path=["Classes", "Fighter"],),
		32 => (path=["Classes", "Fighter"],),
		33 => (path=["Classes", "Fighter"],),
		34 => (path=["Classes", "Fighter"],),
	)#»»
	setmod!("therune",#««
		"" => (after = "cowledmenace",),
	)#»»
	setmod!("tnt",#««
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
	)#»»
	setmod!("tomeandblood",#««
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
	)#»»
	setmod!("wheels",#««
		"" => (before = "stratagems",),
	),#»»
	setmod!("wildmage",#««
		0 => (path=["Spells", "New spells"],),
		1 => (path=["Spells", "New spells"],),
		2 => (path=["Items"],),
		3 => (path=["Classes", "Mage"],),
		4 => (path=["Classes", "Mage"],),
		5 => (path=["Spells", "Alteration"],),
	)#»»
		# update `lastupdate` field««
		for (id, mod) in moddb
			isdir(joinpath(MODS, id)) && lastupdate!(mod)
			for e in (".tar.gz", ".zip", ".7z", ".rar")
				a = id*e
				isfile(joinpath(DOWN, a)) && (mod.archive = a; break)
			end
		end#»»

	file = joinpath(TEMP, "moddb")
	write_moddb(file; moddb)
	# we check that this produces a readable moddb before overwriting:
	global global_moddb = read_moddb(file)
	mv(file, MODDB; force=true)
	return nothing
end#»»
# BWS selection handling ««1
function bws_selection(source = BWS_SELECTION)#««
	section = ""
	selection = Tuple{String,Vector{Int}}[]
	for line in eachline(source)
		if line[1] == '[' && line[end] == ']'
			section = line[2:end-1]
			continue
		end
		if section == "Save"
			id, components = split(line, r"\s*=\s*"; limit=2)
			push!(selection, (lowercase(id), split(components)))
		end
	end
	return selection
end#»»
function import_bws_selection((source, dest) = BWS_SELECTION => SELECTION)#««
	global global_selection = bws_selection(source)
	write_selection(global_selection, dest)
end#»»
# ««1
end

M=ModTool
