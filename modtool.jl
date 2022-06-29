# modtool_no_init = 1
""" ModTool - BG2 modding tool

Usage:

ModTool.status()
ModTool.edit(:stratagems) # or edit selection with vim
ModTool.install()

See also `moddb.jl` for initializing configuration files.
"""
module ModTool
# Preamble ««1
# TODO ««
#  + check that status() shows enough mods
#  + update modtool.jl to write correct compat packets
#  - indicate both lanthorn mods in db
#  - uninstall(n), uninstall(upto=m)
#  + status() shows out-of-order components
#   (i.e. those installed before something that should come after)
# mod-specific fixes:
#  - questpack is very badly named
#  - mortis: tra/Francais -> tra/French
#  - aurora: fix .sh files
#  - spstuff: remove unicode BOM from ee.tra
#  - Keto SoAv5 vs SoAv6
#  - mih_ip fails to download
# - Generic installation order
# (https://forums.beamdog.com/discussion/34882/list-of-bg2ee-compatible-mods)
# - use labels like ProjectInfinity
# - provide a simple way to add a mod to the db with minimal input info
#   - use ProjectInfinity's *.ini files if available
#    - needs extracting mods before (re)computing install order
#    - including readme if needed (still prefer local readme)
#   - for github urls:: auto-guess mod dir + name
#   - use this in moddb.jl
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
using IniFile
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
	global moddb = read_moddb(MODDB)
	printlog("read $(length(moddb)) mods in global database")
	global selection = read_selection(SELECTION; verbose=false)
	printlog("read selection status for $(length(selection)) mods; "*
	"$(sum(length.(values(selection)))) components selected")
	global stack = weidu_stack(GAMEDIR)
	printlog("read $(length(stack.bg1))+$(length(stack.bg2)) installed components")
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

function ask(f, message)
	r = nothing
	printask(message)
	while true
		r = readline(); isempty(r) && continue
		y = f(lowercase(r[1]))
		!isnothing(y) && return y
	end
end

# Data structures ««1

# ModComponent ««2
mutable struct ModComponent
	id::String
	name::String
	group::String
	subgroup::String
	path::Vector{String} # e.g. ["Fighting", "Proficiencies"]
	conflicts::Set{String}
	depends::Set{String}
	exclusive::Set{String}
	@inline ModComponent(i, t, g="", s="") =
		new(string(i), t, g, s, [], Set{String}(), Set{String}(), Set{String}())
	@inline ModComponent(d::AbstractDict) =
		new((get(d, k,"") for k in ("id", "name", "group", "subgroup"))...,
		get(d, "path", []), Set(get(d, "conflicts", [])),
		Set(get(d, "depends", [])), Set(get(d, "exclusive", [])))
end
@inline Base.isempty(c::ModComponent) =
	all(isempty(getfield(c, i)) for i in 1:fieldcount(ModComponent))
@inline toml_field(x) = x
@inline toml_field(x::Set) = sort(collect(x))
@inline dict(c::ModComponent, pairs...) =
	Dict(pairs..., (string(s) => toml_field(getfield(c,  s))
	for s in fieldnames(ModComponent) if !isempty(getfield(c, s)))...)
@inline description(c::ModComponent) =
	let n = isempty(c.name) ? "<unknown>" : c.name
	isempty(c.subgroup) ? n : c.subgroup*'/'*n
	end
# ModCompat ««2
mutable struct ModCompat
	# compatibility properties:
	components::String # to which components does this apply?
	ranges::Set{UnitRange{Int}} # same, parsed
	after::Set{String}
	before::Set{String}
# 	conflicts::Set{String}
# 	depends::Set{String}
	function ModCompat(desc = "", after=[], before=[])
		m = new(desc, Set{UnitRange{Int}}(), Set(after), Set(before))
		isempty(desc) || for r ∈ split(desc, ',')
			l = split(r, r"[-:]")
			push!(m.ranges, parse(Int,l[1]):parse(Int,l[min(2, length(l))]))
		end
		return m
	end
end
@inline Base.isempty(c::ModCompat) =
	isempty(c.components) && isempty(c.after) && isempty(c.before)
@inline ModCompat(d::AbstractDict) =
	ModCompat(get(d,"components",""), get(d,"after",[]), get(d,"before",[]))
@inline dict(c::ModCompat) = Dict(string(k)=>toml_field(getfield(c, k))
	for k in (:components, :after, :before) if !isempty(getfield(c, k)))
	
@inline Base.in(i::Integer, c::ModCompat) = any(i ∈ r for r ∈ c.ranges)
@inline Base.in(i::AbstractString, c::ModCompat) = parse(Int, i) ∈ c
# Mod ««2
mutable struct Mod
	id::String
	url::String
	description::String
	archive::String
	class::String
	lastupdate::Date
	readme::String
	tp2::String
	languages::Vector{String}
	compat::Vector{ModCompat} # holds compatibility (etc.) for mod components
	components::Vector{ModComponent} # sorted as in tp2 file
	game_lang::Int # starts at zero (WeiDU indexing)
	tool_lang::Int

	@inline Mod(;id, url="", description="", class="", archive="", tp2="",
		lastupdate=1970, languages = String[], readme="", components=[]) = begin
		new(lowercase(id), url, description, archive, class, Date(lastupdate),
			readme, tp2, languages, [ModCompat()],
			[ ModComponent(k,v) for (k,v) in pairs(components)], 0, 0)
		end
end
"returns the index of compatibility structure matching component k of mod."
function component_compat(m::Mod, k)
	i = parse(Int, k)
	return something(findfirst(x->i ∈ x, m.compat), 1)
end
"returns only components in this compatibility class."
@inline getcompat(db, id, k; moddb=moddb) =
	filter(x->component_compat(moddb[id],x) == k, get(db, id,Set{String}()))
"parses a string 'id:comp' as (mod id, compat index)."
function compat_parse(str; moddb=moddb)
	contains(str, ':') || return (str, 1)
	(id, k) = split(str, ':'; limit=2)
	return (id, component_compat(moddb[id], k))
end
@inline compatname(m::Mod, k) = k ≤ 1 ? m.id : m.id*'/'*m.compat[k].components
@inline modgame(m::Mod; moddb=moddb) =
	m.id ∈ moddb["eet"].compat[1].after ? :bg1 : :bg2
@inline components(m::Mod) = (m.components)
@inline components(m::Mod, c::ModCompat) = filter(x->x.id∈(c), components(m))
@inline function component(mod, k; fail = false)
	k = lowercase(string(k)); for c in mod.components; c.id == k && return c; end
	if fail; error("mod '$(mod.id)': component '$k' not found")
	else return nothing; end
end
@inline function component!(m::Mod, i, c)
	i = something(i, length(m.components)+1)
	resize!(m.components, max(length(m.components), i))
	m.components[i] = c
end
"setlang: recompute mod languages; call this each time languages might change"
@inline setlang!(m::Mod) = if !isempty(m.languages)
	m.game_lang = argmin([lang_score(l, GAME_LANG) for l in m.languages]) - 1
	m.tool_lang = argmin([lang_score(l, TOOL_LANG) for l in m.languages]) - 1
else m.tool_lang = m.game_lang = 0; end

# Mod DB handling ««1
@inline allcomponents(moddb=moddb) =
	Dict(k => Set(c.id for c in moddb[k].components) for (k,v) in moddb)
@inline ifhaskey(f, d, k) = (x = get(d, k, nothing); isnothing(x) || f(x))
@inline addmods!(moddb, mods...) = for m in mods; moddb[m.id] = m; end
const mod_fields=(:id,:url,:class,:description,:archive,:readme,:languages,:tp2)
function merge_moddb(filename = MODDB; moddb = moddb)
	dict = TOML.parsefile(filename)
	for (id, d) in dict
		m = get!(moddb, id, Mod(;id))
		for k in mod_fields
			ifhaskey(d, string(k)) do x; setfield!(m, k, x); end
		end
		setlang!(m)
		ifhaskey(d, "lastupdate") do x; m.lastupdate = Date(x); end
		ifhaskey(d, "compat") do x
			empty!(m.compat)
			for k in sort(parse.(Int, keys(x)))
				push!(m.compat, ModCompat(x[string(k)]))
			end end
		isempty(m.compat) && push!(m.compat, ModCompat())
		any(c->isempty(c.components), m.compat) || push!(m.compat, ModCompat())
		ifhaskey(d, "components") do x; for (k,prop) in x
			# using dict constructor for `ModComponent`:
			component!(m, parse(Int, k), ModComponent(prop))
		end end
	end
	return moddb
end
@inline read_moddb(filename) = merge_moddb(filename; moddb=Dict{String,Mod}())
function write_moddb(io::IO; moddb=moddb)
	TOML.print(io, moddb; sorted=true) do m
		@assert typeof(m) == Mod "bad type for TOML: $(typeof(m))"
		d = Dict{String,Any}()
		for k in mod_fields
			x = getfield(m, k); isempty(x) || (d[string(k)] = x)
		end
		d["lastupdate"] = m.lastupdate
		for c in ("compat", "components")
			x = filter(!isempty, getfield(m, Symbol(c)))
			isempty(x) || (d[c] = Dict(string(i) => dict(c) for (i,c) in pairs(x)))
		end
		d
	end
end
function write_moddb(filename::AbstractString = MODDB; moddb=moddb)
	printlog("writing moddb: $filename")
	mktemp(TEMP) do path, io # try this in a temporary file first
		write_moddb(io; moddb)
		close(io); read_moddb(path) # catch errors...
		mv(path, filename; force=true)
	end
end

@inline function findmod(id::Union{Symbol,String}; moddb = moddb)
	k = get(moddb, lowercase(string(id)), nothing)
	isnothing(k) || return k
	error("mod '$id' not found")
end
@inline findmod(pattern::Regex; moddb = moddb) =
	[ id for id in keys(moddb) if occursin(pattern, id) ]
@inline function Base.setproperty!(m::Mod, k::Symbol, v)
	x = getfield(m, k)
	v == x && return
	global moddb_changed = true
	setfield!(m, k, convert(typeof(x), v))
end

"Given a *sorted* list of (mod id, compat index) pairs,
returns the set of dependency arrows among these components,
as pairs (i, j) of indices in the list."
@inline function dependencies(list; moddb=moddb)
	@assert issorted(list)
	arrows = NTuple{2,Int}[]
	for (i1, (id1, k1)) in pairs(list)
		c = moddb[id1].compat[k1]
		for (id2, k2) in compat_parse.(c.after; moddb)
			j2 = searchsorted(list, (id2, k2))
			isone(length(j2)) && push!(arrows, (first(j2), i1))
		end
		for (id2, k2) in compat_parse.(c.before; moddb)
			j2 = searchsorted(list, (id2, k2))
			isone(length(j2)) && push!(arrows, (i1, first(j2)))
		end
	end
	return arrows
end

# Mod classes for install order#««
# (https://forums.beamdog.com/discussion/34882/list-of-bg2ee-compatible-mods)
# (this is somewhat different from EE Mod Setup's ordering)
const MOD_CLASSES = split(replace("""
	DlcMerger
	Initial
	Early
	Overwrite
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
	Early_Tweak
	Tweak_Early
	Tweaks
	AI
	Sounds
	Late
	UI
	Final
	BADCLASS
""", r"#[^\n]*\n" =>"\n"))# »»
@inline modclass(m::Mod) =
	let k = (findfirst(==(m.class), MOD_CLASSES))
	isnothing(k) && error("mod \"$(m.id)\": bad mod class \"$(m.class)\"")
	k; end
function sortkey2((id,k), prev, moddb)
	# returns (class(id), max class(before id), date(id))
	c0 = modclass(moddb[id])
	c1 = maximum(modclass(moddb[x]) for (x,_) in prev; init = -1)
	return (c0, c1, moddb[id].lastupdate)
end
@inline sortkey1(list, moddb, arrowsto) = i -> begin
	sortkey2(list[i], (list[j] for j ∈ arrowsto[i]), moddb)
end
"""    circulardep(vertices, edges, names)

Given a graph, returns the set of names of a strongly connected component."""
function circulardep(vertices, edges, labels)
	# renumber everything from indices in vertices
	labels = [ join(labels[vertices[i]],':') for i ∈ eachindex(vertices) ]
	edges = Dict(i => Set(findall(∈(edges[v]), vertices))
		for (i, v) ∈ pairs(vertices))
	n = length(labels)
	done = falses(n)
	for i in 1:n
		done[i] && continue
		stack = [[i]]
		while !isempty(stack)
			path = pop!(stack)
			head = last(path)
			done[head] && return "Circular dependency: ", labels[path]
			done[head] = true
			append!(stack, [path;next] for next ∈ edges[head])
		end
	end
end
"Given a list of (mod => component set) (string => string set),
returns the list of (mod, component) string pairs,
sorted in best installation order. Fails if a circular dependency is found."
function installorder(selection=selection; moddb=moddb)
	list = [ (id, component_compat(moddb[id], k))
		for (id, l) in pairs(selection) for k in l]
	for id in Set(keys(selection)); push!(list, (id, 1)); end
	unique!(sort!(list))
	arrowsfrom, arrowsto = ([Set{Int}() for _ in 1:length(list)] for _ in 1:2)
	for (b, a) in dependencies(list)
		push!(arrowsfrom[b], a); push!(arrowsto[a], b)
	end
	ord = Base.Order.By(sortkey1(list, moddb, arrowsto), Base.Order.Reverse)
	available = heapify!(findall(isempty, arrowsfrom), ord)

	todo = Set(eachindex(list))
	ret = sizehint!(empty(list), length(list))
	while !isempty(todo)
		@assert !isempty(available) circulardep(collect(todo), arrowsfrom, list)
		i = heappop!(available, ord)
		push!(ret, list[i]); delete!(todo, i)
		for j in arrowsto[i]
			delete!(arrowsfrom[j], i)
			isempty(arrowsfrom[j]) && heappush!(available, j, ord)
		end
	end
	return reverse!(ret)
end
function check_conflicts(;selection = selection, moddb=moddb)
	owners = Dict{String, Vector{Tuple{String, String}}}()
	# check exclusivity
	for (id, clist) in selection
		mod = moddb[id]
		for c in components(mod)
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

# Pre-extraction functions: download, status ««1
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
# 			printlog("  $(mod.id): computing new url=$url")
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
function early_late(;moddb=moddb, stack=stack)
	list = unique!([(id, component_compat(moddb[id],j)) for (id,j) ∈[stack...;]])
	slist = unique!(sort(list))
	arrows = Set(dependencies(slist; moddb))
	ilist = [ searchsortedfirst(slist, x) for x ∈ list ]
	bad = NTuple{2,Tuple{String,Int}}[]
	for (i2, j2) in pairs(ilist), (i1, j1) in pairs(view(ilist, 1:i2-1))
		# check that there does not exist an arrow i2->i1
		(j2, j1) ∈ arrows || continue
		println("$(list[i2]) should be before $(list[i1])")
		push!(bad, (list[i1], list[i2]))
	end
	return bad # set of early/late pairs
end
"""    status(mod)

Shows on one line the status of a given mod, in the form:

    abcde date identifier description

where:
 - `a` is download status (either `'d'` or `'.'`);
 - `b` is extraction status (either `'x'` or `'.'`);
 - `c` indicates presence of selected components (either `'s'` or `'.'`);
 - `d` indicates installation status (either `'+'` if some components needs to be installed, `'-'` if they need to be removed, `'#'` if both are true, or `'.'` if nothing needs to be done);
 - `e` indicates if the mod was installed too early (`v`), too late (`^`), both (`x`), or none.
 """
function status(mod::Mod, k; selection=selection, selected=nothing, stack=stack,
		bad=())
	sel, ins = (getcompat(x,mod.id,k) for x ∈ (selection, stack_installed(stack)))
	s = !isempty(sel)
	selected ∈ (s, nothing) || return
		
	st = issubset(sel, ins)<<1 | issubset(ins, sel)
	st1, st2, st3 = (("\e[33m",'#',"\e[m"), ("\e[32m",'+',"\e[m"),
		("\e[31m",'-',"\e[m"),("",'.',""))[st+1]
	el = ((mod.id,k) ∈ first.(bad)) << 1 | ((mod.id,k)∈last.(bad))
	el1,el3 = iszero(el) ? ("", "") : ("\e[31m", "\e[m")
	el2 = ('.', '^', 'v', 'x')[el+1]
# 	mod.id ∈ ("eet", "stratagems") && print("\e[34m")
	@printf("%s%s%c%c%c%c%c %7s %-22s %s%s%s\n", st1, el1,
		isfile(joinpath(DOWN, mod.archive)) ? 'd' : '.',
		isextracted(mod) ? 'x' : '.', s ? 's' : '.', st2, el2,
		Dates.format(mod.lastupdate, "yyyy-mm"),
		compatname(mod, k), mod.description, el3, st3)
	print("\e[m")
end
# Mod extraction & update ««1
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
		# m.languages
		for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-languages $(m.tp2)`)
			x = match(r"^(\d+):(.*)$", line); isnothing(x) && continue
			@assert parse(Int, x.captures[1]) == length(m.languages)
			push!(m.languages, fixutf8(x.captures[2]))
		end
		setlang!(m)
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
modsfrom(list; moddb = moddb) =
	(x for x in lowercase.(string.(list)) if haskey(moddb, x))
"""    ini_data(id): updates from ProjectInfinity ini file."""
function ini_data(m::Mod; moddb = moddb)
	file = joinpath(MODS, m.id, m.id*".ini")
	isfile(file) || return
	ini = redirect_stdout(devnull) do; read(Inifile(), file); end
	ini.sections = Dict(lowercase(k)=>v for (k,v) in ini.sections)
	section = get(ini.sections, "metadata", nothing); isnothing(section) && return
	printlog("reading ini file for $(m.id)")
	dict = Dict(lowercase(k)=>v for (k, v) in section)
	function push_compat(set, s)
		list = modsfrom(split(get(dict, s, ""), r",\s*"); moddb)
		list = filter(∉(set), [list...])
		isempty(list) && return
		printlog("  $(m.id).$s ←", join(list, ' '))
		push!(set, list...)
	end
	push_compat(m.compat[1].after, "after")
	push_compat(m.compat[1].before, "before")
	class = get(dict, "type", "")
	if m.class ≠ class ≠ ""
		printwarn(" '$(m.id)': has class $(m.class), INI file says $class")
	end
	if isempty(m.readme)
		m.readme = get(split(get(dict, "readme", ""), r",\s*"), 1, "")
		isempty(m.readme) ||
		printlog("  $(m.id).readme ← $(m.readme)")
	end
end

# WeiDU status««1
@inline function weidu_filter(line)
	m = match(r"^~(.*)~\s+#(\d+)\s+#(\d+)\s*", line)
	isnothing(m) && return nothing
	(tp2, lang, comp) = m.captures
	id = lowercase(tp2[1:end-4])
	k = findlast('/', id); !isnothing(k) && (id = id[k+1:end])
	startswith(id, "setup-") && (id = id[7:end])
	return (id, String(comp))
end
function weidu_stack(dir::AbstractString)
	file = joinpath(dir, "weidu.log")
	isfile(file) || (file = devnull)
	NTuple{2,String}.(filter(!isnothing, weidu_filter.(eachline(file))))
end
@inline weidu_stack(dirs::NamedTuple) =
	NamedTuple{keys(dirs)}(weidu_stack.(values(dirs)))

function stack_installed(stack=stack)
	d = Dict{String,Set{String}}()
	for s in stack, (id, k) in s; push!(get!(d, id, Set{String}()), k); end
	return d
end

# Mod components ««1
function printcomp(io::IO, m, c::ModComponent; selection, installed, moddb)
	@printf(io, "%c%c%c `%s:%s` %s\n",
		c.id ∈ get(selection, m.id,[]) ? 's' : '.',
		c.id ∉ get(installed, m.id,[]) ? '.' : modgame(m; moddb)==:bg1 ? '1' : '2',
		isempty(c.path) ? '.' : 'p',
		m.id, c.id, description(c))
end
function merge_selection(io; selection=selection, verbose=true)
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
" ⟦5 This file is to be read using the vim editor, launched as
"  vim +'so %' <name of the file>
" The following lines map some useful keys for helping modifying the selection:
" - space key (de)selects a component
" - this uses tree folding (open with zo, close with zf)
"
se fencs=utf8 cms= ft= fdm=marker fmr=⟦,⟧
sy match modComp /\`[^\`]*\`/|hi link modComp Constant
sy match modAdd /^s\.[.p] .*$/ contains=modComp |hi link modAdd DiffAdd
sy match modDel /^\.[12][.p] .*$/ contains=modComp |hi link modDel DiffDelete
sy match modSel /^s[12][.p] .*$/|hi link modSel DiffChange
sy match modGrp /^# .*$/|hi link modGrp ModeMsg
sy match modSub /^## .*$/|hi link modSub Title
nmap <buffer> <silent> <Space> :s/^s/¤/e<cr>:s/^\./s/e<cr>:s/^¤/./e<cr>
normal j
finish "⟧5 """

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
function edit(mod::Mod; moddb=moddb, selection=selection, stack=stack,
		installed=stack_installed(stack), edit=true)
	filename = joinpath(TEMP, "selection-"*mod.id*".txt")
	io = edit ? open(filename, "w") : stdout
	edit && println(io, selection_preamble)
	g = "\0"; h = ""
	for c in components(mod)
		g ≠ c.group && (g = c.group; println(io, "# ", g, "⟦1"))
		h ≠ c.subgroup &&
			(h = c.subgroup; println(io,"## ",h, isempty(h) ? "⟧2" : "⟦2"))
		printcomp(io, mod, c; selection, installed, moddb)
	end
	edit || return
	close(io)
	component_editor(filename; selection)
end
@inline show(mod::Mod; kwargs...)= edit(mod; edit=false, kwargs...)
# Component tree ««1
struct ComponentTree
	alternatives::Vector{NTuple{2,String}}
	children::Dict{String,ComponentTree}
	@inline ComponentTree() = new([], Dict())
end#»»
findbranch(root::ComponentTree, path) = isempty(path) ? root :
	findbranch(get!(root.children, first(path), ComponentTree()), path[2:end])
function build_tree(;moddb=moddb)
	root = ComponentTree()
	for (id, m) in moddb, c in components(m)
		isempty(c.path) && continue
		push!(findbranch(root, c.path).alternatives, (id, c.id))
	end
	root
end
function comp_isless((id1, k1), (id2, k2), order)
	id1 == id2 &&
		return isless(something(parse(Int, k1),-1), something(parse(Int, k2),-1))
	i1 = findfirst(∈(((id1,k1),(id1,""))),order)
	i2 = findfirst(∈(((id2,k2),(id2,""))),order)
	return isnothing(i2) || (!isnothing(i1) && isless(i1, i2))
end
function display_tree(io::IO, root, level=1;
		moddb=moddb, selection=selection, installed, order)
	for (id, k) in sort(collect(root.alternatives);
			lt=(x,y)->comp_isless(x,y,order))
		m = moddb[id]
# 		extract(m);
		c = component(m, k); isnothing(c)&&(printerr("'$id:$k' not found");continue)
		println("$id $k")
		printcomp(io, m, c; selection, installed, moddb)
	end
	for (s, c) in sort(collect(root.children); by=first)
		println(io, '#'^level, ' ', s, " ⟦", level)
		display_tree(io, c, level+1; selection, moddb, installed, order)
	end
end
function write_selection(io::IO; selection=selection, moddb=moddb,
		stack=stack, order=installorder(selection))
	installed = stack_installed(stack)
	println(io, selection_preamble)
	# First: sorted components
	println("order is $order")
	println("tree has $(length(build_tree(;moddb).children)) children")
	display_tree(io, build_tree(;moddb); selection, moddb, installed, order)
	# Then unsorted
	println(io, "# Individual, unsorted components ⟦1")
	i = ""
	for (id,_) in order
		m = moddb[id]; g = ""; h = "";
# 		isempty(get(selection, id, ())) || extract(m)
		for c in components(m)
			isempty(c.path) || continue
			id ≠ i && (i=id; println(io, "## ", i, " ⟦2"))
			g ≠ c.group && (g = c.group; println(io, "### ", g, "⟦3"))
			h ≠ c.subgroup &&
				(h=c.subgroup; println(io,"#### ",h,isempty(h) ? "⟧4" : "⟦4"))
			printcomp(io, m, c; selection, installed, moddb)
		end
	end
end
function write_selection(filename::AbstractString = SELECTION; kwargs...)
	printlog("writing selection: $filename")
	open(filename, "w") do io; write_selection(io; kwargs...); end
end
"""    edit()

Edits the global component selection database.

This is built of two parts:
 - the first part groups all “pathed” components, i.e. those which are
   classified by game feature;
 - the second part lists all remaining unclassified components.
"""
function edit(;file=SELECTION, selection=selection, moddb=moddb,
		 stack=stack, order = installorder(allcomponents(moddb); moddb))
	open(file, "w") do io
		write_selection(io; selection, moddb, stack, order)
	end
	component_editor(file; selection)
end

# Mod installation ««1
@inline install(mod::Mod; kwargs...) = 
	for k in 1:length(mod.compat); install(mod, k; kwargs...); end
function install(mod::Mod, index::Integer; simulate=false, uninstall=false,
		selection=selection, gamedirs=GAMEDIR, stack = stack,
		order = installorder(selection), write=false)
	println("install: $(mod.id)/$index")
	extract(mod) || return
	id = mod.id

	gamedir = gamedirs[modgame(mod)]
	installed = stack_installed(stack)
	current = get(installed, mod.id, String[])
	warned = false
	for (i1, k1) in order; i1 == id && break
		isempty(symdiff(get(installed,i1,[]), get(selection,i1,[]))) && continue
		printwarn("warning: mod '$i1/$k1' should probably be installed before '$id'")
		warned = true
	end
	warned && (ask(==('y'), "proceed anyway? (yn)") || return)
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
			c = component(mod, k)
			printwarn(isnothing(c) ? "$k (not found)" : "$k ($(description(c)))")
		end
		ask(==('y'), "update selection? (yn)") && 
			(selection[id] = Set(now_installed); write_selection())
	end
end
function uninstall(mod::Mod; selection=selection, write=true, kwargs...)
	delete!(selection, mod.id)
	install(mod; write, kwargs...)
	write && write_selection()
end
function uninstall(n::Integer=-1 ; moddb=moddb, stack=stack, simulate=true,
		upto=nothing, confirm=true)
	prev = ""; clist = String[]; i = 0
	(n < 0) && (n = isnothing(upto) ? 1 : typemax(n))
	if upto isa String
		upto = Set([upto])
	elseif upto == early_late
		upto = Set([([x[1][1], x[2][1]] for x ∈ early_late())...;])
	end
	for (id, k) in reverse([stack...;])
		j = component_compat(moddb[id], k)
		id == prev && (push!(clist, k); continue)
		if !isempty(prev)
			m = moddb[prev]
			i+=1; i > n && break
			@assert !isempty(clist)
			cmd = `weinstall $prev --language $(m.game_lang) --skip-at-view --noautoupdate --no-exit-pause --force-uninstall-list $clist`
			printsim(cmd)
			!simulate && (!confirm || ask(==('y'), "confirm?")) &&
				cd(GAMEDIR[modgame(m)]) do; run(ignorestatus(cmd)); end
			!isnothing(upto) && (delete!(upto, prev); isempty(upto) && break)
		end
		clist = [k]
		prev = id
	end
end
# Global routines««1
function do_all(f; class=nothing, pause=false, limit=typemax(Int),
		selected = (f == status ? nothing : true),
		selection = selection, moddb=moddb,
		kwargs...)
	i = 0; c = ""
	l = installorder(f == status ? allcomponents(moddb) : selection; moddb)
	bad = early_late(;moddb)
	for (id, k) in l
		mod = moddb[id]
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
			printlog("\e[1m($n/$(length(selection)) $id)\e[m")
		end
		f(mod, k; selected, bad, kwargs...)
	end
end
"returns the first n (mod, compat) pairs for which some installation needs to be done."
function nextmods(n; moddb=moddb, selection=selection, stack=stack,
		order=installorder(selection;moddb))
	installed = stack_installed(stack)
	ret = Tuple{String,Int}[]
	for (id, k) in order
		sel, ins = (getcompat(x, id, k) for x in (selection, installed))
		isempty(symdiff(sel, ins)) && continue
		push!(ret, (id, k))
		length(ret) ≥ n && break
	end
	ret
end
function topmods(n; selection=selection, dir=GAMEDIR.bg2)
end
function do_first(f, n=1; moddb=moddb, selection=selection,
		kwargs...)
	order = installorder(selection)
	for (id,k) in nextmods(n; selection, order)
		ask("call function $f($(compatname(moddb[id],k)))? (ynqrc)") do r
			r == 'r' && (readme(moddb[id]); return true)
			r == 'c' && (edit(moddb[id]); return true)
			r == 'q' && return false
			r == 'y' && (f(moddb[id], k; order, kwargs...); return true)
			r == 'n' && return true
		end || break
	end
end
list1 = (:download, :extract, :install, :status, :update)
for f in list1; @eval begin
	@inline $f(; kwargs...) = do_all($f; kwargs...)
	@inline $f(n::Integer; kwargs...) = do_first($f, n; kwargs...)
end end
# interactive commands are allowed only *one* mod (but can be a symbol)
list2 = (list1..., :readme, :tp2, :show, :status, :edit)
for f in list2; @eval begin
	@inline $f(idlist::Union{String,Symbol}...; kwargs...) =
	for id in idlist; $f(findmod(id); kwargs...); end
end end

# ««1
end
M=ModTool
R=isdefined(M, :moddb) ? M.moddb["rr"] : nothing
C=isnothing(R) ? R : R.components
nothing
# vim: fdm=syntax fdl=1:
