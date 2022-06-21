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
# FIXME:
# + fix updating selection file
# + allow # on right side of =
# + mod subcomponents
# + better detection of mod components
# + include a “date” field
# - use WeiDU's --list-* options
#  + list-languages
#  + list-components
#  + list-readme [MODDED]
#  - list-actions [MODDED]
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
#  + exclusive: owns a symbol
# - complete dependency & conflict checking
# - try and guess readme for individual components (IMPOSSIBLE)
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
const MODDB="$PREFIX/moddb"
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

# Mod classes for install order
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
""", r"#[^\n]*\n" =>"\n"))

struct ComponentProperties#««
	after::Vector{String}
	before::Vector{String}
	conflicts::Vector{String}
	depends::Vector{String}
	exclusive::Vector{String}
end#»»
# we must not overqualify the type of `d`; passing `Dict()` is fine:
@inline ComponentProperties(d::Dict) = ComponentProperties((get(d, s, String[])
	for s in string.(fieldnames(ComponentProperties)))...)
@inline ComponentProperties() = ComponentProperties([], [], [], [], [])
@inline Base.Dict(prop::ComponentProperties) =
	Dict(string(fieldname(ComponentProperties, i)) => getfield(prop, i)
		for i in 1:fieldcount(ComponentProperties)
		if !isempty(getfield(prop, i)))

struct ModComponent
	id::String
	name::String
	group::String
	subgroup::String
	@inline ModComponent(i, t, g="", s="") = new(string(i), t, g, s)
end
@inline Base.iterate(c::ModComponent, args...) =
	Base.iterate((c.id, c.name, c.group, c.subgroup), args...)
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
	properties::Dict{String,ComponentProperties}
	# data recomputed at runtime:
	tp2::String
	languages::Vector{String}
	sel_lang::Int # starts at zero (WeiDU indexing)
	readme::String
	components::Vector{ModComponent}
	# components["1"] = Dict(
	# 	"depends" => [ dependencies...],
	# 	"exclusive" => [ list of symbols... ],
	# each symbol (e.g. tac0 table) is owned at most once
	#	)

	@inline Mod(;id, url, description, class, archive="", lastupdate=1970,
		properties = Dict(), tp2 = "", languages = String[]) = begin
		new(lowercase(id), url, description, archive, class, Date(lastupdate),
			Dict(k=>ComponentProperties(v) for (k,v) in pairs(properties)), tp2,
			languages, 0, "", [])
		end
end#»»
@inline sortkey(a::Mod) = (modclass(a.class), a.lastupdate)
@inline Base.isless(a::Mod, b::Mod) = sortkey(a) < sortkey(b)
@inline modclass(class::Integer) = class
@inline modclass(class::AbstractString) = (findfirst(==(class), MOD_CLASSES))
@inline modarchive(m::Mod) = m.id*match(r"\.[^.]*$", m.url).match
const EET_class = modclass("EET")
@inline modgame(m::Mod) = modclass(m.class) < EET_class ? :bg1 : :bg2

# Mod DB handling ««1
function read_moddb(io::IO)#««
	dict = TOML.parse(io); return [
		Mod(id=lowercase(id),
			url=d["url"],
			description=d["description"],
			class=d["class"],
			archive=get(d, "archive", ""),
			lastupdate=get(d, "lastupdate", "1970"),
			properties=get(d, "properties", Dict()),
		) for (id, d) in dict ]
end#»»
function write_moddb(io::IO; moddb=global_moddb)#««
	TOML.print(io, Dict(m.id => m for m in moddb)) do m
		d = Dict("url" => m.url, "lastupdate" => m.lastupdate,
		"description" => m.description, "class" => m.class)
		isempty(m.archive) || (d["archive"] = m.archive)
		isempty(m.properties) ||
			(d["properties"] = Dict(k=>Dict(v) for (k,v) in m.properties))
		d
	end
end#»»
@inline write_moddb(filename::AbstractString = MODDB; kwargs...) =
	open(filename, "w") do io write_moddb(io; kwargs...); end
@inline read_moddb(filename::AbstractString = MODDB; kwargs...) =
	open(filename, "r") do io read_moddb(io; kwargs...); end

@inline findmod(id::Union{Symbol,String}; moddb = global_moddb) =
	for mod in moddb; mod.id == lowercase(string(id)) && return mod; end
@inline findmod(pattern::Regex; moddb = global_moddb) =
	[ mod.id for mod in moddb if occursin(pattern, mod.id) ]

# Pre-TP2 functions: download, extract, status ««1
function updateurl(mod::Mod)#««
	printlog("get latest release for github file $(mod.url)")
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
			printlog("download $url to $archive")
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
function extract(mod::Mod; down=DOWN, mods=MODS, simulate=false)#««
	download(mod; down=DOWN) || return false
	mod.archive == "Manual" && return true
	archive = joinpath(down, mod.archive)
	isdir(joinpath(mods, mod.id)) || mktempdir(TEMP) do(dir); cd(dir) do
		printlog("extract $archive to $dir")
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
	lastupdate!(mod; mods) && write_moddb()
	return true
end#»»
function lastupdate!(mod::Mod; mods=MODS)
	# we keep the date of the newest .tp2, .tpa, .tph file:
	f = "setup-$(mod.id).tp2"; t = isfile(f) ? mtime(f) : 0
	d = Date(unix2datetime(maximum(mtime(joinpath(root, f))
		for (root,_,files) in walkdir(joinpath(mods, mod.id))
		for f in files if endswith(f, r"\.tp[2ah]"i); init=t)))
	d == mod.lastupdate && return false
	mod.lastupdate = d
	printlog("$(mod.id): recomputed last update to $d")
	return true
end
function status(mod::Mod; selection=global_selection, selected=false)#««
	sel = !isempty(get(selection, mod.id, Int[]))
	!selected || sel || return
	@printf("%c%c%c %7s %-22s %s\n",
		isfile(joinpath(DOWN, mod.archive)) ? 'd' : '.',
		ispath(joinpath(MODS, mod.id)) ? 'x' : '.',
		sel ? 's' : '.',
		Dates.format(mod.lastupdate, "yyyy-mm"),
		mod.id, mod.description)
end#»»

# TP2 data extraction ««1
function lang_score(langname, pref_lang=PREF_LANG)#««
	for (i, pref) in pairs(pref_lang)
		occursin(pref, langname) && return i
	end
	return typemax(Int)
end#»»
function tp2data!(m)#««
	extract(m) || return nothing
	id = m.id; m.tp2 = ""
	for path in ("$id.tp2", "$id/$id.tp2", "$id/setup-$id.tp2", "setup-$id.tp2")
		ispath(joinpath(MODS, path)) && (m.tp2 = path; break)
	end
	isempty(m.tp2) && error("no TP2 file found")
	m.languages = String[]
	m.sel_lang = 0
	cd(MODS) do
	# m.languages««
	for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-languages $(m.tp2)`)
		x = match(r"^(\d+):(.*)$", line); isnothing(x) && continue
		@assert parse(Int, x.captures[1]) == length(m.languages)
		push!(m.languages, x.captures[2])
	end
	isempty(m.languages) ||
		(m.sel_lang = argmin([lang_score(l, PREF_LANG) for l in m.languages]) - 1)
#»»
	# components««
	empty!(m.components)
	for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-components-json $(m.tp2) $(m.sel_lang)`)
		startswith(line, "[{") || continue
		v = JSON.parse(line)
		m.components = [ ModComponent(x["number"], x["name"], 
			get(x["group"], 1, ""), get(x, "subgroup", "")) for x in v ]
	end#»»
	# readme««
	m.readme = ""
	for line in eachline(`weidu --game $(GAMEDIR.bg2) --list-readme $(m.tp2) $(m.sel_lang)`)
		x = match(r"^R (.*)", line); isnothing(x) && continue
		isfile(x.captures[1]) && (m.readme = joinpath(MODS, x.captures[1]); break)
	end
	if isempty(m.readme) # try and guess...
		readmes = [ f for f in readdir(joinpath(MODS, id))
			if occursin(r"readme"i, f) && isfile(joinpath(MODS, id, f)) ]
		!isempty(readmes) && 
			(m.readme = joinpath(MODS, id, 
				readmes[argmin([ lang_score(f, PREF_LANG) for f in readmes ])]))
	end#»»
	end # cd(mods)
end#»»
function readme(mod::Mod)#««
	extract(mod) || return false
	tp2data!(mod)
	printlog("showing readme file '$(mod.readme)'")
	if endswith(mod.readme, r".html?"i)
		if occursin('#', mod.readme)
		# cannot call w3m directly, it fails with mesh sign in filename
		# don't run -dump — there might be links to supplemental info
			cd(basename(mod.readme)) do
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
function tp2(mod::Mod)#««
	extract(mod) || return
	run(`less $MODS/$(modtp2file(mod))`)
end#»»

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
	for id in todo
		text*= id*'='*join(selection[id], ' ')*'\n'
	end
	printlog("modified $nchanged lines, added $(length(todo)) lines")
	cp(filename, filename*'~'; force=true)
	open(filename, "w") do io; write(io, text); end
	run(ignorestatus(`diff --color=always $(filename*'~') $filename`))
end#»»
# Mod components ««1
# function weidu_status(weidu_log)#««
# 	status = Dict{String, Vector{String}}()
# 	ispath(weidu_log) && for line in eachline(weidu_log)
# 		line = replace(line, r"\s*//.*" => "")
# 		isempty(line) && continue
# 		(tp2, lang, comp, rest...) = split(line, ' ')
# 		tp2 = lowercase(tp2[2:prevind(tp2, length(tp2))])
# 		push!(get!(status, tp2, String[]), comp[2:end])
# 	end
# 	status
# end#»»
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
# @inline modstatus(db, tp2file) = get(db, tp2file, Int[])
# @inline componentstatus(db, tp2file, c) = (c ∈ modstatus(db, tp2file))
function components(mod::Mod; selection=global_selection, gamedirs=GAMEDIR,#««
		less=true)
	tp2data!(mod)
	g = modgame(mod); gamedir = gamedirs[g]
	ins = modstatus(mod.tp2, gamedir)
	sel = get!(selection, mod.id, Int[])
	@printf("\e[1m%-30s %s\e[m\n", mod.id, mod.description)
	open(less ? `view - ` : `cat`, "w", stdout) do io
	for c in mod.components
		s = c.id ∈ sel ? 's' : '.'
		i = isempty(ins) ? '.' : g == :bg1 ? '1' : '2'
		@printf(io, "%c%c % 4s %s\n", s, i, c.id, description(c))
	end
	end
end#»»
function components!(mod::Mod; selection=global_selection,#««
		gamedir = GAMEDIR[modgame(mod)], cols=80)
	tp2data!(mod)
	ins = modstatus(mod.tp2, gamedir)
	sel = get!(selection, mod.id, Int[])
	filename = joinpath(TEMP, "selection-"*mod.id*".txt")
	open(filename, "w") do io
		g = ""; h = ""
		for c in mod.components
			g ≠ c.group && (g = c.group; println(io, "# ", g, "««1"))
			h ≠ c.subgroup &&
				(h = c.subgroup; println(io, "## ", h, isempty(h) ? "»»2" : "««2"))
			s = c.id ∈ sel ? 's' : '.'
			i = isempty(ins) ? '.' : modgame(mod) == :bg1 ? '1' : '2'
			@printf(io, "%c%c % 4s %s\n", s, i, c.id, description(c))
		end
	end
	# vim will be better than whiptail: we have syntax highlight + folding
	run(`vim -c 'se ft= fdm=marker fmr=««,»»|
	sy match modAdd /^s\. .*$/|hi link modAdd DiffAdd|
	sy match modDel /^\.[12] .*$/|hi link modDel DiffDel|
	sy match modSel /^s[12] .*$/|hi link modSel DiffChange|
	sy match modGrp /^# .*$/|hi link modGrp ModeMsg|
	sy match modSub /^## .*$/|hi link modSub Title|
	nmap <Space> :s/^s/¤/e<cr>:s/^\./s/e<cr>:s/^¤/./e<cr>' $filename`)
	newsel = String[]
	open(filename, "r") do io; for line in eachline(io)
		m = match(r"^([s])[.12]\s+(\d+)\s+", line); isnothing(m) && continue
		push!(newsel, m.captures[2])
	end end
	rm(filename)
	to_add, to_del = setdiff(newsel, selection[mod.id]),
		setdiff(selection[mod.id], newsel)
	printlog("added $(length(to_add)) components: ", join(to_add, ", "))
	printlog("deleted $(length(to_del)) components: ", join(to_del, ", "))
	selection[mod.id] = newsel
	# call whiptail
# 	args = [ "--output-fd", "2", "--separate-output",
# 		"--checklist", "Select mod components", "25", string(cols), "15" ]
# 	for c in mod.components
# 		push!(args, c.id,
# 			(isempty(ins) ? '.' : modgame(mod)==:bg1 ? '1' : '2')*' '*description(c),
# 			(c.id ∈ sel ? "1" : "0"))
# 	end
# 	fd = Pipe()
# 	proc = run(pipeline(ignorestatus(`whiptail $args`), stderr=fd))
# 	close(fd.in)
# 	iszero(proc.exitcode) || return
# 	sel = [ replace(x, '"' => "") for x in eachline(fd) ]
# 	found = false
# 	selection[mod.id] = sel
	return
end#»»
# Mod installation ««1
"Returns the dependency matrix for these mods, encoded as
(after = dict(mod1 => mod2, ...), before = dict(mod1 => mod2, ...))"
function dependencies(list ;moddb=global_moddb)#««
	dep = (arrowsfrom = Dict{String,Set{String}}(),
		arrowsto = Dict(id => 0 for id in list))
	function connect((b, a),)
		fb = get!(dep.arrowsfrom, b, Set{String}())
		a ∉ fb && (push!(fb, a); dep.arrowsto[a]+= 1)
	end
# 	@inline connect((b, a),) =
# 		(push!(get!(dep.arrowsfrom, b, Set{String}()), a); dep.arrowsto[a]+= 1)
	for id in list
		c = findmod(id; moddb).properties
		for (k, v) in pairs(c)
			# special: empty key indicates a dependency for the whole mod
			isempty(k) || continue
			isempty(v.after) && isempty(v.before) && continue
			for a in v.after; a ∈ list && connect(a => id); end
			for b in v.before; b ∈ list && connect(id => b); end
		end
	end
	return dep
end#»»
"Returns a list of mod IDs in install order, given all dependencies"
function installorder(list; moddb=global_moddb)#««
	# use Kahn's algorithm for topological sorting
	# (this should be average-time quasi-linear with the use of a heap)
	ret = sizehint!(String[], length(list))
	todo = Set(collect(list))
	dep = dependencies(list;moddb)
	ord = Base.Order.By(id -> sortkey(findmod(id;moddb)))
	available = [ id for id in list if iszero(dep.arrowsto[id]) ]
	heapify!(available, ord)
	function check_arrows(y)
		s = [ x for x ∈ todo if y ∈ get(dep.arrowsfrom, x, String[])]
		n = count(y ∈ get(dep.arrowsfrom, x, String[]) for x ∈ todo)
	end
			
	while !isempty(todo)
		# loop invariant: arrowsfrom[y] = count of x ∈ todo such that x->y
		@assert !isempty(available) "Circular dependency found: $(todo)"
		x = heappop!(available, ord)
		push!(ret, x); delete!(todo, x)
		for y in get(dep.arrowsfrom, x, String[])
			dep.arrowsto[y]-= 1
			iszero(dep.arrowsto[y]) && heappush!(available, y, ord)
		end
	end
	return ret
end #»»
function check_conflicts(;selection = global_selection, moddb=global_moddb)#««
	owners = Dict{String, Vector{Tuple{String, String}}}()
	# check exclusivity
	for (id, clist) in selection
		mod = findmod(id; moddb)
		for (c, v) in mod.properties
			c ∈ clist || continue
			for e in v.exclusive
				push!(get!(owners, e, valtype(owners)([])), (id, c))
			end
		end
	end
	for (x, v) in owners; length(v) ≤ 1 && continue
		println("tag \"$x\" is owned by the following components:")
		for (id, c) in v; println("  ", id, ":", c); end
	end
end#»»
function install(mod; simulate=false, uninstall=false, #««
		selection=global_selection, gamedirs=GAMEDIR)
	id = mod.id; tp2data!(mod)
	selected = uninstall ? Int[] : get!(selection, id, Int[])

	gamedir = gamedirs[modgame(mod)]
	status = weidu_status(join(gamedir, "weidu.log"))
	current = Set(modstatus(status, mod.tp2))

	to_add = string.(setdiff(selected, current))
	to_del = string.(setdiff(current, selected))
	isempty(to_add) && isempty(to_del) && (printlog("nothing to do"); return)
	if !uninstall
		# create symlink
		for file in ("$id.tp2", "setup-$id.tp2", "$id")#««
			target = joinpath(MODS, file); ispath(target) || continue
			link = joinpath(gamedir, file); ispath(link) && continue
			if simulate
				printlog("create $link -> $(relpath(link, target))")
			else
				symlink(relpath(target, gamedir), link)
			end
		end#»»
	end
	cd(gamedir) do#««
		cmd = `weinstall $(mod.id) --language $(mod.sel_lang) --skip-at-view --noautoupdate --no-exit-pause --force-install-list $to_add --force-uninstall-list $to_del`
		printlog(cmd)
		simulate && return
		run(cmd)
	end#»»
end#»»
@inline uninstall(mod; kwargs...) = install(mod; uninstall=true, kwargs...)
# Global routines««1
function do_all(f; class=nothing, pause=false, limit=typemax(Int),
		selected = (f ≠ status), selection = global_selection, moddb=global_moddb,
		kwargs...)
	i = 0; c = ""
	l = installorder(f == status ? [mod.id for mod in moddb] :
		[id for (id,l) in selection if !isempty(l)]; moddb)
	for id in l
		mod = findmod(id; moddb)
		pause && (println("(paused)"); readline())
		i+= 1; (i > limit) && break
		# do nothing if this is the wrong mod class:
		mod.class ≠ class ≠ nothing && continue
		# do nothing if we process only `selected` mods and this one is not
		# selected:
		# FIXME: we should act if mod is selected OR installed
		s = !isempty(get(selection, mod.id, Int[]))
		!selected || s || continue 
		if f == status
			(mod.class ≠ c) && (c = mod.class; println("\e[1m$c\e[m"))
		else
			printlog("\e[1m($n/$(length(global_selection)) $id)\e[m")
		end
		f(mod; kwargs...)
	end
end
list1 = (:download, :extract, :install, :uninstall, :show_components, :status)
for f in list1; @eval begin
	@inline $f(; kwargs...) = do_all($f; kwargs...)
end end
# interactive commands are allowed only *one* mod (but can be a symbol)
list2 = (list1..., :readme, :tp2, :components!, :components, :status)
for f in list2; @eval begin
	@inline $f(id::Union{String,Symbol}; kwargs...) = $f(findmod(id); kwargs...)
end end

# BWS mod db handling ««1
function bws_moddb(source = BWS_MODDB, orderfile=BWS_BG2IO)#««
	thismod, url, name, arch = [ Dict{String,String}() for _ in 1:4 ]
	id = ""
	for line in eachline(source)
		if line[1] == '['
			id = lowercase(line[2:end-1])
		else
			(k, v) = split(line, r"\s*=\s*"; limit=2)
			thismod[k] = v
			k == "Tra" || continue # last key:
			url[id], name[id], arch[id] = (thismod["Down"], thismod["Name"], thismod["Save"])
		end
	end
	id_done = Set{String}()
	class = Dict{String, String}()
	current_game = 1
	for line in eachline(orderfile)
		occursin(r";EET;", line) && (current_game = 2)
		v = split(line, ";")
		first(v) ∈ ("MUC", "STD", "SUB") || continue
		id = lowercase(v[2]); id ∈ id_done && continue
		push!(id_done, id)
		class[id]= current_game == 1 ? "PreEET" :
			["Initial", "Fixes", "BigQuests", "Quests", "SmallQuests",
			"NPCs", "SmallNPCs", "NPCTweaks", "Tweaks", "Tweaks", "Items",
			"Tweaks", "Kits", "GUI"][1+parse(Int, v[4])]
	end
	return [ mkmod(id, url[id], name[id], class[id], arch[id])
		for id in id_done if haskey(url, id) ]
end#»»
function import_bws_moddb((source, dest) = BWS_MODDB => MODDB,#««
		orderfile = BWS_BG2IO)
	file = joinpath(TEMP, "moddb")
	db = bws_moddb(source, orderfile)
	complete_db!(db)
	write_moddb(file; moddb=db)
	# we check that this produces a readable moddb before overwriting:
	global_moddb = read_moddb(file)
	mv(file, MODDB; force=true)
	return nothing
end#»»
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

# Generating mod DB ««1
function mkmod(id, url, description, class, archive = "")#««
	m = match(r"github.com/(([^/])*/([^/]*))", url)
	id == "klatu" && (id = "tnt")
	if m ≠ nothing
		url = "github:"*m.captures[1]
		archive = ""
	end
	# use github urls whenever possible
	url = get(Dict(#««
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
	"bolsa" => "github:SpellholdStudios/Bolsa",
	"fadingpromises" => "github:SpellholdStudios/Fading_Promises",
	"lucy" => "github:SpellholdStudios/Lucy_the_Wyvern",
	"tnt" => "github:BGforgeNet/bg2-tweaks-and-tricks",
	"bg1npcs" => "github:Gibberlings3/BG1NPC",
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
	), id, url)#»»
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
	class = get(Dict("DlcMerger"=>"Initial",
		"ascension"=>"BigQuests",
		), id, class)
# 	id ∈ (:bg1aerie, :bg1ub, :darkhorizonsbgee, :drake, :sirene, 
# 		Symbol("garrick-tt"), :k9roughworld, :k9sharteelnpc, :tenyathermidor,
# 		:soa, :karatur, :verrsza, :white) && (class = :PreEET)
	return Mod(;id, url, description, class, archive)
end#»»
function mod_exclusives(mod::Mod; except = String[])#««
	g = modgame(mod); gamedir = GAMEDIR[g]
	tp2data!(mod)
	ret = Pair{Int, Vector{String}}[]
	cd(MODS) do
		for c in mod.components
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
# function exclusive!(id;moddb = global_moddb, except=String[])
# 	mod = findmod(id;moddb)
# 	setcomp!(mod.components, mod_exclusives(mod; except)...)
# end
function complete_db!(moddb) #««
	push!(moddb,
	# Argent77 ««3
	mkmod("a7-convenienteenpcs", "github:Argent77/A7-NoEENPCs",
		"Convenient EE NPCs", "NPCTweaks"),
	mkmod("extraexpandedencounters", "https://forums.beamdog.com/uploads/editor/ta/auj7pwad39pd.zip", "Extra Expanded Encounters", "Quests"),
	# ArtemiusI ««3
	mkmod("housetweaks", "github:ArtemiusI/House-Rule-Tweaks",
		"House Rule Tweaks", "Tweaks"),
	# Spellhold Studios ««3
	mkmod("saradas_magic", "http://www.mediafire.com/download/rg5cyypji1om22o/Saradas%2520Magic%2520ENG%2520V_1.1.zip", "Saradas Magic", "PreEET"),
	mkmod("saradas_magic_2", "github:SpellholdStudios/Saradas_Magic_for_BG2", "Saradas BG2", "Spells"),
	mkmod("beyond_the_law", "github:SpellholdStudios/Beyond_the_Law", "Beyond the Law", "NPCs"),
	mkmod("kiara-zaiya", "github:SpellholdStudios/Kiara_Zaiya", "Kiara Zaiya NPCs", "NPCs"),
	mkmod("iylos", "github:SpellholdStudios/Iylos", "Iylos (ToB monk)", "NPCs"),
	mkmod("firkraag", "github:SpellholdStudios/Super_Firkraag", "Super Firkraag", "Quests"),
	mkmod("ruad", "github:SpellholdStudios/RuadRofhessaItemUpgrade", "Ruad Ro'fhessa Item Upgrade", "Items"),
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
	mkmod("sodtrtd", "github:Gibberlings3/Road_To_Discovery_for_SoD", "Road to Discovery", "Quests"),
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
	#»»3
	)
	function setmod!(id, list...; kwargs...)#««
		m = findmod(id; moddb)
		for (i, kv) in list
			p = get!(m.properties, string(i), ComponentProperties())
			for (k, v) in pairs(kv)
				push!(getfield(p, Symbol(k)), unique(sort([v;]))...)
			end
		end
		for (k, v) in kwargs
			k == :class && (m.class = v; break)
			error("unknown keyword: $k")
		end
	end#»»
	setmod!("atweaks",#««
		"" => (after = ["rr", "stratagems"],),
# 		102 => (exclusive = "shammr.itm",), # spiritual hammer
# 		103 => (exclusive = ["spwi406.spl", "spwi602.spl"],), # globes of invul.?
# 		104 => (exclusive = "spwi105.spl",), # colored spray
# 		105 => (exclusive = "spdimndr.bam",), # dim. door animation
# 		110 => (exclusive = "arrow_damage",),
# 		125 => (exclusive = "spcl311.spl",), # animal empathy
# 		150 => (exclusive = "smarter_demons",),
# 		152 => (exclusive = "smarter_demons",),
# 		153 => (exclusive = "smarter_demons",),
# 		186 => (exclusive = "sppr410.spl",), # call woodland beings
# 		205 => (exclusive = "simulacrum_items",),
# 		211 => (exclusive = "sppr409.spl",), # death ward
# 		212 => (exclusive = "sppr209.spl",), # know alignment
# 		261 => (exclusive = "xpbonus.2da",), # trap XP
# 		301 => (exclusive = "npchan.itm",), # Corthala armor
# 		302 => (exclusive = "wa2robe.itm",), # Vecna robe
# 		500 => (exclusive = "container_capacity",),
# 		502 => (exclusive = "container_capacity",),
# 		 101 => (exclusive = ["fl#idim3.eff"],)
 104 => (exclusive = ["spwi105.spl", "spin937.spl"],),
 105 => (exclusive = ["spwi402.spl"],),
 202 => (exclusive = ["spin101.spl", "spin104.spl", "spin102.spl", "spin105.spl", "spin106.spl"],),
 300 => (exclusive = ["spdimndr.bam", "eff_m09.wav"],),
 322 => (exclusive = ["spdimndr.bam", "eff_m09.wav"],),
 323 => (exclusive = ["spwi402.spl"],),
 324 => (exclusive = ["spdimndr.bam", "eff_m09.wav"],),
	)#»»
	setmod!("cdtweaks",#««
		"" => (after = ["tomeandblood", "bg1npc", "thecalling", "item_rev", "divine_remix"],),
		1040 => (exclusive = ["amncen1.cre", "amng1.cre" ],), #(Etc.) Amn guards
		1120 => (exclusive = "store_stacks",),
		1130 => (exclusive = "reputation_reset",),
		1150 => (exclusive = ["wwbearwe.cre", "wwbear.cre", "sppr604.spl", "spcl643.spl", "spcl644.spl", "anisum04.2da", "anisum05.2da"],),
		2040 => (exclusive = "universal_clubs",),
		2060 => (exclusive = "universal_fighting_styles",),
		2080 => (exclusive = "lunumab.2da",),
		2090 => (exclusive = ["xplevel.2da", "hpclass.2da",],),
		2091 => (exclusive = "xplevel.2da",),
		2092 => (exclusive = "xplevel.2da",),
		2100 => (exclusive = "armored_thieving",),
		2120 => (exclusive = "armored_spellcasting",),
		2150 => (exclusive = "protection_items",),
		2151 => (exclusive = "protection_items",),
		2152 => (exclusive = "protection_items",),
		2160 => (exclusive = "weapprof.2da/types",),
		2161 => (exclusive = "weapprof.2da/types",),
		2162 => (exclusive = "weapprof.2da/types",),
		2163 => (exclusive = "weapprof.2da/types",),
		2164 => (exclusive = "weapprof.2da/types",),
		2161 => (exclusive = "weapprof.2da/types",),
		2210 => (exclusive = "wspecial.2da/speed",),
		2211 => (exclusive = "wspecial.2da/speed",),
		2240 => (exclusive = "thac0.2da",),
		2250 => (exclusive = "mxsplsrc.2da",),
		2260 => (exclusive = "mxsplwiz.2da",),
		2261 => (exclusive = "mxsplwiz.2da",),
		2270 => (exclusive = "mxsplbrd.2da",),
		2271 => (exclusive = "mxsplbrd.2da",),
		2280 => (exclusive = "mxsplclr.2da",),
		2281 => (exclusive = "mxsplclr.2da",),
		2290 => (exclusive = "mxspldrd.2da",),
		2291 => (exclusive = "mxspldrd.2da",),
		2292 => (exclusive = "mxspldrd.2da",),
		2293 => (exclusive = "mxspldrd.2da",),
		2294 => (exclusive = "mxspldrd.2da",),
		2295 => (exclusive = "mxspldrd.2da",),
		2296 => (exclusive = "mxspldrd.2da",),
		2298 => (exclusive = "mxspldrd.2da",),
		2300 => (exclusive = ["lufmc.2da", "lufmt.2da"],),
		2310 => (exclusive = "spell_save_penalties",),
		2311 => (exclusive = "spell_save_penalties",),
		2312 => (exclusive = "spell_save_penalties",),
		2340 => (exclusive = "summlimt.2da/celestial",),
		2370 => (exclusive = "clsrcreq.2da",),
		2372 => (exclusive = "clsrcreq.2da",),
		2390 => (exclusive = "mxsplpal.2da",),
		2391 => (exclusive = "mxsplpal.2da",),
		2400 => (exclusive = "mxsplran.2da",),
		2401 => (exclusive = "mxsplran.2da",),
		2410 => (exclusive = "druid_alignment",),
		2420 => (exclusive = "multiclass_cleric_weapons",),
		2430 => (exclusive = "multiclass_druid_weapons",),
		2431 => (exclusive = "multiclass_druid_weapons",),
		2440 => (exclusive = "clswpbon.2da",),
		2530 => (exclusive = "lightning_bolt",),
		3000 => (exclusive = "hpclass.2da",),
		3001 => (exclusive = "hpclass.2da",),
		3002 => (exclusive = "hpclass.2da",),
		3008 => (exclusive = "hpclass.2da",),
		3030 => (exclusive = "spell_learning",),
		3031 => (exclusive = "spell_learning",),
		3040 => (exclusive = "container_capacity",),
		3070 => (exclusive = "store_prices",),
		3071 => (exclusive = "store_prices",),
		3072 => (exclusive = "store_prices",),
		3073 => (exclusive = "store_prices",),
		3080 => (exclusive = "ammo_stacking",),
		3081 => (exclusive = "ammo_stacking",),
		3082 => (exclusive = "ammo_stacking",),
		3083 => (exclusive = "ammo_stacking",),
		3090 => (exclusive = "gem_stacking",),
		3091 => (exclusive = "gem_stacking",),
		3092 => (exclusive = "gem_stacking",),
		3093 => (exclusive = "gem_stacking",),
		3100 => (exclusive = "potion_stacking",),
		3101 => (exclusive = "potion_stacking",),
		3102 => (exclusive = "potion_stacking",),
		3103 => (exclusive = "potion_stacking",),
		3110 => (exclusive = "scroll_stacking",),
		3111 => (exclusive = "scroll_stacking",),
		3112 => (exclusive = "scroll_stacking",),
		3113 => (exclusive = "scroll_stacking",),
		3120 => (exclusive = "happy_patch",),
		3200 => (exclusive = "sellable_items",),
		4010 => (exclusive = "improved_fate_spirit",),
		4110 => (exclusive = "kagain.cre",),
		4120 => (exclusive = "coran.cre",),
	)#»»
	setmod!("cowledmenace",#««
		"" => (after = "eet", depends = "eet",),
	)#»»
	setmod!("d0questpack", #««
		"" => (after = ["kelsey", "keto", "ub" ],),
		0 => (exclusive = "AI",),
		5 => (exclusive = ["c6kach.cre", "ar0300.bcs", "ar0300.are", "aran.cre", "maevar.cre", "mvguard1.cre", "mvpries.cre", "aran02.cre", "arnfgt03.cre", "arnfgt04.cre", "renal.cre", "thief1.cre", "tassa.cre", "c6tanov.bcs", "vvtanov.cre",],),
   6 => (exclusive = ["sw1h50.itm"],),
		8 => (exclusive = ["ar0812.are", "ar1515.bcs", "thumb.cre", ],),
		9 => (exclusive = ["potn33.itm", "potn38.itm", "spwi106.spl", "spwi815.spl",],),
  10 => (exclusive = ["hellself.eff", "spin755.spl", "spin751.spl", "sphl004.spl", "spin753.spl", "hellself.cre", "sphl003.spl", "sphl005.spl", "hellgen2.cre", "sphl001.spl", "sphl002.spl", "cutc7g.bcs", "spin749.spl", "spin747.spl"],),
  11 => (exclusive = ["amtarc01.cre", "amtcap01.cre", "amtcle01.cre", "amtgen01.cre", "amtmag01.cre", "amtpik01.cre"],),
		12 => (exclusive = ["Oasis", "ar6300.are", ],),
		13 => (exclusive = ["ppsanik.cre", "pirmur02.cre", "pirmur05.cre",],),
  15 => (exclusive = ["suinvis.cre", "sumound.cre", "sudryad.cre", "sufake.itm", "sutear.itm", "sutear.bam", "sufake.bam"],),
  16 => (exclusive = ["besamen.cre", "baisera.cre"],),
		17 => (exclusive = ["kamir.cre",],),
		19 => (exclusive = ["ar2402.are",],),
  20 => (exclusive = ["spin671.spl"],),
		21 => (exclusive = ["ar0530.are", "ar0530.bcs",],),
 401 => (exclusive = ["sukiss1.cre", "sukissk.wav", "sumist.cre", "suspyim.cre", "reddeath.bcs",],),
	; class = "Quests")#»»
	setmod!("d0tweak",#««
		11 => (after = ["rr",],), # ioun stones...
	)#»»
	setmod!("d5_random_tweaks", #««
		"" => (after = ["spell_rev", "item_rev"],),
# 		2105 => (exclusive = "spwi105.spl",), # Color Spray (not exclusive)
		 1202 => (exclusive = "sppr202.spl",), # barkskin
		 1207 => (exclusive = ["sppr207.spl"],),
		 1212 => (exclusive = "sppr212.spl",), # slow poison
		 1251 => (exclusive = "sppr251.spl",), # alicorn lance
		 1323 => (exclusive = ["sppr350.spl"],), # clarity/exaltation
		 1351 => (exclusive = ["moonbla.itm"],), # moonblade
		 1404 => (exclusive = "sppr404.spl",), # neutralize poison
		 1603 => (exclusive = "sppr603.spl",), # blade barrier
		 1611 => (exclusive = "sppr611.spl",), # wondrous recall
		 1614 => (exclusive = "sorb.itm",), # sol's searing orb
		 1710 => (exclusive = "sppr710.spl",), # holy word
		 2108 => (exclusive = "spwi108.spl",), # Prot. Petrification
		 2112 => (exclusive = "spwi112.spl",), # magic missile
		 2201 => (exclusive = ["7eyes.2da"],),
		 2209 => (exclusive = "spwi209.spl",), # luck
		 2212 => (exclusive = "spwi212.spl",), # mirror image
# 		 2212 => (exclusive = ["spwi323.spl"],),
		 2213 => (exclusive = "spwi213.spl",), # stinking cloud
		 2215 => (exclusive = "spwi215.spl",), # web
		 2217 => (exclusive = "spwi217.spl",), # aganazzar's missiles
		 2224 => (exclusive = "spwi224.spl",), # glitterdust
		 2251 => (exclusive = "cdideca.itm",), # decastave
		 2305 => (exclusive = ["msectype.2da", "spwi312.spl", "potn14.spl", "spwi305.spl"],), # haste/slow
		 2324 => (exclusive = ["spwi324.spl", "spwi234d.spl", "spwi720.spl"],), # hold/control undead
		 2413 => (exclusive = ["spwi413a.spl","spwi413d.spl"],), # otiluke
		 2418 => (exclusive = ["spwi418.spl", "spwi403.spl"],), # fire shields
		 2451 => (exclusive = ["shades.2da"],), # shades/shadow monsters
		 2518 => (exclusive = "spwi518.spl",), # phantom blade
		 2523 => (exclusive = "spwi523.spl",), # sunfire
		 2708 => (exclusive = "spwi708.spl",), # mantle
		 2808 => (exclusive = "sppr603.spl",), # moment of prescience
		 2811 => (exclusive = ["spwi811.spl", "scrl9f.itm"],), # symbol:fear
#  2811 => (exclusive = ["spwi706.spl"],),
# 		 2914 => (exclusive = "spwi914.spl",), # black blade of disaster
 2914 => (exclusive = ["spwi914.spl", "spwi806.spl"],),
 2915 => (exclusive = ["spwi806.spl"],),
		 2916 => (exclusive = "spwi916.spl",), # shapechange
 2916 => (exclusive = ["spwi806.spl"],),
		 2923 => (exclusive = "plangood.cre",), # planetar
		 3010 => (exclusive = "ring36.itm",), # ring of danger sense
		 3020 => (exclusive = "brac18.itm",), # gloves of missile snaring
 3020 => (exclusive = ["7eyes.2da"],),
		 3030 => (exclusive = ["pfirea.bam", "pfirex.bam"],), # exploding weapons
#  3030 => (exclusive = ["b_pfire.pro", "pfirea.bam", "pfirex.bam", "#prfire.vvc", "#eff_p45.wav", "#are_p03.wav"],),
		 3040 => (exclusive = "leat14.itm",), # skin of the forest armor
 3040 => (exclusive = ["eff_e02.wav"],),
		 3060 => (exclusive = "boot12.spl",), # cloak of the gargoyle
		 3070 => (exclusive = ["hamm06.itm", "hamm06a.itm", "hamm06b.itm"],),
		 3080 => (exclusive = ["misc3n.itm", "misc3o.itm"],), # instruments
		 3090 => (exclusive = "misc89.itm",), # edwin's amulet
		 3100 => (exclusive = "clck15.itm",), # cloak of the weave
	) #»»
	setmod!("divine_remix",#««
		100 => (exclusive = "mxsplprs.2da",),
		109 => (exclusive = "Tempus",),
	)#»»
	setmod!("faiths_and_powers",#««
		"" => (after = ["divine_remix", "item_rev", "iwdification", "monasticorders", "spell_rev", "tomeandblood" ],
			before = ["might_and_guile", "scales_of_balance", "stratagems", "cdtweaks"],),
		# TODO
	)#»»
	setmod!("eet_tweaks",#««
		2000 => (exclusive = "xplevel.2da",),
		2001 => (exclusive = "xplevel.2da",),
		2002 => (exclusive = "xplevel.2da",),
		2003 => (exclusive = "xplevel.2da",),
		2040 => (exclusive = "xpbonus.2da",),
		2041 => (exclusive = "xpbonus.2da",),
		2042 => (exclusive = "xpbonus.2da",),
		2043 => (exclusive = "xpbonus.2da",),
		2044 => (exclusive = "xpbonus.2da",),
		4000 => (exclusive = "Tempus",),
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
		380 => (exclusive = ["Oasis", "amtarc01.cre", "amtmag01.cre", "amtcle01.cre", "amtcap01.cre", "amtpik01.cre"],),
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
	0 => (exclusive = ["hlolth.itm", "clolth.itm", "amul01.itm", "amul01.spl", "arow01.itm", "ax1h01.itm", "blun01.itm", "bolt01.itm", "sahbolt.itm", "kuobolt.itm", "boot01.itm", "bow01.itm", "brac01.itm", "bull01.itm", "chan01.itm", "clck01.itm", "dagg01.itm", "dart01.itm", "dwblun01.itm", "dwbolt01.itm", "dwchan01.itm", "dwclck01.itm", "dwhalb01.itm", "dwplat01.itm", "dwshld01.itm", "dwsper01.itm", "dwsw1h01.itm", "dwxbow01.itm", "halb01.itm", "hamm01.itm", "helm01.itm", "amsoul01.itm", "leat01.itm", "aegis.itm", "bruenaxe.itm", "bruenpla.itm", "cattibow.itm", "catliowp.cre", "figlion.itm", "spidfgsu.cre", "figspid.itm", "bsw1h01.itm", "bersersu.cre", "bleat01.itm", "miscbc.itm", "nebdag.itm", "quiver01.itm", "reaver.itm", "korax01.itm", "nparm.itm", "npbow.itm", "npbelt.itm", "npchan.itm", "npclck.itm", "npmisc1.itm", "npstaf.itm", "npplat.itm", "keldorn.spl", "npring01.itm", "npshld.itm", "npsw01.itm", "clolth.itm", "hlolth.itm", "finsarev.itm", "plat01.itm", "rods01.itm", "rods01.spl", "shld01.itm", "slng01.itm", "sper01.itm", "staf01.itm", "smoundsu.cre", "smoundsu.itm", "sw1h01.itm", "xbow01.itm", "waflail.itm", "wawak.itm"],),
 1030 => (exclusive = ["chrmodst.2da", "repmodst.2da"],),
	; class = "Early")#»»
	setmod!("metweaks",#««
		1000 => (exclusive = "7eyes.2da",), # monk deflect missile
		1200 => (exclusive = "spcl922.spl",), # ranger tracking
		1400 => (exclusive = "spcl311.spl",), # ranger charm animal
		1500 => (exclusive = "spcl212.spl",), # paladin detect evil
		1600 => (exclusive = "spcl131.spl",), # wizard slayer magic resistance
		1800 => (exclusive = "backstab.2da",), # assassin backstab
		2000 => (exclusive = [ "scrl1$i.itm" for i in 1:7 ],), # cursed scrolls
		3150 => (exclusive = "bdbelhif.cre",), # Belhifet
		3200 => (exclusive = "weapprof.2da/styles",), # figthing styles
	)#»»
	setmod!("might_and_guile",#««
		"" => (before = ["refinements", "stratagems"],),
	),#»»
	setmod!("npckit",#««
		
	)#»»
	setmod!("rr",#««
		"" => (after = ["refinements", "item_rev", "eetact2", "song_and_silence", "divine_remix", "tod", "spell_rev", "ctb", "d0questpack", "beyond_the_law", ],
			before = ["stratagems", "atweaks", "eet_tweaks", "virtue"],
			conflicts = ["iispellsystemadjustments"],),
		0 => (exclusive = ["weapprof.2da"],),
		1 => (exclusive = ["clabth02.2da", "clabth04.2da", "clabth03.2da"],), # assassin ability table
		2 => (exclusive = ["luth0.2da", "luth1.2da", "lumt0.2da", "luct0.2da", "luft0.2da"],),
		3 => (exclusive = ["skillrac.2da"],),
		4 => (exclusive = ["clabba01.2da", "clabba04.2da", "clabba03.2da"],),
		5 => (exclusive = ["luba0.2da", "luba2.2da", "luba3.2da"],),
		6 => (exclusive = ["mxsplbrd.2da"],),
		9 => (exclusive = ["potn36.itm", "potn39.itm"],),
		10 => (exclusive = ["potn36.itm", "potn39.itm"],),
		12 => (exclusive = ["c6arkan.cre", "c6arkan3.cre", "c6kach.cre", "c6yean.cre", "c6arkan.bcs", "stguard1.cre", "mook02.cre", "arkanisg.cre", "mookft01.cre", "palern.cre", "ar0300.bcs", "stguard1.bcs", "aran.cre", "gaelan.cre", "mook.cre", "booter.cre"],),
	)#»»
	setmod!("scales_of_balance",#««
		"" => (conflicts = ["kit_rev", "kitpack",],
			after = ["cdtweaks", "item_rev", "kitpack", "tomeandblood",
				"might_and_guile"],),
		100 => (exclusive = ["dexmod.2da", "skilldex.2da"],),
		101 => (exclusive = "masterwork_weapons",),
		122 => (conflicts = ["rr"],
			exclusive=["universal_clubs", "weapprof.2da", "thac0.2da"],),
		124 => (exclusive = ["weapprof.2da/styles"],),
		160 => (exclusive = ["savemonk.2da", "saveprs.2da", "saverog.2da",
			"savewar.2da", "savewiz.2da"],),
		170 => (exclusive = ["clabth01.2da"],), # evasion
		200 => (exclusive = ["strmod.2da", "strmodex.2da", "intmod.2da", "savecndh.2da", "savecng.2da"],), # stat bonuses
		201 => (exclusive = ["splprot.2da","mxsplwiz.2da"],),
		205 => (exclusive = ["hpconbon.2da", "hpbarb.2da", "hpcm.2da", "hpct.2da", "hpfc.2da", "hpfm.2da", "hpfmt.2da", "hpmt.2da", "hpmonk.2da", "hpprs.2da", "hpprog.2da", "hpwar.2da", "hpwiz.2da"],),
		210 => (exclusive = ["xplevel.2da", "lunumab.2da", "mxspldru.2da",],),
	)#»»
	setmod!("spell_rev",#««
		"" => (after = ["ub",],),
		0 => (exclusive = ["elemtype.itm","mstone.itm", "shille.itm", "shille2.itm", "shille3.itm", "spcl213.spl", "spcl721.spl", "spcl722.spl", "spdr101.spl", "spdr201.spl", "spdr301.spl", "spdr401.spl", "spdr501.spl", "spdr601.spl", "spentaai.bam", "spentaci.bam", "spin101.spl", "spin102.spl", "spin103.spl", "spin104.spl", "spin105.spl", "spin106.spl", "spin113.spl", "spin683.spl", "spin701.spl", "spin788.spl", "spin789.spl", "spmagglo.bam", "spmagglo.vvc", "sppr101.spl", "sppr102.spl", "sppr103.spl", "sppr104.spl", "sppr105.spl", "sppr106.spl", "sppr107.spl", "sppr108.spl", "sppr109.spl", "sppr110.spl", "sppr111.spl", "sppr113.spl", "sppr116.spl", "spra301.spl", "spra302.spl", "spra303.spl", "spra304.spl", "spra305.spl", "spra306.spl", "spwi977.spl", "spwi978.spl", "undtype.itm", "vermtype.itm"],),
		10 => (exclusive = ["plangood.cre", "planevil.cre", "devagood.cre", "devaevil.cre"],),
		65 => (exclusive = ["spcl900.spl","spcl901.spl","spcl907.spl","spwish12.spl"],),
	; class = "Early")#»»
	setmod!("stratagems",#««
		"" => (after = ["item_rev", "d0questpack", "ascension", "refinements",
			"spell_rev", "tactics", "wheels", "eetact2"],
			before = ["cdtweaks"],),
		2110 => (exclusive = "spcl231.spl",), # inquisitor dispel
		2111 => (exclusive = "spcl231.spl",), # inquisitor dispel
		3010 => (exclusive = "masterwork_arrows",),
		3020 => (exclusive = "masterwork_weapons",),
		3021 => (exclusive = "masterwork_weapons",),
		3022 => (exclusive = "masterwork_weapons",),
		4030 => (exclusive = ["spcl643.spl", "spcl644.spl", "spcl611.spl", "spcl612.spl"],),
		4120 => (exclusive = "NPC_at_inn",),
		4230 => (exclusive = ["wmart1.cre", "wmart2.cre"],), # Joluv, Deidre
		5020 => (exclusive = "cloak_of_displacement",),
		5030 => (exclusive = "cloak_of_mirroring",),
		5900 => (exclusive = "AI",),
		6200 => (exclusive = ["spidsw.cre", "spidhu.cre", "spidph.cre"],),
		6300 => (exclusive = ["sirspell.bcs", "hama.bcs",],),
		6310 => (exclusive = ["crawler.bcs",],),
		6320 => (exclusive = ["lbasilsk.bcs", "gbasilsk.bcs",],),
		6500 => (exclusive = ["golcly01.cre", "golsto01.cre", "goliro01.cre", "golice01.cre", "golbra01.cre",],),
		6510 => (exclusive = ["smarter_demons", "demfig01.cre", "demfig02.cre", "dembal01.cre", "dembal02.cre", "demglab.cre", "demglab2.cre", "glabrez.cre", "tanari.cre", "tanari2.cre", "marilith.cre", "demsuc.cre", "pwarden.cre", "dembal01.bcs", "demglab.bcs", "tanari.bcs", "cornugon.bcs", "erinyes.bcs"],),
		6520 => (exclusive = "efreet01.bcs",),
		6540 => (exclusive = ["dragred1.itm", "firkra02.cre", "gorsal.cre", "fsdragon.cre", "dragblac.cre", "udsilver.cre", "shadra01.cre", "dragblue.cre", "dragblac.bcs", "dragbrow.bcs", "dragred.bcs"],),
		6550 => (exclusive = ["beheld01.cre", "udelder.cre", "hlvaxal.cre", "hlhive.cre", "behold01.bcs", "behdir01.bcs", "gauth01.bcs"],),
		6560 => (exclusive = ["udmaster.cre", "flayer01.bcs", "flayer02.bcs", "flayun.bcs"],),
		6570 => (exclusive = ["gith01.bcs", "gith01.cre", "gorgit.cre"],),
		6580 => (exclusive = ["vampir01.bcs", "nevm4.bcs", "vampm01.cre", "vampjah.cre", "lassal.cre", "c6valen.cre", "bodvam01.cre",],),
		6590 => (exclusive = ["finmel.bcs", "meliss01.bcs",],),
		6800 => (depends = "ascension", exclusive = ["illasera.bcs"],),
		6810 => (depends = "ascension", exclusive = ["jumjum.bcs", "fingrom.bcs"],),
		6820 => (depends = "ascension", exclusive = ["yagaft.bcs", "finyaga.bcs"],),
		6830 => (depends = "ascension", exclusive = ["abaz2.bcs", "drake.bcs"],),
		6840 => (depends = "ascension", exclusive = ["finsend.bcs", "finsend.cre", "finiren.cre"],),
		6850 => (depends = "ascension", exclusive = ["finbalor.bcs", "finaluf.bcs", "finmaril.bcs", "finnabas.bcs"],),
		7000 => (exclusive = ["noblpa.bcs", "doppss.bcs", "zorl.bcs", "sardopp.bcs"],),
		7010 => (exclusive = ["ronelit.cre", "ronguar.cre", "irongu.cre"],),
		7020 => (exclusive = ["lamalh.cre", "molkar.cre", "maneir.cre", "telka.cre", "zeela.cre", "drakar.cre", "halaca.cre", "morvin.cre", "drakar.bcs", "halaca.bcs", "morvin.bcs", "molkar.bcs", "maneir.bcs", "lamalh.bcs"],),
		7030 => (exclusive = ["kobolda.cre", "kobold.cre",],),
		7040 => (exclusive = ["grema_d.cre", "ogrema.cre",],),
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
	setmod!("therune",#««
		"" => (after = "cowledmenace",),
	)#»»
	setmod!("tnt",#««
		"" => (after = ["ctb",],),
		0 => (exclusive = ["fmcat.cre", "fmmep.cre", "fmfae.cre", "fmfer.cre", "fmimp.cre", "fmqua.cre", "fmrab.cre", "famps.cre", "fmspd.cre", ],), # familiars
		45 => (exclusive = "spwi609.spl",), # true sight
		46 => (exclusive = ["spwi413a.spl","spwi413d.spl"],), # otiluke
		48 => (exclusive = ["spwi515.spl", "spwi609.spl", "spcl505.spl"],), # true sight
		50 => (exclusive = "spwi703.spl",), # project image
		51 => (exclusive = "spwi607.spl",), # mislead
		56 => (exclusive = "taerom_ankhegs",),
		53 => (exclusive = "store_prices",),
	)#»»
	setmod!("tomeandblood",#««
		"" => (after = ["spell_rev"],),
		11 => (exclusive = ["spwi607.spl", "spwi703.spl", "spwi804.spl"],), # simulacrum
		14 => (exclusive = ["spwi203.spl", "spwi224.spl", "sppr309.spl", "spwi322.spl", "scrl6k.itm", "spwi224.spl", "spwi515.spl", "spwi609.spl"],), # invisibility etc.
		16 => (exclusive = "spwi110.spl",), # identify
		48 => (exclusive = "armored_spellcasting",),
		66 => (exclusive = "scrl77.itm",), # find familiar scroll (removed)
		67 => (exclusive = "select_familiar",),
		69 => (exclusive = "familiar_penalty",),
		71 => (exclusive = "spell_switching",),
	)#»»
	setmod!("wheels",#««
		"" => (before = "stratagems",),
	),#»»
	# Modify a few mod classes ««
		setmod!("dlcmerger"; class="DlcMerger")
		setmod!("eetact2"; class="Quests")
		setmod!("azengaard"; class="Quests")
		setmod!("impasylum"; class="Quests")
		setmod!("imnesvale"; class="Quests")
		setmod!("the_horde"; class="Quests")
		setmod!("butchery"; class="Quests")
		setmod!("turnabout"; class="Quests")
		setmod!("eet_end"; class="Final")
# 		setmod!("might_and_guile"; class="Tweaks")
		# »»
		# update `lastupdate` field««
		for mod in moddb
			isdir(joinpath(MODS, mod.id)) && lastupdate!(mod)
			for e in (".tar.gz", ".zip", ".7z", ".rar")
				a = mod.id*e
				isfile(joinpath(DOWN, a)) && (mod.archive = a; break)
			end
		end#»»
end#»»
# ««1
end

M=ModTool
