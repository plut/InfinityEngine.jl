push!(LOAD_PATH, "src/", "../src/")
using Documenter
using InfinityEngine
makedocs(
	modules=[InfinityEngine],
	pages = [
		"index.md",
		"items.md",
		"dialogs.md",
	],
	sitename="InfinityEngine.jl",
	format=Documenter.HTML(prettyurls=get(ENV,"CI",nothing) !=nothing),
)

deploydocs(
	repo = "github.com/plut/InfinityEngine.jl.git",
)
