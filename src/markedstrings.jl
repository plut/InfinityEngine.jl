module MarkedStrings
#««2 Marked strings type
struct MarkedString{S<:AbstractString}; str::S; end
@inline Base.show(io::IO, s::MarkedString) = print(io, '_', repr(s.str))
"""    _"text"
A marker for text that needs to be translated.
This does nothing by itself (it is equivalent to `"text"`),
but it marks the string for parsing to a `".pot"` file.

## Comments

Any consecutive block of comments of up to 5 lines just before the string
is appended to the `".pot"` file. These comments will thus be visible
to any translators. Don't hesitate to use them to write useful context.

## String markers

Strings may contain markers of the form `?{marker}`.
Such markers are discarded before saving the string to game files.
However, they will be included in strings marked for translation.
They can therefore be used to distinguish two strings which are identical
in the original language (e.g. English) but which could have
differing translations depending on language;
for example, using "?{verb}Slow" and "?{adjective}Slow" could be helpful.

## `"dialogF.tlk"`

Several languages (German, Spanish, French, Japanese, Polish, Portuguese,
Russian) have a second translation file, `"dialogF.tlk"`,
which is used when <CHARNAME> has female grammatical gender.
Most strings are identical for both genders (for example in French,
only 800 of 34000 strings differ).
To make it easier to provide correct strings depending on grammatical gender,
any string in the source file marked with `"?{F}"` or `"?{M}"`
will output *two* variants of the string in the `".pot"` file:
one with the the mark replaced by `"?{M}"`
(translation will be saved in `"dialog.tlk"`)
and one with the mark replaced by `"?{F}"`
(translation will be saved in `"dialogF.tlk"`).

It is advised to use such a mark on every sentence
that grammatically refers to <CHARNAME> in any way.
The translator may choose to leave either translation empty.
In this case, the other translation will be used.
(This means that superfluous `"?{[FM}}"` marks demand almost no extra
work from the translator).

A translator seeing a string which would require two translations,
but which is not marked with `"?{[FM]}"` can also edit the `.po` file
to include both versions (however contacting the author to correctly
mark the input string is preferred).
"""
macro __str(s); :(MarkedString($s)) end

remove_comments(s) = replace(s, r"\?\{[^}]*\}" => "")
# in structures: saved as Strref
# input by user: can be commented
#  => in new_strings also: can be commented
# comparison between new_strings: with comments
#    new and game strings: always different
# (to use an original game string, just use Strref(...) directly)
# saving to tlk file removes comments
#««2 Reading marked strings and producing `.pot` file
#XXX find if there would be a better way of structuring comments
#adapted to the syntax of say/reply etc.
struct TranslationEntry
	file::Symbol
	line::Int
	msgid::String
	comment::String
end

"""    walk_tree(expr) do; code...; end

Recursively evaluates `code` on an `Expr` object. No return value."""
function walk_tree(f, expr, comment = "")
	f(expr, comment)
	expr isa Expr || return
	i = 1
	if expr.head == :call  && expr.args[1] != :(=>)
		fname = expr.args[1]
		comment = fname ∈ (:say, :reply, :interject) ?
			"\n#. in function: $fname" : ""
		i = 2
	elseif expr.head == :(=) && Meta.isexpr(expr.args[1], :(.))
		propname = expr.args[1].args[2].value
		comment = "\n#. object property: $propname"
	end
	for a in expr.args[i:end]; walk_tree(f, a, comment); end
end

"""    translations(file)

Returns the list of all strings to translate in this file and all
included files, as a `Vector{TranslationEntry}`.
Comments may be used to document strings: all comment lines (up to 5 lines)
immediately preceding the string will be used.
"""
function translations(file, list = TranslationEntry[],
		seen_files = Symbol[])
	sym = Symbol(file); sym ∈ seen_files && return
	lines = readlines(file)
	expr = Meta.parse(join(["quote"; lines; "end"], '\n')).args[1]
	walk_tree(expr) do node, comment
		if Meta.isexpr(node, :macrocall) && node.args[1]==Symbol("@__str")
			line, str = node.args[2].line-1, node.args[3]
			# Look for translator comments in the 5 preceding lines at most.
			for l in lines[line-1:-1:max(line-5,1)]
				m = match(r"^\s*#\s*(\S.*)$", l)
				isnothing(m) && break
				comment = "\n#. " * m.captures[1] * comment
			end
			comment = chomp(comment)
			mark = r"\?\{[FM]\}"
			if contains(str, mark)
				# Include both gender-marked strings in .pot:
				push!(list,
					TranslationEntry(sym, line, replace(str, mark => "?{M}"), comment),
					TranslationEntry(sym, line, replace(str, mark => "?{F}"), comment))
			else
				push!(list, TranslationEntry(sym, line, str, comment))
			end
		elseif Meta.isexpr(node, :call) && node.args[1]==:include
			translations(joinpath(dirname(file), node.args[2]), list,
				seen_files ∪ [sym])
		end
	end
	list
end
"""    write_pot(file, output)

Writes a `.pot` translation template from the given file
(and recursively, all included files)."""
function write_pot(input, output::IO)
	write(output, """
# Strings extracted from: $input
# Use this template file to create per-language `.po` files in this way
# (example for French language, substitute as needed:)
#
# msginit -i file.pot -l fr
#
# Edit .po file with (for example) poedit fr.po

msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8"
""")
	for t in translations(input)
		print(output, """
$(t.comment)
#: $(t.file):$(t.line)
msgid $(repr(t.msgid))
msgstr ""
""")
	end
end
write_pot(input, output) = open(io->write_pot(input, io), output, "w")
#««2 Reading `.po` file as a dictionary
@inline if_match(f, args...) = (m = match(args...); m ≠ nothing && (f(m); true))
function read_po!(dict::AbstractDict, io::IO)
	mode = :nothing
	msgid, msgstr = "", ""
	for line in io|>eachline
		if_match(r"""^msgid\s+"(.*)"$""", line) do m
			(mode == :msgstr) && (dict[msgid] = msgstr; mode = :nothing)
			msgid = m.captures[1]|>unescape_string
			mode = :msgid
		end && continue
		if_match(r"""^msgstr\s+"(.*)"$""", line) do m
			msgstr = m.captures[1]|>unescape_string
			mode = :msgstr
		end && continue
		if_match(r"""^\s*"(.*)"$""", line) do m
			s = m.captures[1]|>unescape_string
			mode == :msgid && (msgid*= s; return)
			mode == :msgstr && (msgstr*= s; return)
			error("unexpected string: $line")
		end && continue
		if !isempty(line) && !startswith(line, r"^\s*#")
			error("unknown line content: $line")
		end
		(mode == :msgstr) && (dict[msgid] = msgstr; mode = :nothing)
	end
	return dict
end
@inline read_po!(dict, filename::AbstractString) =
	open(io->read_po!(dict, io), filename)
@inline read_po(io) = read_po!(Dict{String,String}(), io)
#»»1
export MarkedString, @__str
end # module
