module MarkedStrings
#««2 Marked strings type
struct MarkedString{S<:AbstractString}; str::S; end
@inline Base.show(io::IO, s::MarkedString) = print(io, '_', repr(s.str))
"""    _"text"
A marker for text that needs to be translated.
This does nothing by itself (it is equivalent to `"text"`),
but is parsed to a `.po` file."""
macro __str(s); :(MarkedString($s)) end
remove_comments(s) = replace(s, r"^\?.*\?(?!\?)" => "")
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
	walk_tree(expr) do node
		if Meta.isexpr(node, :macrocall) && node.args[1]==Symbol("@__str")
			line, str = node.args[2].line-1, node.args[3]
			comment = ""
			# Look for translator comments in the 5 preceding lines at most.
			for l in lines[line-1:-1:max(line-5,1)]
				m = match(r"^\s*#\s*(\S.*)$", l)
				isnothing(m) && break
				comment = "\n#. " * m.captures[1] * comment
			end
			push!(list, TranslationEntry(sym, line, str, chomp(comment)))
		elseif Meta.isexpr(node, :call) && node.args[1]==:include
			translations(dirname(file)*node.args[2], list,
				seen_files ∪ [sym])
		end
	end
	list
end

"""    walk_tree(expr) do; code...; end

Recursively evaluates `code` on an `Expr` object. No return value."""
function walk_tree(f, expr)
	f(expr)
	expr isa Expr && for a in expr.args; walk_tree(f, a); end
end

"""    write_pot(file, output)

Writes a `.pot` translation template from the given file
(and recursively, all included files)."""
function write_pot(input, output::IO)
	write(output, """
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
function read_po(io)
	mode = :nothing
	msgid, msgstr = "", ""
	dict = Dict{String,String}()
	for line in io|>eachline
		m = match(r"""^msgid\s+"(.*)"$""", line); if m ≠ nothing
			(mode == :msgstr) && (dict[msgid] = msgstr; mode = :nothing)
			msgid = m.captures[1]|>unescape_string
			mode = :msgid
			continue
		end
		m = match(r"""^msgstr\s+"(.*)"$""", line); if m ≠ nothing
			msgstr = m.captures[1]|>unescape_string
			mode = :msgstr
			continue
		end
		m = match(r"""^\s*"(.*)"$""", line); if m ≠ nothing
			s = m.captures[1]|>unescape_string
			mode == :msgid && (msgid*= s; continue)
			mode == :msgstr && (msgstr*= s; continue)
			error("unexpected string: $line")
		end
		if !isempty(line) && !startswith(line, r"^\s*#")
			error("unknown line content: $line")
		end
		(mode == :msgstr) && (dict[msgid] = msgstr; mode = :nothing)
	end
	return dict
end
#»»1
export MarkedString, @__str
end # module
