#««1 Handling translations

"""    _"text"
A marker for text that needs to be translated.
This does nothing by itself (it is equivalent to `"text"`),
but is parsed to a `.po` file."""
macro __str(s); s; end

#««2 Reading marked strings and producing `.pot` file
struct TranslationEntry
	file::Symbol
	line::Int
	msgid::String
	comment::String
end
"""    translations(file)

Returns the list of all strings to translate in this file and all
included files, as a `Vector{TranslationEntry}`."""
function translations(file, list = TranslationEntry[],
		seen_files = Symbol[])
	sym = Symbol(file); sym ∈ seen_files && return
	lines = readlines(file)
	expr = Meta.parse(join(["quote"; lines; "end"], '\n')).args[1]
	walk_tree(expr) do node
		if Meta.isexpr(node, :macrocall) && node.args[1]==Symbol("@__str")
			line, str = node.args[2].line-1, node.args[3]
			comment = ""; c = ""
			# Look for translator comments in the 5 preceding lines at most.
			for l in lines[line-1:-1:max(line-5,1)]
				startswith(l, r"^\s*#") || break
				c = replace(l, r"^\s*#\s*" => " ") *c
				if startswith(c, r"\s*TRANS:")
					comment = replace(c, r"^\s*TRANS:\s*" => "")
					break
				end
			end
			push!(list, TranslationEntry(sym, line, str, comment))
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
		println(output)
		isempty(t.comment) || println(output, "#. ", t.comment)
		print(output, """
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

write_pot("a.jl", "a.pot")
read_po("fr.po")
