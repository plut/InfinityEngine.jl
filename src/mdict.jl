module MultiDicts

"""    MultiDict

A collection of dictionaries, indexed by key type of each dictionary.
Access a given dictionary by `multidict[keytype1]`."""
struct MultiDict{T<:Tuple}
	dict::T
	@inline MultiDict{T}() where{T} = new{T}(((D() for D in T.parameters)...,))
end

@inline Base.iterate(m::MultiDict, state...) = iterate(m.dict, state...)
@inline dictstypes(::Type{<:MultiDict{X}}) where{X} = (X.parameters...,)
@inline fieldindex(M::Type{<:MultiDict}, K::Type) =
	findfirst(T->keytype(T)==K, dictstypes(M))
@inline Base.valtype(M::Type{<:MultiDict}, K::Type) =
	valtype(fieldtype(fieldtype(M,1), fieldindex(M, K)))
@inline Base.getindex(m::MultiDict, K::Type) = m.dict[fieldindex(typeof(m), K)]

"""    @MultiDict{Key1=>Val1, Key2=>Val2}

Defines a `MultiDict` collecting dictionaries `Dict{Key1,Val1}` etc."""
macro MultiDict(args...)
	length(args) == 1 && Meta.isexpr(args[1], :braces) && (args=args[1].args)
	expr = :(MultiDict{Tuple{}}); code = expr.args[2].args
	for (i, expr) in pairs(args)
		@assert Meta.isexpr(expr, :call) && expr.args[1] == :(=>)
		K, V = (Core.eval(__module__, a) for a in expr.args[2:3])
		push!(code, :(Dict{$K,$V}))
	end
	expr
end

export @MultiDict

end
