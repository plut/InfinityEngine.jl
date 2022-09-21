# Items

```@docs
item
items
InfinityEngine.Item
InfinityEngine.ItemAbility
```

## Item templating

Any item may be used as a template for creating a new item:
```julia
albruin = item("sw1h34")
heavysword = albruin(weight = 500, enchantment = +5)
```

In addition to this, most of the item types are useable as resource
templates. The following items will, when used as resource templates,
produce a basic version of the item:
```@eval
using InfinityEngine
join(sort([" - "*string(k) for k ∈ keys(InfinityEngine._RESOURCE_TEMPLATE)]), "\n")
```
