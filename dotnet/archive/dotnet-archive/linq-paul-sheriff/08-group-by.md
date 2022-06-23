# 08 `GroupBy`



## `GroupBy`

```cs
IEnumerable<IGrouping<string, Product>> sizeGroup;
```

`IGrouping<TKey, TElement>` représente une `collection` d'objets ayant la clé en commun.

Ici la clé `TKey` est la `Size` de type `string`.

```cs
// Query Syntax
sizeGroup = (from p in Products
            orderby p.Size
            group p by p.Size);
```

```cs
// Method Syntax
sizeGroup = Products
    .OrderBy(p => p.Size)
    .GroupBy(p => P.Size);
```

On va itérer sur la clé et sur le groupe de `Product` correspondant à la clé :

```cs
foreach(var group in sizeGroup) {
    WriteLine($"Size : {group.Key} Count : {group.Count()}");
    
    foreach(var p in group)
    {
        WriteLine($"Id = {p.ProductID} Name = {p.Name}");
    }
}
```

