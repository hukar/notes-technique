# 05 Cas d'utilisation de `Linq`

## Retrouver l'`ID Max` : `Max` et `DefaultIfEmpty`

```cs
List<Product> l = new() {new(5, "Titi")};

var maxId = l.Select(p => p.Id).DefaultIfEmpty(-1).Max();
```

```
5
```

```cs
List<Product> l = new();

var maxId = l.Select(p => p.Id).DefaultIfEmpty(-1).Max();
```

```
-1
```

`DefaultIfEmpty` défini une valeur par défaut si le `IEnumerable` précédent est vide.