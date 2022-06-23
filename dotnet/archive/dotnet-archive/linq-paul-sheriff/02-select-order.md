# 02 `Select` et `Order`

> `partial` permet de définir une classe ( ou une méthode) en plusieurs parties assemblées à l'exécution :
>
> ```cs
> partial class Bot
> {
>     public string Name { get; set; }
> 
>     public Bot(string name)
>     {
>         Name = name;
>     }
> }
> 
> partial class Bot
> {
>     public void SayHello()
>     {
>         WriteLine($"hello I'm {Name}");
>     }
> }
> ```

## Remplir une `list` avec `foreach`

```cs
List<Product> list = new List<Product>();

foreach (var item in Products)
{
    list.Add(item);
}
```



## Remplir une `list` avec `Select`

#### `query`

```cs
list = (from prod in Products select prod).ToList();
```

#### `Method`

```cs
list = Products.Select(prod => prod).ToList();
```



## sélectionner une colonne

> `Environment.Newline`  renvoie `\r\n` pour les plateformes non-Unix, ou `\n` pour les plateformes Unix.

```cs
list.AddRange(from prod in Products select prod.Name)
```

```cs
list.AddRange(Products.Select(prod => prod.Name));
```

Pour nettoyer une `List` : `Clear()`

```cs
Products.Clear();
```



## Sélectionner des colonnes spécifiques

```cs
Products = (from prod in Products
           	select new Product
            {
                ProductID = prod.ProductID,
                Name = prod.Name,
                Size = prod.Size
            }).ToList();
```

```cs
Products = Products.Select(prod => new Product {
    ProductID = prod.ProductID,
    Name = prod.Name,
    Size : prod.Size
}).ToList();
```



## Utilisation d'une classe anonyme `new { ... }`

```cs
// Query Syntax
var products = (from prod in Products
                select new
                {
                    Identifier = prod.ProductID,
                    ProductName = prod.Name,
                    ProductSize = prod.Size
                }).ToList();
```

```cs
// Method Syntax
var products = Products.Select(prod => new
                               {
                                   Identifier = prod.ProductID,
                                   ProductName = prod.Name,
                                   ProductSize = prod.Size
                               });
```



## Ordonner les données : `OrderBy`

```cs
// Query Syntax
Products = (from prod in Products
            orderby prod.Name
            select prod).ToList();
```

```cs
// Method Syntax
Products = Products.OrderBy(p => p.Name).ToList();
```

Avec la méthode `OrderBy`, on peut se passer d'utiliser `Select`.

Par défaut l'ordre est ascendant.



## `OrderByDescending`

```cs
// Query Syntax
Products = (from prod in Products
            orderby prod.Name descending
            select prod).ToList();
```

```cs
// Method Syntax
Products = Products.OrderByDescending(p => p.Name).ToList();
```



## Ordonner par plusieurs champs: `ThenBY`, `ThenByDescending`

```cs
// Query Syntax
Products = (from prod in Products
            orderby prod.Color descending, prod.Name
            select prod).ToList();
```

Il suffit de séparer les champs par une virgule (`coma`).

Le mot clé `ascending` existe mais c'est la valeur par défaut.

```cs
// Method Syntax
Products = Products
    .OrderByDescending(p => p.Color)
    .ThenBy(p => p.Name).ToList();
```

Il existe aussi un `ThenByDescending`.
