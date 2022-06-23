# 01 Le `Model`



`Entity Framework` est basé sur des conventions de nommage.



## Création d'un `Model`

Le `modèle` est l'ensemble des classes qui seront représentées par des tables en `BDD`.

### `Dish.cs`

```cs
public class Dish
{
    public int Id { get; set; }
```

Par convention une propriété de type `int` et nommée `Id` sera considérée comme `PK` (clé primaire) et remplie automatiquement en `BDD`.

```cs
	public string Title { get; set; } = string.Empty
```

cette propriété (colonne en `BDD`) sera considérée comme `Non Nullable`.

Si on veut une propriété (colonne) `Nullable` en `BDD` il faut écrire `string?` :

```cs
	public string? Title { get; set; }
```

Ceci est possible grâce à l'indication dans `MyProject.csproj` :

```xml
<Nullable>enable</Nullable>
```

On peut ajouter une `data annotation` :

```cs
	[MaxLength(100)]
	public string? Title { get; set; }
```

Cela induira un `nvar(100)` en `BDD`. 

```cs
	[MaxLength(100)]
	public string? Notes { get; set; }

	public int? Stars { get; set; }

	public List<DishIngredient> Ingredients { get; set; } = new();
}
```

`?` force une colonne à être optionnelle (`Nullable`).



### `DishIngredient.cs`

```cs
public class DishIngredient
{
    public int Id { get; set; }
    
    [MaxLength(100)]
    public string Description { get; set; } = string.Empty;
    
    [MaxLength(50)]
    public string UnitOfMeasure { get; set; } = string.Empty;
    
    [Column(TypeName = "decimal(5,2)")]
    public decimal Amount { get; set; }
    
    public Dish? Dish { get; set; }
    public int DishId { get; set; }
}
```

Il y a une marge entre la représentation (le type) des données en `c#` et en `BDD`.

Les `annotations` permettent de combler cette marge.

`[Column(TypeName = "decimal(5,2)")]` spécifie le type `decimal` pour `mssql` et donne comme longueur max 5 chiffres avec 2 chiffres derrière la virgule.

On a une relation `1 - n` entre `Dish` et `DishIngredient`.

`public Dish? Dish` est appelé `Navigation Property`

`public int DishId` est la `FK` (clé étrangère) vers `Dish`.