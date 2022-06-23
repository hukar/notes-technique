# 10 `No Tracking`



## Récupérer une liste d'entité

```cs
using var context = new CookBookContext();

// SELECT * FROM Dishes
var dishes = await context.Dishes.ToArrayAsync();
```

Pour que la magie `EF Core` opère, toute la liste (le tableau) est tracké par le `Change Tracker`. `EF Core` retient toutes les `Original Value` pour pouvoir effectuer les modification adéquat avec un simple `SaveChangesAsync`.

Mais pour des données en lecture seule, qu'on ne va pas modifier, on ne veut pas forcement garder toutes ces informations en mémoire.

```cs
foreach(var dish in dishes)
{
    state = context.Entry(dish).State; // Unchanged
}
```

Si on regarde le `state` des entités de `dishes`, on verra qu'elles sont toutes à `Unchanged`.



## `AsNoTracking()`

```cs
var dishes = await context.Dishes.AsNoTracking().ToArrayAsync();

foreach(var dish in dishes)
{
    state = context.Entry(dish).State; // Detached
}
```

En ajoutant `AsNoTracking` à la requête, on a maintenant comme `state` pour chacunes de nos entités `Detached`.

### Toujours utiliser `AsNoTracking` pour des scénario en `Read Only`.