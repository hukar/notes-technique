# 09 `Attach` entities

On voudrait pouvoir dire à `EF Core` de ne plus tracké une entité, de l'oublié.

### Fait comme si tu n'avais jamais vu cet objet avant !

```cs
using var context = new CookBookContext();
var dish = new Dish { Title = "FooFoo", Notes = "Baz" };

context.Dishes.Add(dish);

await context.SaveChangesAsync();
```



## Oublié une `entity`

On va écrire le `state` d'une `Entry` et non le lire :

```cs
context.Entry(dish).State = EntityState.Detached;
```



## Tracké un objet inconnu

```cs
context.Dishes.Add(dish);
var state = context.Entry(dish).State; // Added

await context.SaveChangesAsync();
state = context.Entry(dish).State; // Unchanged

context.Entry(dish).State = EntityState.Detached;
state = context.Entry(dish).State; // Detached

dish.Title = "Momo Taro";
state = context.Entry(dish).State; // Detached

context.Dishes.Update(dish);
state = context.Entry(dish).State; // Modified

await context.SaveChangesAsync();
state = context.Entry(dish).State; // Unchanged
```

Si un objet est reçu dans une `API Web` par exemple et qu'il n'est pas connu par `EF Core`, on peut utiliser la méthode `Update` :

```cs
context.Dishes.Update(dish);
```

```sql
UPDATE [Dishes] SET [Notes] = @p0, [Stars] = @p1, [Title] = @p2
WHERE [Id] = @p3;
SELECT @@ROWCOUNT;
```

`Update` track l'entité et met son `state` à `Modified`.

#### `Update`, tout comme `Add`, `Attach` l'entité au `Change Tracker`.

Après un `SaveCnagesAsync` on voit que tous les champs sont écrasés, qu'ils soient ou non différent de la valeur en `BDD`.

`EF Core` ne connaissant pas cet objet avant `Update`, il écrase complètement l'enregistrement en `BDD`.

Si l'`Id` n'est pas spécifié dans l'objet passé à `Update`, il sera marqué `Added` plutôt que `Modified`.