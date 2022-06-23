# 12 `Transaction`

Le but d'une `Transaction` est d

e lié des traiatements ensemble dans leur réussite ou leur échec.



## Créer une `Transaction`

```cs
using var context = new CookBookContext();

context.Dishes.Add(new Dish { Title = "Super Burger", Notes = "With garlic and dragon herb"});
await context.SaveChangesAsync();

await context.Database.ExecuteSqlRawAsync("SELECT 1/0 AS Bad");
```

À ce stade, malgré l'échec (l'erreur) de la deuxième requête, la première est bien enregistrée en `BDD`.



### `BeginTransactionAsync`, `CommitAsync`

On trouve le nécessaire pour créer des `Transactions` dans `context.Database` :

```cs
using var context = new CookBookContext();
using var transaction = await context.Database.BeginTransactionAsync();

try
{
    context.Dishes.Add(new Dish { 
        Title = "Super Burger", 
        Notes = "With garlic and dragon herb"
    });
    await context.SaveChangesAsync();

    await context.Database.ExecuteSqlRawAsync("SELECT 1/0 AS Bad");
    await transaction.CommitAsync();
}
catch (Exception ex)
{
    Console.Error.WriteLine(ex.Message);
}
```

La nouvelle donnée n'est jamais insérée en `BDD`.