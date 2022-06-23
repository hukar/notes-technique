# BB Suppression en `BDD`



## Supprimer une `DB ` : `EnsureDeletedAsync`

```cs
await db.Database.EnsureDeletedAsync();
Console.WriteLine("db is deleted");
```

On peut se passer des `migrations ` avec  :

```cs
using var context = new MyContext();

await context.Database.EnsureDeletedAsync();
await context.Database.EnsureCreatedAsync();
```





Ou en `CLI`

```bash
dotnet ef database drop
```





## Vider une table

```cs
await context.Database.ExecuteSqlRawAsync("DELETE FROM Dishes WHERE Id != -1");
```

`TRUNCATE TABLE Dishes` peut provoquer une `exception` s'il y a des contraintes de `Foreign Key`.