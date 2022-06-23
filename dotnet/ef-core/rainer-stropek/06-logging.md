# 06 `Logging`



## Le plus simple

(testé avec `Blazor Server`)

Dans `appsettings.development.json`

```json {6}
"Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft": "Warning",
      "Microsoft.Hosting.Lifetime": "Information",
      "Microsoft.EntityFrameworkCore.Database.Command": "Information"
    }
  }
```





## Insatallation du `package` nécessaire

```bash
dotnet add package Microsoft.Extensions.Logging.Console --version 7.0.0-preview.1.22076.8
```

(Déjà présent dans un projet `asp.net`)



## Ajout à notre `DbContext`

Dans `MyApplicationDbContext`, dans la méthode `OnConfiguring` :

```cs
protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
{
    // ...
    optionsBuilder
        .UseLoggerFactory(
        	LoggerFactory.Create(builder => builder.AddConsole())
    	)
        .UseSqlServer(...);
}
```

<img src="assets/Screenshot%202022-02-18%20at%2010.09.35.png" alt="Screenshot 2022-02-18 at 10.09.35" style="zoom:50%;" />

On obtient maintenant les renseignement sur la traduction `SQL` de la requête.

```cs
var dishesQuery = context.Dishes.Where(d => d.Title!.Contains("Pizza"));
var dishes = await dishesQuery.ToListAsync();
```

On observe que `Contains("Pizza")` a été traduit par `LIKE N"%Pizza%"`.



Si je modifie mon code comme ceci :

```cs
var dishesQuery = context.Dishes.Where(d => d.Title == "Pizza");
var dishes = await dishesQuery.ToListAsync();
```

J'obtiens cette fois ci :

<img src="assets/Screenshot%202022-02-18%20at%2010.16.03.png" alt="Screenshot 2022-02-18 at 10.16.03" style="zoom:80%;" />





## Manière alternative d'ajouter les `logs` à la `Console`

```cs
protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
{ 
    optionsBuilder
        .LogTo(Console.WriteLine, new[] { RelationalEventId.CommandExecuted 		 })
        .EnableSensitiveDataLogging()
        .UseSqlServer(
        // ...
    );
}
```

`EnableSensitiveDataLogging` affiche la valeur des paramètres plutôt qu'un point d'interrogation dans les requêtes.

```sql
Executed DbCommand (38ms) [Parameters=[@p0='?' (Size = 4000), @p1='?' (Size = 4000), @p2='?' (Size = 4000), @p3='?' (Size = 4000), @p4='?' (Size = 4000), @p5='?' (Size = 4000)], CommandType='Text', CommandTimeout='30']
```

Et avec `EnableSensitiveDataLogging`  :

```sql
 Executed DbCommand (41ms) [Parameters=[@p0='EF Lovers' (Nullable = false) (Size = 4000), @p1='Pizza Crazy' (Nullable = false) (Size = 4000), @p2='microsofties' (Nullable = false) (Size = 4000), @p3='arthur' (Nullable = false) (Size = 4000), @p4='wendy' (Nullable = false) (Size = 4000), @p5='georges' (Nullable = false) (Size = 4000)], CommandType='Text', CommandTimeout='30']
```













