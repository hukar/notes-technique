# 03. `DbContext`

On va créer une classe `DataContext.cs` dans le projet `Persistence`.

Cette classe hérite de `DbContext` d'`EF Core`.

> Une instance de `DbContext` représente une session avec la base de données et peut être utilisée pour  interroger et sauvegarder les instances de vos entités. DbContext est une combinaison des patterns `Unit Of Work` et `Repository patterns`.

```csharp
using Domain;
using Microsoft.EntityFrameworkCore;

namespace Persistence
{
    public class DataContext : DbContext
    {
        public DataContext(DbContextOptions options) : base(options)
        {
        }

        public DbSet<Activity> Activities { get; set; }
    }
}
```

Un `DbSet` représente une table en base de données.

`EF Core` utilise les conventions pour reconnaître le champs `Id` comme `primary key`. On doit donc respecter les conventions de nomade si on veut que `EF Core` soit capable de reconnaître la clé primaire.

On va injecter notre `DbContext` dans le conteneur de service de l'application, il se trouve dans `Startup.cs` :

```cs
public void ConfigureServices(IServiceCollection services)
{
	// ...
  services.AddDbContext<DataContext>(option => {
    option.UseSqlite(/* stand ... */);
  });
}
```

`UseSqlite` prend en paramètre le `connection string`.



### Refactor de la propriété `Configuration`

Avant :

```csharp
public Startup(IConfiguration configuration)
{
  Configuration = configuration;
}

public IConfiguration Configuration { get; }
```

Après :

```csharp
private readonly IConfiguration _config;
public Startup(IConfiguration config)
{
  _config = config;
}
```



### récupération de la `ConnectionStrings`

dans `Startup.cs`

```csharp
services.AddDbContext<DataContext>(option =>
{
  option.UseSqlite(_config.GetConnectionString("DefaultConnection"));
});
```

Et dans `API/appsettings.Development.json` :

```csharp
{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft": "Warning",
      "Microsoft.Hosting.Lifetime": "Information"
    }
  },
  "ConnectionStrings": {
    "DefaultConnection": "Data source=reactivities.db"
  }
}
```

#### ! Ne pas oublier `s` à la fin de `ConnectionStrings`

