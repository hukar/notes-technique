# 06 `Entity Framework`



## Installation

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer

dotnet tool update --global dotnet-ef

dotnet add package Microsoft.EntityFrameworkCore.Design
```

Ou 

```cs
dotnet add package Microsoft.EntityFrameworkCore.Sqlite
```

Pour `Sqlite`.

#### ! Bien choisir la version compatible `--version`

```bash
# Par exemple
dotnet add package Microsoft.EntityFrameworkCore.SqlServer --version 6.0.0-rc.2.21480.5

dotnet tool update --global dotnet-ef --version 6.0.0-rc.2.21480.5

dotnet add package Microsoft.EntityFrameworkCore.Design --version 6.0.0-rc.2.21480.5
```



## `Connexion String` pour `SQL Server`

Il faut d'abord renseigner le `connection string` dans `appsettings.json`.

```json
"ConnectionStrings": {
        "HukarConnection": "Server=localhost,1433;Database=MinimalAPITest;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true"
    },
```



## `Connexion String` pour `Sqlite`

```cs
"ConnectionStrings": {
    "SqliteConnection": "DataSource=MinimalApi.db"
}
```





## Connexion à la `DB`

Pour récupérer notre `ConnectionString` de `appsettings.json` on utilise `builder.Configuration`.

```cs
builder.Configuration["ConnectionStrings:HukarConnection"]
  // ou
builder.Configuration.GetConnectionString("HukarConnection")
```



On ajoute notre service dans `Program.cs` :

```cs
// GET CONFIGURATION
var connectionString = builder.Configuration.GetConnectionString("HukarConnection");

// ADDING SERVICES
builder.Services.AddDbContext<MinimalContext>(
  options => options.UseSqlServer(connectionString)
);
```



### Connexion à `SQLite`

```cs
var connectionString = builder.Configuration.GetConnectionString("SqliteConnection");

builder.Services.AddDbContext<JobSiteDb>(
    options => options.UseSqlite(connectionString)
);
```



## `DbSet` non `Nullable`

Pour écrire ses `DbSet` sans générer un `warning` : `must contain a non-null value`, on initialise les `DbSet` de cette façon : `Set<MyEntity>()`

```cs
public DbSet<Vehicle> Vehicles => Set<Vehicle>();
public DbSet<Colour> Colours => Set<Colour>();
```



## Passer `DbContextOptions`

Quand on utilise l'injection de dépendance :

```cs
builder.Services.AddDbContext<MinimalContext>(
  options => options.UseSqlServer(connectionString)
);
```

On a besoin d'avoir un constructeur qui passe les `options` à la classe de base :

```cs
public class MinimalApiContext : DbContext
{
    // On passe les options ici
    public MinimalApiContext(DbContextOptions<MinimalApiContext> options) 
        : base(options) { }

    public DbSet<Customer> Customers => Set<Customer>();
    public DbSet<Hobby> Hobbys => Set<Hobby>();
}
```



## `Diagnostics.EntityFrameworkCore`

C'est un package d'`EF Core` permettant d'afficher les erreurs en `BDD`.

```bash
dotnet add package Microsoft.AspNetCore.Diagnostics.EntityFrameworkCore
```

Après le `AddDbContext` on ajoute `AddDatabaseDevelloperPageExceptionFilter` :

```cs
builder.Services
    .AddDbContext<MinimalContext>(
        options => options.UseSqlServer(connectionString)
    )
	.AddDatabaseDeveloperPageExceptionFilter();
```

