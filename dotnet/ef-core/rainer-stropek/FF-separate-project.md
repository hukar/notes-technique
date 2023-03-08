# FF Projet Séparé

On peut vouloir mettre tous ce qui concerne les données dans un projet séparé.



## Création d'une `classlib`

```bash
dotnet new classlib -o MyProject.Data
```



## Insatallation des packages

On va installé `EF Core` et la possibilité de gérer les fichiers `appsettings.json`

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer

dotnet add package Microsoft.EntityFrameworkCore.Design

dotnet add package Microsoft.Extensions.Configuration.Json
```

`EntityFrameworkCore.Design` permet les migrations (`code first`).

`Configuration.Json` permet d'utiliser un fichier `appsettings.json` pour la configuration car il n'y en a pas dans une `classlib`.

`appsettings.json`

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning"
    }
  },
  "ConnectionStrings": {
    "MSSqlConnect": "Server=localhost,1433; Database=share-for-future; User=sa; Password=huk@r2Xmen99"
  },  
  "AllowedHosts": "*"
}
```



## Création du `DbContext`

```cs
using Microsoft.EntityFrameworkCore;

public class MyAppContext : DbContext
{
    public MyAppContext(DbContextOptions<MyAppContext> options) : base(options) { }

    public DbSet<User> Users => Set<User>();
    
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfigurationsFromAssembly(typeof(MyAppContext).Assembly);
    }
}
```

Ici les fichiers de configuration sont externe à `OnModelCreating` et `EF Core` scanne l'`assembly` pour les charger (voire fiche `16-fluent-api`).



## Créer les `Migrations`

```bash
dotnet ef migrations add Init
Build started...
Build succeeded.
Unable to create an object of type 'MyAppContext'. For the different patterns supported at design time, see https://go.microsoft.com/fwlink/?linkid=851728
```

En l'état on obtient une erreur, pourtant le package `EntityFrameworkCore.Design` est bien installé.



### `ContextFactory`

On doit utiliser un `ContextFactory` pour que `EF Core` est connaissance du `context` en absence de `Program.cs` et de l'injection de dépendance :

`MyAppContextFactory.cs`

```cs
using Microsoft.EntityFrameworkCore.Design;
using Microsoft.Extensions.Configuration;

class MyAppContextFactory : IDesignTimeDbContextFactory<MyAppContext>
{
    public MyAppContext CreateDbContext(string[]? args = null)
    {
        var configuration = new ConfigurationBuilder().AddJsonFile("appsettings.json").Build();

        var optionsBuilder = new DbContextOptionsBuilder<MyAppContext>();
        optionsBuilder
            .UseSqlServer(configuration["ConnectionStrings:MSSqlConnect"]);

        return new MyAppContext(optionsBuilder.Options);
    }
}
```

Il faut ajouter ceci dans `MyApp.csproj` pour ne pas avoir une erreur :

```xml
  // ...
	<ItemGroup>
      <None Update="appsettings.json">
        <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
        <ExcludeFromSingleFile>true</ExcludeFromSingleFile>
        <CopyToPublishDirectory>PreserveNewest</CopyToPublishDirectory>
      </None>
    </ItemGroup>

</Project>
```

On a maintenant les `migrations` fonctionnant parfaitement dans notre `classlib`.

