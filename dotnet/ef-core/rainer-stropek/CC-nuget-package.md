# CC . `Nuget` package



## `Entity Framework Core`

Pour les `migrations`

```bash
dotnet add package Microsoft.EntityFrameworkCore.Design
```



Pour utiliser `MSSql`

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer 
```



Pour les outils en ligne de commande :

```cs
dotnet tool install --global dotnet-ef
```

Pour les mettre à jour :

```bash
dotnet tool update --global dotnet-ef
```



## Utilisation de `appsettings.json`

> Déjà présent avec `asp.net` : `dotnet new web`

```bash
dotnet add package Microsoft.Extensions.Configuration.Json
```



## `SQL` dans la `Console`

Permet d'afficher les requêtes `SQL` générée par `EF Core`.

Déjà présent avec `asp.net`.

```bash
dotnet add package Microsoft.Extensions.Logging.Console
```

