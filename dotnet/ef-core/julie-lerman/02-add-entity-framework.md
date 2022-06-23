# 02-ajouter `entity framework`



## `nuget` package

En choisissant la base de données utilisée avec `EF Core`, on peut installer un package spécifique qui aura comme dépendance `Microsoft.EntityFrameworkCore`.

### Installer `EF Core` pour `SQL Server`

On voit que `Microsoft.EntityFrameworkCore.SqlServer` a une dépendance vers `Microsoft.EntityFrameworkCore.Relational` qui a une dépendance vers `Microsoft.EntityFrameworkCore`.

On peut donc directement installer `Microsoft.EntityFrameworkCore.SqlServer` et les dépendances seront automatiquement installées.

Dans le projet `SamouraiApp.Data`

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer --version 5.0.4
```

`SamouraiApp.Data.csproj`

```csharp
<Project Sdk="Microsoft.NET.Sdk">
  // ...

  <ItemGroup>
    <PackageReference Include="Microsoft.EntityFrameworkCore.SqlServer" Version="5.0.4" />
  </ItemGroup>
  // ...
```



## Installer la version pour `Sqlite`

C'est préférable pour commencer d'installer d'abord la version pour `Sqlite` :

```bash
dotnet remove package Microsoft.EntityFrameworkCore.SqlServer

dotnet add package Microsoft.EntityFrameworkCore.Sqlite --version 5.0.4
```

