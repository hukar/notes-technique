# 01 On commence avec `EF Core`

`Entity Framework Core` est un `ORM` : Object Relational Mapping.

## Installer `Entity Framework Core`

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer --version 5.0.5
```

```cs
// dotnet-rpg.csproj

<Project Sdk="Microsoft.NET.Sdk.Web">

  <PropertyGroup>
    <TargetFramework>net5.0</TargetFramework>
    <RootNamespace>dotnet_rpg</RootNamespace>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="AutoMapper.Extensions.Microsoft.DependencyInjection" Version="8.1.1" />
    <PackageReference Include="Microsoft.EntityFrameworkCore.SqlServer" Version="5.0.5" />
    <PackageReference Include="Swashbuckle.AspNetCore" Version="5.6.3" />
  </ItemGroup>

</Project>

```

`Swashbuckle.AspNetCore` générant les pages `Swagger`.

#### ! choisir une version de `EF Core` identique à la version de `.Net`, ici `5`



## Installer les outils `EF Core`

Pour les migrations on a besoins des `Entity Framework Tools`.

```bash
dotnet ef


                     _/\__       
               ---==/    \\      
         ___  ___   |.    \|\    
        | __|| __|  |  )   \\\   
        | _| | _|   \_/ |  //|\\ 
        |___||_|       /   \\\/\\

Entity Framework Core .NET Command-line Tools 5.0.4
```

Pour checker s'ils sont déjà installés.

Dé-installer `dotnet-ef` :

```bash
dotnet tool uninstall --global dotnet-ef
```

et les ré-installer

```bash
dotnet tool install --global dotnet-ef --version 5.0.5
```

#### ! bien choisir la version

```bash
dotnet ef

                     _/\__       
               ---==/    \\      
         ___  ___   |.    \|\    
        | __|| __|  |  )   \\\   
        | _| | _|   \_/ |  //|\\ 
        |___||_|       /   \\\/\\

Entity Framework Core .NET Command-line Tools 5.0.5
```



## Installer `Entity Framework Design`

Contient le code utilisé par `dotnet ef` pour les migrations.

```bash
dotnet add package Microsoft.EntityFrameworkCore.Design --version 5.0.5
```

```cs
<Project Sdk="Microsoft.NET.Sdk.Web">

  <PropertyGroup>
    <TargetFramework>net5.0</TargetFramework>
    <RootNamespace>dotnet_rpg</RootNamespace>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="AutoMapper.Extensions.Microsoft.DependencyInjection" Version="8.1.1" />
    <PackageReference Include="Microsoft.EntityFrameworkCore.Design" Version="5.0.5">
      <IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>
      <PrivateAssets>all</PrivateAssets>
    </PackageReference>
    <PackageReference Include="Microsoft.EntityFrameworkCore.SqlServer" Version="5.0.5" />
    <PackageReference Include="Swashbuckle.AspNetCore" Version="5.6.3" />
  </ItemGroup>

</Project>

```

`PrivateAssets` permet de ne pas publier sur le serveur (?), dans le cas où on souhaite publier une bibliothèque.