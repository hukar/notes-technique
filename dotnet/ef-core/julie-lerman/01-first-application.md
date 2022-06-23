# 01 Construire sa première application



## Créer une nouvelle solution

`SamouraiApp`

```bash
dotnet add sln -o SamouraiApp
```

`-o` le chemin où placer le contenu généré



## Créer le `Domain` : `SamouraiApp.Domain`

C'est une librairie de classes :

```bash
dotnet new classlib -o SamouraiApp.Domain
```

C'est la représentation des données : les entités.

## Créer  `SamouraiApp.Data`

C'est une librairie de classes :

```bash
dotnet new classlib -o SamouraiApp.Data
```

Cest ici que les données sont manipulés, c'est ici qu'on installe `EF Core`.

## Créer l'application `SamouraiApp.UI`

C'est une application `console` :

```bash
dotnet new console -o SamouraiApp.Ui
```

On peut regarder dans son fichier `SamouraiApp.UI.csproj` que c'est un exécutable  et qu'on cible `.net 5`:

```csharp
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net5.0</TargetFramework>
  </PropertyGroup>

</Project>
```



## Ajouter les projets à la solution

```bash
dotnet sln add **/*.csproj
```



## Ajouter deux classes dans `SamouraiApp.Domain`

`Samourai.cs`

```csharp
using System.Collections.Generic;

namespace SamouraiApp.Domain
{
    public class Samourai
    {
        public int Id { get; set; }
        public string Name { get; set; }

        public List<Quote> Quotes { get; set; } = new List<Quote>();
    }
}
```



`Quote.cs`

```csharp
namespace SamouraiApp.Domain
{
    public class Quote
    {
        public int Id { get; set; }
        public string Text { get; set; }
        public Samourai Samourai { get; set; }
        public int SamouraiId { get; set; }
    }
}
```



## Ajouter les références



### `SamouraiApp.Data` 

`SamouraiApp.Data` doit avoir une référence de `SamouraiApp.Domain` :

```bash
dotnet add SamouraiApp.Data reference SamouraiApp.Domain
```

`SamouraiApp.Data.csproj`

```csharp
<Project Sdk="Microsoft.NET.Sdk">

  <ItemGroup>
    <ProjectReference Include="..\SamouraiApp.Domain\SamouraiApp.Domain.csproj" />
  </ItemGroup>
  // ...
```

