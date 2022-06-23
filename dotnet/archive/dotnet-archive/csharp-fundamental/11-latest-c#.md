# 11 Les nouveautés en `C#`

## Non Nullable Reference Type

Si je n'initialise pas une liste dans mon constructeur :

```cs
public InMemoryBook(string name) : base(name)
{
    // grades = new List<double>();
    Name = name;
}

// ...

private List<double> grades;
```

mon `build` fonctionne mais j'ai un `warning` :

```bash
Build succeeded.

InMemoryBook.cs(64,30): warning CS0649: Field 'InMemoryBook.grades' is never assigned to, and will always have its default value null [/Users/kar/Documents/programmation/dotnet/csharp-fundamental/gradebook/src/GradeBook/GradeBook.csproj]
```

Par défaut `grades` est initialisé à `null`.

Dans `c# 8` par défaut les types références ne peuvent pas être null.

Il faut ajouter deux lignes dans `GradeBook.csproj` :

```cs
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>netcoreapp3.1</TargetFramework>
    <!-- ici -->
    <LangVersion>8.0</LangVersion>
    <NullableContextOptions>enable</NullableContextOptions>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="ColoredConsole" Version="1.0.0" />
  </ItemGroup>

</Project>
```

Dans le code on peut utiliser `?` pour dire qu'un type peut avoir la valeur `null` :

```cs
Book? book = null;
```

