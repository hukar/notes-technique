# 01 Setup

## Création du projet

```bash
  -rrc|--razor-runtime-compilation  
  Determines if the project is configured to use Razor runtime compilation in Debug builds.                                                              
  bool - Optional                                                                                                                                        
  Default: false 
```

Permet de compiler les pages `razor` au `runtime` => un style de `hot reload`.

```bash
dotnet new mvc -o InAndOut -rrc
```

`--auth none` option par défaut.

`-rrc` inclus un package dans `InAndOut.csproj`:

```cs
<Project Sdk="Microsoft.NET.Sdk.Web">

  <PropertyGroup>
    <TargetFramework>net5.0</TargetFramework>
    <CopyRefAssembliesToPublishDirectory>false</CopyRefAssembliesToPublishDirectory>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.AspNetCore.Mvc.Razor.RuntimeCompilation" Version="5.0.7" />
  </ItemGroup>

</Project>
```

