# 12 Services et librairies

## Application actuelle

<img src="assets/Screenshot2020-10-25at07.56.26.png" alt="Screenshot 2020-10-25 at 07.56.26" style="zoom: 33%;" />

On veut un service ou `daemon` :

<img src="assets/Screenshot2020-10-25at07.59.44.png" alt="Screenshot 2020-10-25 at 07.59.44" style="zoom:33%;" />

- Toujours disponible
- Tourne en `background`
- Fiable : peut se relancer tout seul en cas de problème

## Library

On veut refaire l'application pour que celle-ci s'exécute automatiquement.

On pourrait copier l'original pour créer notre nouvelle application ou bien utiliser une librairie :

<img src="assets/Screenshot2020-10-19at17.13.01.png" alt="Screenshot 2020-10-19 at 17.13.01" style="zoom:50%;" />

## création de la structure

On crée trois dossier un pour l'`app` un pour le `service` et un pour la `library`.

Pour l'`app` et le `service` on utilise un template de base (`console`) .

Pour la `library` on utilise `classlib`.

```bash
dotnet new console -o ./DictationProcessorApp/

dotnet new console -o ./DictationProcessorSvc/

dotnet new classlib -o ./DictationProcessorLib/
```

<img src="assets/Screenshot2020-10-25at15.49.16.png" alt="Screenshot 2020-10-25 at 15.49.16" style="zoom:33%;" />

## `.net standard`

C'est une spécification qui garantie toute la compatibilité avec les différents projets `.net`.

Dans le projet de librairie :

`DictationProcessorLib.csproj`

```csharp
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
  </PropertyGroup>

</Project>
```

Par défaut le librairie cible `netstandard`.

On doit remanier le code en mettant la logique dans la librairie.
