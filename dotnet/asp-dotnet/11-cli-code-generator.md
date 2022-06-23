# 11 Générer du code en ligne de commande

## Utilisation de `Code Generator`

### Installation ou mise à jour

```bash
dotnet tool install -g dotnet-aspnet-codegenerator
```

```bash
dotnet tool update -g dotnet-aspnet-codegenerator
```

On doit aussi ajouter le `Nuget Package` contenant les `templates`  au projet :

```bash
dotnet add package Microsoft.VisualStudio.Web.CodeGeneration.Design
```



## Créer un `Controller` d'`API`

```bash
dotnet aspnet-codegenerator controller -name Selfy -api -outDir Controllers
```

### Options

`--relativeFolderPath` ou `outDir`

`--model` ou `-m` : La classe servant de `Model`.

`--dataContext` ou `-dc` le `DbContext` à utiliser.

`--readWriteActions` ou `-actions` génère un `controller` avec les `actions` de base.

```bash
dotnet-aspnet-codegenerator controller -name MakesController -m Make -dc ApplicationDbContext --relativeFolderPath Controllers -api -actions
```



## Créer un template `razor` :  `Create`

```bash
dotnet aspnet-codegenerator view Index Create -m Product -outDir Views/Home -scripts -udl
```

`Index` le nom de la `View`

`Create` le nom du `Template` utilisé

`-m` définit le modèle

`-outDir` Chemin relatif au dossier du projet où créer la `View`

`-scripts` ajoute `_ValidationScriptsPartial`

`-udl` Utilise le `Layout` par défaut (sinon mais le `Layout = null`)



