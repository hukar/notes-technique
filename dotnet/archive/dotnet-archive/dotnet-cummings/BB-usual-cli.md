# BB. Les commandes usuelles

## Info sur `dotnet`

### Voire les versions installée

```bash
dotnet --list-sdks
```

### Connaitre la version de `.net`

```bash
dotnet --version
```

### Créer un fichier pour changer de version dans un répertoire

```bash
dotnet new globaljson
```

### Toutes les versions installées

```bash
dotnet --info
```

### Avoir de l'aide

```bash
dotnet -h
dotnet new -h
```

## Créer un fichier de solution

```bash
dotnet new sln
```

## Créer un projet

```bash
dotnet new classlib -n <Project>
```

`-n` nom.

`-o` chemin du projet : crée le dossier s'il n'existe pas

### Lister tous les types de projets

```bash
dotnet new
# ou
dotnet new -l # plus explicite
```

```
Templates                     Short Name      Language    Tags                  
----------------------------  --------------  ----------  ----------------------
Console Application           console         [C#],F#,VB  Common/Console        
Class library                 classlib        [C#],F#,VB  Common/Library        
xUnit Test Project            xunit           [C#],F#,VB  Test/xUnit            
ASP.NET Core Web API          webapi          [C#],F#     Web/WebAPI            
ASP.NET Core gRPC Service     grpc            [C#]        Web/gRPC              
dotnet gitignore file         gitignore                   Config                
global.json file              globaljson                  Config                
Solution File                 sln                         Solution              
Protocol Buffer File          proto                       Web/gRPC 
```

Ceux que j'utilise.

### Ajouter tous les projets au fichier de solution

```bash
dotnet sln add **/
```

### Lister tous les projets de la solution

```bash
sln list
```

### Ajouter une ou plusieurs référence.S à un projet

```bash
dotnet add <Project> reference <Project_path_1> <Project_path_2>
```

### Lancer un projet

```bash
dotnet run -p <Project>
```

`-p` cible un projet.

## `Package`

### Ajouter un `package`

```bash
dotnet add Persistence/ package Microsoft.EntityFrameworkCore.Sqlite --version 3.1.9

dotnet add <Project> package <Package> [--version 3.1.9]
```

### Retirer un `package`

```bash
dotnet remove package package Microsoft.EntityFrameworkCore.Sqlite
```



## Certificat

### Créer un certificat

```bash
dotnet dev-certs https --trust
```



## Entity Framework : `dotnet ef`

