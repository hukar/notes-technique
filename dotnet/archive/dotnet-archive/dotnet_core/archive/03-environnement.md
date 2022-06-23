# 03 environnement

Dans .net core il y a trois environnements :

`Development` `Staging` `Production`

## Modifier la varible d'environnement

### Temporairement 

Dans le terminal :

```bash
export ASPNETCORE_ENVIRONMENT=production
```

### Définitivement

Dans `.bash_profile` :

```bash
export ASPNETCORE_ENVIRONMENT=production
# la même chose
```

Dans Windows définir la variable d'environnement.

## dotnet run

Quand l'application est lancée avec la commende `dotnet run`, c'est le fichier `launchSettings.json` qui est lu s'il existe, sinon c'est la variable d'environnement.
