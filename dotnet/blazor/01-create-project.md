# 01 Créer un projet Blazor

## En `CLI`

### Pour un projet `Balzor WASM`

```bash
dotnet new blazorwasm -ho -au Individual -p -o MyProject
```

`-ho` `--hosted` => `ASP.NET Core Hosted` ajoute une api.

`-au` `--auth` `Individual` ajoute l'authentification

`-p` `--pwa` => `Progressive Web App`



## Lancer un projet `--hosted`

Il faut lancer le projet dans le repértoire `Server` et faire un classique `dotnet run` :

```bash
✨ Server : dotnet watch run
```





## Ajouter : `Blazored.LocalStorage`

```bash
dotnet add package Blazored.LocalStorage
```

































