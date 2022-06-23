# 01 Création du projet

On va créer un projet `CarRentManagement`.

On choisie l'option `Web Assembly`, cette option ne nécessite pas l'utilisation d'un serveur car le code tourne dans le navigateur.

On inclus `ASP.NET Core Hosted` et `Progressive Web App`.

Pour l'authentification, on utilies `Individual User Account` aui utilise les standard de `Open AUTH`.

## En `CLI`

```bash
dotnet new blazorwasm -ho -au Individual -p -o CarRentManagement2
```

`-ho` `--hosted` => `ASP.NET Core Hosted` ajoute une api.

`-au` `--auth` `Individual`

`-p` `--pwa` => `Progressive Web App`



## `Client`

`manifest.json` et `service-worker.js` sont des fichiers spécifiques à `PWA`.



## `Server`

Le dossier `Areas` sert à l'authentification.