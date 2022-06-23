# 15 Déploiement d'unz application `Blazor`

## `Blazor Server`

<img src="assets/blazor-server-model.png" alt="blazor-server-model" style="zoom:67%;" />



### `SignalR`

- Bi-Directionnel (connexion persistente)
- Multi Utilisateur
- Full Duplex (communication simultanée dans les deux sens)
- Asynchrone



### Test de performance

<img src="assets/blazor-server-performance-tests.png" alt="blazor-server-performance-tests" style="zoom:80%;" />



## Déploiement `Blazor Server` sur `IIS`

### 1. Installer `IIS` : Modules nécessaire

<img src="assets/requirement-for-iis-install.png" alt="requirement-for-iis-install" style="zoom:80%;" />



### 2. Installer le `Runtime`

Il faut ensuite installer sur le serveur :

Windows Hosting Bundle

https://dotnet.microsoft.com/en-us/download/dotnet/thank-you/runtime-aspnetcore-6.0.4-windows-hosting-bundle-installer



### 3. Publier le projet dans un dossier

```bash
dotnet publish -c Release [-o ../Publish/Release]
```

`-c` configuration défaut `Debug`

`Balzor Server` l'`App` est publié dans `/bin/Release/{TARGET FRAMEWORK}/publish` si on ne précise pas le dossier.

C'est ce dossier qu'il faut déployer sur le serveur.

<img src="assets/content-for-deploiement.png" alt="content-for-deploiement" style="zoom: 25%;" />



### 4. Changer le port du site par défaut

`80` => `90`



### 5. Créer un site web avec le port 80

Créer le Website sur `IIS` et copié le dossier `publish` dedans.



#### Schéma de fonctionnement classique

<img src="assets/how-work-iis.png" alt="how-work-iis" style="zoom:50%;" />

Le serveur `IISHttpServer` transforme la requête en un objet `HttpContext` qu'il envoie à l'application (`asp.net`).



#### Schéma pour `Blazor Server`

<img src="assets/schema-fir-blazor-server-works.png" alt="schema-fir-blazor-server-works" style="zoom:37%;" />



