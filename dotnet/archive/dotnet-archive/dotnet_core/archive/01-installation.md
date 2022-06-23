# 01 installation

Pour créer un template avec **.net core** on peut utiliser la commande dotnet.

````bash
# mettre à jour toutes les possibilités
dotnet new --install Microsoft.AspNetCore.SpaTemplates::*

# ensuite pour utiliser.net core avec vue.js
dotnet new vue
````

Avant de lancer le projet on exécute :

```bash
npm install 
```

## Lancer l'application

```bash
dotnet run
```

## arborescence

ClienApp = javascript (Vue.js)

Controllers = c#

wwwroot = dossier public de l'application css et js compilés.

