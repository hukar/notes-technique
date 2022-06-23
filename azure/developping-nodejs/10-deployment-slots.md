# 10 Slots de déploiement

## Déploiement classique

<img src="assets/Screenshot2020-07-15at15.37.41.png" alt="Screenshot 2020-07-15 at 15.37.41" style="zoom:50%;" />

`Staging Environment` environnement de pré-production, d'essai, de simulation.

On a différent environnements par lesquelles l'application doit passer avant d'arriver en production.

Si on a une `version 2` disponible, elle doit faire le même chemin vers la production :

<img src="assets/Screenshot2020-07-15at15.39.24.png" alt="Screenshot 2020-07-15 at 15.39.24" style="zoom:50%;" />

## Déploiement par Slots

Disponible seulement pour les versions `standard`, `prenium` et `isolated` de `Azure App Service`.

 <img src="assets/Screenshot2020-07-15at15.41.55.png" alt="Screenshot 2020-07-15 at 15.41.55" style="zoom:50%;" />

Plutôt que de passer la `version 2` en environnement de production, on fait pointer l'environnement de production vers la `version 2` et l'environnement d'essai vers la `version 1`.

Si un bug nous oblige à retourner vers la version 1, il suffit de switcher de nouveau les pointeurs :<img src="assets/Screenshot2020-07-15at15.41.44.png" alt="Screenshot 2020-07-15 at 15.41.44" style="zoom:50%;" />

## Utiliser Deployment Slots

On va changer le contenu de `index.js` :

```js
var express = require("express");
var router = express.Router();

/* GET home page. */
router.get("/", function (req, res, next) {
  const data = {
    title: "",
    message: process.env.MESSAGE || "devlopment environment",
  };
  res.render("index", data);
});

module.exports = router;
```

On va aussi mettre à jour le template `index.jade` :

```jade
extends layout

block content
  h1= title
  p Welcome to #{title}
  p #{message}
```

On `commit` mais on ne déploie pas.

## Définir les variables d'environnement dans le portail

<img src="assets/Screenshot2020-07-15at16.14.34.png" alt="Screenshot 2020-07-15 at 16.14.34" style="zoom:50%;" />

Dans `configuration` on peut ajouter `new application setting`, c'est en fait une variable d'environnement.

On va ajouter deux variables :

`NODE_ENV=production`

`MESSAGE=this is production`

<img src="assets/Screenshot2020-07-15at16.27.33.png" alt="Screenshot 2020-07-15 at 16.27.33" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-15at16.34.58.png" alt="Screenshot 2020-07-15 at 16.34.58" style="zoom:50%;" />

Pour `MESSAGE` on coche la case `Deployment slot setting`.

Cela veut dire que cette valeur est attachée au `slot`.

Puis sauvegarder les changements :

<img src="assets/Screenshot2020-07-15at17.19.09.png" alt="Screenshot 2020-07-15 at 17.19.09" style="zoom:50%;" />

## Créer un nouveau `staging slot`

### dans VSCode

Par défaut on a un slot `production` :

<img src="assets/Screenshot2020-07-15at17.23.16.png" alt="Screenshot 2020-07-15 at 17.23.16" style="zoom:50%;" />

Dans VSCode on va dans l'onglet Azure :

<img src="assets/Screenshot2020-07-15at17.25.39.png" alt="Screenshot 2020-07-15 at 17.25.39" style="zoom:50%;" />

On donne un nom.

<img src="assets/Screenshot2020-07-15at17.25.53.png" alt="Screenshot 2020-07-15 at 17.25.53" style="zoom:50%;" />

On choisie dans ce cas de cloner notre application.

<img src="assets/Screenshot2020-07-15at17.26.27.png" alt="Screenshot 2020-07-15 at 17.26.27" style="zoom:50%;" />

On va l'ouvrir dans notre portail **Azure**.

<img src="assets/Screenshot2020-07-15at17.29.15.png" alt="Screenshot 2020-07-15 at 17.29.15" style="zoom: 25%;" />

On a maintenant une App service staging :

<img src="assets/Screenshot2020-07-15at17.30.21.png" alt="Screenshot 2020-07-15 at 17.30.21" style="zoom:50%;" />

Avec sa propre `url`.

<img src="assets/Screenshot2020-07-15at17.31.33.png" alt="Screenshot 2020-07-15 at 17.31.33" style="zoom:50%;" />

Mais l'application en `staging` n'est pas encore déployée.

### Modifier la variable d'environnement `MESSAGE`

<img src="assets/Screenshot2020-07-16at10.15.56.png" alt="Screenshot 2020-07-16 at 10.15.56" style="zoom:50%;" />

## Déploiement

On a un nouveau dépôt `git` et de nouvelles valeurs d'authentification (credentials) :

<img src="assets/Screenshot2020-07-16at10.47.36.png" alt="Screenshot 2020-07-16 at 10.47.36" style="zoom:50%;" />

Une nouvel `url` `git` :

<img src="assets/Screenshot2020-07-16at11.27.43.png" alt="Screenshot 2020-07-16 at 11.27.43" style="zoom:50%;" />

```
https://hukar-weapp-nodejs-staging.scm.azurewebsites.net:443/hukar-weapp-nodejs.git
```

et les credentials :

<img src="assets/Screenshot2020-07-16at10.52.11.png" alt="Screenshot 2020-07-16 at 10.52.11" style="zoom:50%;" />

### On ajoute un nouveau remote

```bash
🦄 webapp git remote add staging https://null@hukar-weapp-nodejs-staging.scm.azurewebsites.net:443/hukar-weapp-nodejs.git
```

### On `push`

```bash
git push staging master
```

Il faut fournir les credentials du portail.

<img src="assets/Screenshot2020-07-16at11.42.50.png" alt="Screenshot 2020-07-16 at 11.42.50" style="zoom:50%;" />

On retrouve notre application dans l'environnement de `staging`.
