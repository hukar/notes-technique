# 02 Express Advanced



## Middleware

C'est une fonction qui prend la requête en entrée et qui renvoie une réponse ou passe la main à un autre `middleware`.

`express.json()` est un middleware, tout comme la `route()` que prend `app.get` en `callback`.

Les différents `middleware` forme un `pipeline` où transite la requête.

## Créer un `custom middleware`

Pour ajouter un `middleware` on utilise `app.use`.

Un `middleware` prend trois paramètres : `req`, `res` et `next`.

`next` est une référence vers le prochain `middleware`.

```js
app.use(function(req, res, next) {
    console.log('Logging...')
    next()
})

app.use(function(req, res, next) {
    console.log('authenticating...')
    next()
})
```

#### ! si on ne met pas de `next`, la requête reste bloquée.

Une bonne pratique est de mettre les `middleware` dans des fichiers séparés :

`logger.js`

```js
function log(req, res, next) {
    console.log('Logging...')
    next()
}

module.exports = log
```

`app.js`

```js
const logger = require('./logger')

// ...

app.use(logger)
```





## Built In `middleware`

### `urlencoded` (obsolète)

Permet de *parser* le contenu d'un formulaire dont `enctype="application/x-www-form-urlencoded"`



### `body-parser`

Remplace `json`, `raw`, `text` et `urlencoded`.

Met le contenu reçu dans `req.body`.

```bash
npm i body-parser
```

```js
const bodyParser = require('body-parser')
```

On peut instancier plusieurs `parser` :

```js
const jsonParser = bodyParser.json()
const urlencodedParser = 
```

