# 00 `NPM` Node Package Manager

## `npm init`

crée le fichier `package.json`

```json
{
  "name": "nodehttp",
  "version": "1.0.0",
  "description": "simple server http",
  "main": "server.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1",
    "start": "node server.js"
  },
  "author": "hukar",
  "license": "ISC",
  "dependencies": {
    "nodemon": "^2.0.2"
  }
}
```

## `npm install package`

```bash
npm install -D package # --save-dev
```

installe le package comme dépendance de développement uniquement

```js
"devDependencies": {
  "nodemon": "^2.0.2"
}
```

## nodemon

relance le script à chaque sauvegarde du fichier 