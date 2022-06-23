# 01 installer `express`

```bash
npm i express
```

## Premier serveur

```js
const express = require("express");

const app = express();

app.get("/", (req, res) => {
    res.send("<html><body><h1>hello Express</h1></body></html>");
});

const PORT = process.env.NODE_PORT || 3000;
app.listen(PORT);
```

`express` choisie au mieux lui-même le `Content-Type` et l'encodage.

Chaque verbe `http` est *mappé* sur un méthode de `app`.

`express` est une fonction qui retourne une fonction.

Comme les fonctions `javascript` sont avant tout des objets, `app` possède donc des méthodes.

### Servir du `json`

```js
const express = require("express");
const app = express();


app.get("/api", (req, res) => {
    res.json({ firstname: "john", age: 29 });
});

const PORT = process.env.NODE_PORT || 3000;
app.listen(PORT);
```

