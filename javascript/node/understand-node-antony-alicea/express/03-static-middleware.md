# 03 Les fichiers statiques et les `middleware`

## `Middleware`

Du code qui se trouve entre deux couches logicielles.

Dans le cas de `express`, du code se situant entre la requête et la réponse.

## `Static files` : les fichiers statiques

### ≠ dynamique !

Les fichiers statique ne sont pas générés de manière programmatique => `html`, `css`, `image`.

### Fabrication maison

```js
app.get("/static/:file", (req, res) => {
    const { file } = req.params;
    const readable = fs.createReadStream(`./static/${file}`);

    if (path.extname(file) === ".html") {
        res.writeHead(200, {
            "Content-Type": "text/html;charset=utf-8"
        });
    } else if (path.extname(file) === ".png") {
        res.writeHead(200, {
            "Content-Type": "image/png"
        });
    }

    readable.pipe(res);
});
```

Avec l'idée que tous les fichiers statiques soient stockés dans le dossier `static`.

#### ! Ne pas mettre `utf8` en option dans `fs.createReadStream`, sinon l'image sera transformée (illisible).

## Utilisation des `middleware` : `.use`

```js
app.use("/assets", express.static(path.join(__dirname, "public")));
```

Ce `middleware` associe une route à un dossier `static`.

Tous les fichiers mis dans ce dossier seront servis par express automatiquement.

## Création de son propre middleware

```js
app.use("/", (req, res, next) => {
    console.log("Request URL :", req.url);
    next();
});
```

le premier argument est la route, le deuxième est un `callback` qui appelle `next` pour passer au prochain `middleware`.

`.get` est un `middleware` et sera le prochain appelé.

## Récupérer les cookies avec `cookie-parser`

### Installer le `middleware`

```bash
npm i cookie-parser
```

### L'importer dans le code

```js
const cookieParser = require("cookie-parser");
```

### L'utiliser

```js
app.use(cookieParser());

app.use("/", (req, res, next) => {
    console.log("Cookies ", req.cookies);
    next();
});
```

### le résultat

```bash
Cookies  {
  _ga: 'GA1.1.1924986067.1554122637',
  __utmz: '111872281.1567758859.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)',
  __utma: '111872281.1924986067.1554122637.1567765123.1567770154.4',
  style: 'defaultstyle',
  csrftoken: 'nuh3zVCkuRjzteK2IgkiCTOZsbMJO2zwVStKSbWPkE6j5M3KdECp0CqmVCPh9FMz'
}
```

