# 11 Utilisation d'un moteur de `template` 

## `template` définition

Un modèle ou un patron dans lequel des `placeholder` seront remplacer par de vrais valeurs à l'exécution (à l'affichage).

## Mon propre moteur

### Comprendre `match` et `matchAll`:

```js
const indexFile = `<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>`;

const result1 = indexFile.match(/{(\w+)}/);
console.log(result1);

const result2 = indexFile.match(/{(\w+)}/g);
console.log(result2);
```

```bash
[
  '{title}',
  'title',
  index: 4,
  input: '<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>',
  groups: undefined
]
[ '{title}', '{message}', '{mail}' ]
```

Dans le premier cas on obtient qu'une seule correspondance et avec le flag `g`, `match` ignore les groupes capturants.

#### On préférera `matchAll`

```js
const result1 = indexFile.matchAll(/{(\w+)}/);
console.log(result1);
console.log([...result1]);
```

```bash
Object [RegExp String Iterator] {}
[
  [
    '{title}',
    'title',
    index: 4,
    input: '<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>',
    groups: undefined
  ]
]
```

On récupère un `object iterator` qu'on ouvre avec `[...iterator]`.

Notre capture se trouve à l'indice `1`.

#### Avec le flag `g`

```js
const result2 = indexFile.matchAll(/{(\w+)}/g);
console.log([...result2]);
```

```bash
[
  [
    '{title}',
    'title',
    index: 4,
    input: '<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>',
    groups: undefined
  ],
  [
    '{message}',
    'message',
    index: 19,
    input: '<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>',
    groups: undefined
  ],
  [
    '{mail}',
    'mail',
    index: 40,
    input: '<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>',
    groups: undefined
  ]
]
```

On obtient bien toutes nos correspondance

### Cas complet

Mise au point d'une fonction de remplacement dans le `template`

`template.js`

```js
function replaceTemplate(page, values) {
    const regex = /{(\w+)}/gm;
		
  	// seul l'élément d'indice 1 correspond au groupe capturant
    const keys = [...page.matchAll(regex)].map(elt => elt[1]);

    for (key of keys) {
        page = page.replace(`{${key}}`, values[key]);
    }

    return page;
}

const pageIndex = "<h1>{title}</h1><p>{message}</p><footer>{mail}</footer>";
const doc = {
    title: "salut coco",
    message: "il fait beau aujourd'hui, c'est l'été !",
    mail: "tropico69@coco.be"
};

const output = replaceTemplate(pageIndex, doc);

console.log(output);

module.exports = replaceTemplate;
```

```
<h1>salut coco</h1><p>il fait beau aujourd'hui, c'est l'été !</p><footer>tropico69@coco.be</footer>
```

On voit que les variables entre accolades ont bien été remplacées par leur valeur.

### Simplification de `replaceTemplate`

```js
function replaceTemplate(page, values) {
    for (key in values) {
        page = page.replace(`{${key}}`, values[key]);
    }

    return page;
}
```

### Maintenant dans le serveur `app.js`

```js
const http = require("http");
const fs = require("fs");
const template = require("./template");
```
```js
const doc = {
    title: "RoBoTs",
    title_one: "Hello, we like robots",
    message: "Hy everybody, we are going to conquere the world!!",
    mail: "k.robot@al123.com"
};
```

```js
const app = http.createServer((req, res) => {
    if (req.url === "/") {
      	// ajouter l'encodage permet d'obtenir un string en sortie,
      	// sinon ce serait un Buffer
        const indexHtml = fs.readFileSync("./index.html", {
            encoding: "utf8"
        });

        const output = template(indexHtml, doc);

        res.writeHead(200, {
            "Content-Type": "text/html"
        });

        res.end(output);
    }

    if (req.url === "/favicon.ico") { /* ... */}
});
```

```js
const PORT = process.env.NODE_PORT || 6666;

app.listen(PORT, console.log(`server is running on [${PORT}]`));
```

