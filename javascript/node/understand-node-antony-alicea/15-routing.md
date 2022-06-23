# 15 Le routage

Lier une requête `HTTP` à un contenu, une `url` à une ressource.

```js
const server = http.createServer((req, res) => {
```

```js
    const regexApi = /^\/api.*/;
    const regexImg = /^\/img.*/;
```
Création de `regex` :

`^` : commence par

`\/` : caractère `\`

`.` : n'importe qule caractère

`*` : 0 ou plusieurs fois

`api` ou `img` : le littéral correspondant.

```js
    if (regexApi.test(req.url)) {
        res.writeHead(200, { "Content-type": "application/json" });

        const o = {
            name: "John",
            age: 44,
            pet: "Doggy",
            url: req.url
        };

        res.end(JSON.stringify(o));
```
Ici on traite le cas où on appelle l'api.


```js
    } else if (regexImg.test(req.url)) {
        const extName = path.extname(req.url).replace(".", "");

        console.log(extName);
        res.writeHead(200, {
            "Content-Type": `image/${extName}`
        });

        const filePath = path.join(__dirname, "img", path.basename(req.url));
      	// const filePath = path.join(__dirname, "titi", path.basename(req.url));

        fs.createReadStream(filePath).pipe(res);
```
Ici on répond au demande d'image.

On peut créer ou pas le dossier `img`, en fait comme on fait correspondre manuellement l'`url` aux ressources, la structure d'`url` peut suivre une logique très différente de la structure des fichiers du projet.

On utilise le `pipe`

#### `fs.createreadStream(filePath).pipe(res)`


```js
    } else {
        fs.createReadStream("./index.html", "utf8").pipe(res);
    }
}).listen(4546, console.log("server run at 4546"));
```

Les autres `url` appelle `index.html`.

### remarque

Il faut bien isoler les `url` avec des `if/else if/else` ou un `switch` car on peut vite avoir des comportement conflictuel entre les différents traitements, car une `url` peut matcher plusieurs patterns.