# 07 bis un mini serveur web

```js
const http = require("http");

const server = http.createServer((req, res) => {
    if (req.url === "/") {
        res.end("Home");
    }

    if (req.url === "/about") {
        res.end("about");
    }
});

server.listen(8787, () => console.log("server running ..."));
```

## Mini `API`

```js
const http = require("http");

const server = http.createServer((req, res) => {

    if (req.url === "/api/users") {
      
        const users = [
            {
                name: "Sarah",
                age: 37
            },
            {
                name: "pâtée et chèvrïk",
                age: "34€"
            }
        ];
      
        res.writeHead(203, { "Content-Type": "application/json" });
        res.end(JSON.stringify(users));
    }
});

server.listen(8787, () => console.log("server running ..."));
```

Mime Type : `application/json`

Si on ne renseigne pas le `Content-Type`, on a un problème d'accent.

### On peut spécifier le `charset`   : `application/json;charset=utf-8`

Si on ne le fait pas, c'est le `charset` par défaut du navigateur qui est utilisé.

Toujours spécifié `utf-8`.

#### `res.writeHead(203, { "Content-Type": "application/json;charset=utf-8" });`