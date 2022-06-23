# 00 `HTTP` streaming

Pour faire du streaming avec `HTTP 1.1` on doit utiliser `Transfert-Encoding: chunked` dans le `Header`.

## Test avec Node JS

`app.js`

```js
const http = require("http");
const fs = require("fs");

const text = fs.readFileSync("./text.html");

const server = http.createServer((req, res) => {
    res.end(text);
});

server.listen(3031, console.log("server is listening on port 3031"));
```

Dans ce cas le fichier est envoyé d'un block.

On a comme `headers` de réponse :

```
HTTP/1.1 200 OK
Date: Fri, 24 Jul 2020 07:33:09 GMT
Connection: keep-alive
Content-Length: 663
```

### Maintenant utilisation de `stream`

`app.js`

```js
const http = require("http");
const fs = require("fs");

const server = http.createServer((req, res) => {

    const text = fs.createReadStream("./text.html" ,{
        highWaterMark: 12
    } );

    res.setHeader("Content-Type","text/html")
    text.pipe(res);
});

server.listen(3030, console.log("server is listening on port 3030"));
```

Ici on ajoute un `highWaterMark` à 12, ce qui veut dire qu'on envoie notre réponse 12 bytes à la fois. (ou 12 caractères).

Voici le `header` de la réponse envoyée :

```
HTTP/1.1 200 OK
Content-Type: text/html
Date: Fri, 24 Jul 2020 07:37:20 GMT
Connection: keep-alive
Transfer-Encoding: chunked
```

### `Transfert-Encoding: chunked`

Cela permet de segmenter la réponse (streaming).