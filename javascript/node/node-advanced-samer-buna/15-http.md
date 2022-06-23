# 15 `http`

```js
const server = require("http").createServer();

server.on("request", (req, res) => {
    console.log("client connected!");

    res.writeHead(200, {
        "Content-Type": "text/plain;charset=utf-8"
    });
    res.write("hello client (^-°)/\n");
    res.end();
});

const PORT = process.env.NODE_PORT || 8000;

server.listen(PORT, () => {
    console.log(`server is listemnning on port ${PORT}`);
})
```

On va utiliser `curl` pour tester le serveur.

```bash
curl -i localhost:4546
HTTP/1.1 200 OK
Content-Type: text/plain;charset=utf-8
Date: Mon, 30 Mar 2020 08:45:19 GMT
Connection: keep-alive
Transfer-Encoding: chunked

hello client (^-°)/
```

`-i` pour avoir le `header` de la réponse.

`Connection: keep-alive` signifie que le serveur reste ouvert après l'envoie de la réponse.

`Transfer-Encoding: chunked` signifie que `Node.js` utilise un `stream` pour la réponse.

## `.end()` et `.write()`

Si on n'utilise pas la méthode `.end()`, le serveur continue de `streamer` du contenu :

```js
res.write("hello client (^-°)/\n");

setTimeout(() => {
    res.write("ho how hooww !!\n");
}, 1000);

setTimeout(() => {
    res.write("hello kitty\n");
}, 2000);
```

```bash
curl localhost:4546

hello client (^-°)/
# après une seconde
ho how hooww !!
# après deux secondes
hello kitty
```

## `server.timeout`

C'est le temps après lequel le serveur s'arrête en cas de longue requête :

```js
console.log(server.timeout);
120000 // deux minutes
```

Si on exécute une requête trop longue :

```js
setTimeout(() => {
        res.write("ho how hooww !!\n");
    }, 120005);
```

On ne parviendra jamais à obtenir la réponse :

```bash
curl localhost:4546
curl: (52) Empty reply from server
```

On peut modifier le `server.timeout` :

```js
server.timeout = 1000;

server.on("request", (req, res) => {
    res.writeHead(200, {
        "Content-Type": "text/plain;charset=utf-8"
    });

    setTimeout(() => {
        res.write("ho how hooww !!\n");
    }, 1001);

});
```

```bash
curl localhost:4546
curl: (52) Empty reply from server
```

