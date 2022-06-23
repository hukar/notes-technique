#  16 `https`

## Base en `http`

```js
const server = require("http").createServer();

server.on("request", (req,res) => {
    res.write("hello kiki\n");
    res.end();
});

const PORT = process.env.NODE_PORT || 8000;
server.listen(PORT,() => {
    console.log(`server listenning ${PORT}`);
});
```

## Modification pour `https`

Pour pouvoir tester `https` il faut générer un certificat auto-signé avec `openSSL`.

Ce certificat n'est pas valable pour les navigateurs, mais est sufisant pour les tests.

```bash
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -nodes
```

Cela génère deux fichier une clé privée `key.pem` et un certificat `cert.pem`.

```js
const fs = require("fs");

const server = require("https").createServer({
    key: fs.readFileSync("./key.pem"),
    cert: fs.readFileSync("./cert.pem")
});

server.on("request", (req, res) => {
    res.write("hello kiki\n");
    res.end();
});

// const PORT = process.env.NODE_PORT || 8000;

const SPORT = 443; // port pour https
server.listen(SPORT, () => {
    console.log(`server listenning ${SPORT}`);
});
```

Mais tout cela ne fonctionne pas sur `Chrome`, il n'accepte pas le Certificat auto-signé.

Cela fonctionne sur `Safari`

