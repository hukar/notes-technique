# `http` routeur

```js
const http = require("http");
const fs = require("fs");
const path = require("path");
const port = process.env.NODE_PORT || 8080;

const server = http.createServer();
```
On va utiliser les événement plutôt que de passer directement une fonction à `createServer`.
```js
server.on("request", requestHandler);
server.listen(port, onListen);

function onListen() {
    console.log(`Server listen on port ${port}`);
}

function requestHandler(req, res) {
    let filepath;

    res.setHeader("Content-Type", "text/html;charset=utf-8");
```
On doit traiter les fichiers `css` différement, toute url correspondant à `"/css/___.css"` essayera de télécharger un fichier `css` ou sera rediriger vers `"/"`.
```js

    if (/^\/css\/\w*\.css/m.test(req.url)) {
        filepath = path.join(__dirname, "public", req.url);
        if (fs.existsSync(filepath)) {
            res.writeHead(200, { "Content-Type": "text/css" });
            fs.createReadStream(filepath).pipe(res);
        } else {
            res.writeHead(301, { Location: "/" });
            res.end();
        }
        return;
    }

    switch (req.url) {
        case "/":
        case "/hello":
            res.writeHead(200);
            filepath = path.join(__dirname, "public", "html", "index.html");
            break;
        default:
            res.writeHead(404);
            filepath = path.join(__dirname, "errors", "404.html");
    }

    const rs = fs.createReadStream(filepath);
    rs.pipe(res);
}
```

