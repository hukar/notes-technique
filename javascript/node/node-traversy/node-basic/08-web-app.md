# 08 Web Application

Un application web dynamique :

```js
const http = require("http");
const path = require("path");
const fs = require("fs");

const server = http.createServer((req, res) => {
    // Build file path
    let filePath = path.join(
        __dirname,
        "public",
        req.url === "/" ? "index.html" : req.url
    );

    const page404File = path.join(__dirname, "public", "404.html");

    // Extension of file
    let extName = path.extname(filePath);

    // Initial content type
    let contentType = "text/html";

    // check ext and set content type
    switch (extName) {
        case ".js":
            contentType = "text/javascript";
            break;
        case ".css":
            contentType = "text/css";
            break;
        case ".json":
            contentType = "application/json";
            break;
        case ".png":
            contentType = "image/png";
            break;
        case ".jpg":
            contentType = "image/jpg";
            break;
    }

    // Read file

    // Error NO ENTry (or Error NO ENTity)
    fs.readFile(filePath, (err, content) => {
        if (err) {
            if (err.code === "ENOENT") {
                fs.readFile(page404File, (err, content) => {
                    if (err) throw err;

                    res.writeHead(404, {
                        "Content-Type": "text/html;charset=utf-8"
                    });
                    res.end(content);
                });
            } else {
                res.writeHead(500, { "Content-Type": "text/plain" });
                res.end(`Server error ${err.code}`);
            }
        } else {
            res.writeHead(200, { "Content-Type": contentType });
            res.end(content);
        }
    });
});

const PORT = process.env.PORT || 5000;

server.listen(PORT, () => console.log(`server running on port:[${PORT}] ...`));
```

### `ENOENT` Error NO ENtry

`const PORT = process.env.PORT || 5000;` cela permet de récupérer le port du serveur lorsqu'on déploie en production.`