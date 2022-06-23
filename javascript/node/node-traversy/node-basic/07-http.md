# `HTTP`

## un simple serveur web `http`

```js
const http = require("http");

// Create server object
http.createServer((req, res) => {
    // write response
    res.write("hello my server");
    res.end();
}).listen(5009, console.log("server is running ..."));
```

## Réagir à la requête

```js
const http = require("http");
const path = require("path");
const fs = require("fs");

const server = http.createServer((req, res) => {
    if (req.url === "/") {
        fs.readFile(
            path.join(__dirname, "template", "index.html"),
            "utf8",
            (err, data) => {
                if (err) throw err;
  
                res.write(data);
                res.end();
            }
        );
    }
});

const PORT = process.env.PORT || 5050;

server.listen(PORT, () => console.log(`server running on port ${PORT}`));

```

Tant qu'on n'appelle pas `res.end()`, le navigateur continue de charger la page.

On peut directement mettre le template dans `end`

```js
res.end(data);
```

## `process`

C'est un objet global disponible partout dans **Node.js**.

cet objet fourni beaucoup de renseignement par exemple sur les variables d'environnement défini sur la machine :

```js
env: {
      SHELL: '/usr/local/bin/bash',
      # ...
      PWD: '/Users/kms/Documents/programmation/node/revision',
      # ...
      HOME: '/Users/kms',
      LANG: 'en_GB.UTF-8',
      # ...
      USER: 'kms',
      # ...
      PATH: '/Users/kms/mongodb-macos-x86_64-enterprise-4.2.3/bin:/Library/Frameworks/Python.framework/Versions/3.8/bin:/Users/kms/Library/sonar-scanner-4.0.0.1744-macosx/bin:/Users/kms/Library/Python/3.8/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/usr/local/share/dotnet:/opt/X11/bin:~/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands:/Applications/Xamarin Workbooks.app/Contents/SharedSupport/path-bin:/Users/kms/mongodb-macos-x86_64-enterprise-4.2.3/bin:/Users/kms/opt/anaconda3/condabin:/Library/Frameworks/Python.framework/Versions/3.8/bin:/Users/kms/Library/sonar-scanner-4.0.0.1744-macosx/bin:/Users/kms/Library/Python/3.8/bin',
      # ...
      _: '/usr/local/bin/nodemon'
    }
```

Je vais définir un `NODE_PORT` par défaut.

dans `.bash_profile` ajouter cette ligne :

```bash
export NODE_PORT=4546
```

Du coup dans Node.js

```js
const PORT = process.env.NODE_PORT || 5050;
```

## Ajouter `nodemon` au script

`package.json`

```json
"scripts": {
        "start": "node index",
        "dev": "nodemon index"
    },
```

```bash
npm run dev
```

## Configurer le header `writeHead`

```js
res.writeHead(202, {
  "Content-Type": "text/html"
});
```

## simple `API`

```js
if (req.url === "/api/users") {
        const users = [
            { name: "bob smith", age: 44 },
            { name: "Clara Tiby", age: 38 }
        ];
        res.writeHead(203, { "Content-Type": "application/json" });
        res.end(JSON.stringify(users));
    }
```

## Récupérer l'adresse `IP` du client `remoteAddress`

```js
server.on("connection", socket => {
    console.log(socket.remoteAddress);
});
```

Après que le serveur soit initialisé bien sûr.

## Récupérer les clés de l'object `req` : request

```js
const server = http.createServer((req, res) => {
    console.log("----------------------------");
    Object.entries(req).forEach(([k, v]) => console.log(k));
    res.end();
});
```

```bash
----------------------------
_readableState
readable
_events
_eventsCount
_maxListeners
socket
connection
httpVersionMajor
httpVersionMinor
httpVersion
complete
headers
rawHeaders
trailers
rawTrailers
aborted
upgrade
url
method
statusCode
statusMessage
client
_consuming
_dumped
```

### `Object.entries`

prend un objet en argument et renvoie un tableau de paires `[key, value]`

```js
const a = {name: "Babou", age: 45, color: "blue"};

Object.entries(a); // [["name", "Babou"],["age", 45],["color", "blue"]]
```

