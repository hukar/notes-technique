# `http`

## Serveur de video

```js
const http = require("http");
const fs = require("fs");

const server = http.createServer((request, response) => {
    const readStream = fs.createReadStream("./sample.mp4");
    response.writeHead(200, { "Content-Type": "video/mp4" });

    readStream.pipe(response);
});

server.listen(8080, () => console.log("server is listening on 8080"));
```

## Serveur static

### Mon exemple :

`index.html`

```html
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>My site</title>
</head>

<body>
    <h1>My static site : nom d'une chèvre 🐐</h1>
    <form action="form" method="post" enctype="multipart/form-data">
        Yéééh! info : <input type="text" name="info">
        <br>
        upload mon gégé : <input type="file" name="file">
        <br>
        <button type="submit">Vive noël !</button>
    </form>
</body>

</html>
```

Ce qui est intéressant ici c'est `enctype="multipart/form-data"`.

`enctype` c'est le  `type MIME` d'encodage des données du formulaire :

- `application/x-www-form-urlencoded` le type initiale.
- `multipart/form-data` le type qui autorise [`input`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input) à `uploader` des fichiers.
- `text/plain  `  un type introduis en `HTML5`.

> **wikipedia**
>
> L'attribut `enctype` permet de spécifier l'encodage pour un envoi de type `POST`. 
>
> En particulier la valeur `enctype="multipart/form-data"` permet l'envoi de fichiers au serveur, en utilisant un champ de type fichier (`<input type="file"/>`). 
>
> Sans l'utilisation de cet encodage, un champ fichier n'envoie que le nom du fichier au serveur, sans son contenu.

`MIME type` : format de données des courriels (historiquement), Multipurpose Internet Mail Extensions

```js
const http = require("http");
const fs = require("fs");
const path = require("path");

const server = http.createServer((req, res) => {
    const index = fs.createReadStream(
        path.join(__dirname, "template", "index.html")
    );

    const url = req.url;

    switch (url) {
        case "/":
            res.writeHead(200,{ "Content-Type": "text/html;charset=utf-8" });
            index.pipe(res);
            break;
        case "/form":
            req.on("data", data => {
                console.log(`form : ${data}`);
            });
            req.on("end", () => {
                res.writeHead(301, {"Location": "/"});
                res.end();
            });
            break;
        default:
            res.writeHead(404, { "Content-Type": "text/plain;charset=utf-8" });
            res.end("Hum you're wrong !! 👻 héhéhé ...")
    }
});

server.listen(8080, () => console.log("server listenning on 8080"));
```

On récupère ceci :

```bash
form : ------WebKitFormBoundary39jguGViVRWSxet1
Content-Disposition: form-data; name="info"

yo coco
------WebKitFormBoundary39jguGViVRWSxet1
Content-Disposition: form-data; name="file"; filename="citrix_mac.rtf"
Content-Type: text/rtf

{\rtf1\ansi\ansicpg1252\cocoartf2512
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 ArialMT;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red0\green0\blue233;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;\cssrgb\c0\c0\c93333;}
\paperw11900\paperh16840\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs32 \cf2 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 Sur Mac seul marche l'extension Chrome Citrix Receiver.\
\
\pard\pardeftab720\partightenfactor0
{\field{\*\fldinst{HYPERLINK "https://chrome.google.com/webstore/detail/citrix-receiver/pjifibmneiofdojiaplameloephoakpj"}}{\fldrslt \cf3 \ul \ulc3 \strokec3 https://chrome.google.com/webstore/detail/citrix-receiver/pjifibmneiofdojiaplameloephoakpj}}\
\
Par contre il ne faut pas lancer Citrix Workspace avec car on se retrouve avec une erreur de certificat.\
Tu peux d\'e9sinstaller Citrix Workspace et ainsi tu ne passeras que par l'extension Chrome.\
}
------WebKitFormBoundary39jguGViVRWSxet1--
```

Il ne reste plus qu'a *parser* la réponse.

### Redirection dans `nodejs`

```js
res.writeHead(301, {"Location": "/"});
res.end();
```

### Exemple du cours

```js
const server = require("http").createServer(handler);
const path = require("path");
const fs = require("fs");

const PORT = process.env.NODE_PORT || 8080;
server.listen(PORT, onListener);

function onListener() {
    console.log(`server is listenning on ${PORT}`)
}

function handler(req, res) {
    const publicFolder = "public";
    
    const pathFile = path.join(__dirname, publicFolder, req.url);
    const fileExists = fs.existsSync(pathFile);
    

    if(fileExists) {
        fs.createReadStream(pathFile).pipe(res);
    } else {
        res.writeHead(404, {"Content-Type":"text/plain;charset=utf-8"});
        res.end("misericordious 😱 rhââaa...")
    }
}
```

`fs.existsSync(pathFile)` test l'existence d'un fichier et renvoie `true` ou `false`.

`path.join` est bien approprié pour construire le chemin.

`req.url` contient la partie après le nom de domaine et le `port`.