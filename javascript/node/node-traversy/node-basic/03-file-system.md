## 03 Système de fichier `fs`

### Créer un dossier `mkdir`

```js
const fs = require("fs");
const path = require("path");

// Create folder
// async -> (path, option, callback)
fs.mkdir(path.join(__dirname, "test"), {}, err => {
    if (err) throw err;
    console.log("Folder created ... ");
});
```

`mkdir` est asynchrone, il existe une version synchrone : `fs.mkdirSync(path[, options])` qui n'a donc pas de fonction `callback`.

### Créer un fichier et écrire dedans `writeFile`

```js
// Create and write to file
fs.writeFile(path.join(__dirname, "test", "hello.txt"), "hello titi", err => {
    if (err) throw err;
    console.log("File created ... ");
});
```

Si le fichier existe déjà, il est écrasé.

### Modifier un fichier `appendFile`

Comme `writeFile` est asynchrone, on doit mettre la modification dans son `callback` :

```js
fs.writeFile(path.join(__dirname, "test", "hello.txt"), "hello titi", err => {
    if (err) throw err;
    console.log("File created ... ");

    // file append
    fs.appendFile(
        path.join(__dirname, "test", "hello.txt"),
        "\n I love node js (^-^)/",
        err => {
            if (err) throw err;
            console.log("Append file ... ");
        }
    );
});
```

Si le fichier n'existe pas, il est créé (comme avec `fs.writeFile`).

### Lire un fichier `readFile`

```js
// read file
fs.readFile(path.join(__dirname, "test", "hello.txt"), "", (err, data) => {
    if (err) throw err;
    console.log("data : ", data);
});
```

```bash
data :  <Buffer 68 65 6c 6c 6f 20 74 69 74 69 0a 20 49 20 6c 6f 76 65 20 6e 6f 64 65 20 6a 73 20 28 5e 2d 5e 29 2f>
```

On lit directement le buffer, si on veut le texte en `utf8`, il faut ajouter l'encodage.

```js
fs.readFile(path.join(__dirname, "test", "hello.txt"), "utf8", (err, data) => {
    if (err) throw err;
    console.log("data : ", data);
});
```

### Encodage supporté par `node`

```bash
 utf8: {
    encoding: 'utf8',
    encodingVal: encodingsMap.utf8,
    byteLength: byteLengthUtf8,
    write: (buf, string, offset, len) => buf.utf8Write(string, offset, len),
    slice: (buf, start, end) => buf.utf8Slice(start, end),
    indexOf: (buf, val, byteOffset, dir) =>
      indexOfString(buf, val, byteOffset, encodingsMap.utf8, dir)
  },
  ucs2: { ... },
  utf16le: { ... },
  latin1: { ... },
  ascii: { ... },
  base64: { ... },
  hex: { ... }
```

Trouver dans https://github.com/nodejs/node/blob/master/lib/buffer.js

### Renommer un fichier `rename`

Renommer un dossier :

```js
// rename directory
const dir = path.join(__dirname, "memo");
const newDir = path.join(__dirname, "memonew");

fs.rename(dir, newDir, err => {
    if (err) console.log(err);

    console.log("directory renamed");
});
```



Renommer un fichier :

```js
// rename file
fs.rename(
    path.join(__dirname, "test", "hello.txt"),
    path.join(__dirname, "test", "hellococo.txt"),
    err => {
        if (err) throw err;

        console.log("File renamed !");
    }
);
```

```js
fs.rename(oldName, newName, callback);
```

### vérifier l'existence d'un dossier ou d'un ficher `existsSync`

Méthode synchrone

```js
if (!fs.existsSync(myDir)) {
  fs.mkdir(myDir, {}, err => {
    if (err) throw err;
  });
}
```

```js
if (fs.existsSync(this.loggFile)) {
  fs.appendFile(
    this.loggFile,
    `id: ${uuid.v4()} => message: ${msg}\n`,
    err => {
      if (err) throw err;
    }
  );
}
```

