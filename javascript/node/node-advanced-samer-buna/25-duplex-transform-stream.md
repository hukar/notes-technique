# 25 `duplex` et `transform` `stream`

## `Duplex stream` 

Le `duplex stream` implémente à la fois `Readable` et `Writable` (les méthodes `write` et `read`).

```js
const { Duplex } = require("stream");

const inoutStream = new Duplex({
    write(chunk, encoding, callback) {
        console.log(chunk.toString());
        callback();
    },

    read(size) {
        this.push(String.fromCharCode(this.currentCharacterCode++));

        if(this.currentCharacterCode > 90) {
            this.push(null);
        }
    }
});

inoutStream.currentCharacterCode = 65;

process.stdin.pipe(inoutStream).pipe(process.stdout);
```

`process.stdin.pipe(inoutStream).pipe(process.stdout);` on peut donc *piper* en chaîne.

La partie `readable` et la partie `writable` sont complètement indépendantes.

## `Transform stream`

C'est un `Duplex stream` mais plus intéressant.

### Avec `Duplex`

```js
const { Duplex } = require("stream");

const upperCaseTr = new Duplex({
    write(chunk, encoding, callback) {
        this.push(chunk.toString().toUpperCase());
        callback();
    },

    read(size) {

    }
});

process.stdin.pipe(upperCaseTr).pipe(process.stdout);
```



### Simplification de l'écriture avec `Transform`

Pas besoin d'implémenter `read` et ` write`, seulement `transform` qui est une combinaison des deux.

```js
const { Transform } = require("stream");

const upperCaseTr = new Transform({
    transform(chunk, encoding, callback) {
        this.push(chunk.toString().toUpperCase());
        callback();
    }
});

process.stdin.pipe(upperCaseTr).pipe(process.stdout);
```

```bash
node transform.js
hello
HELLO
```

## Utilisation de `zlib`

```js
const fs = require("fs");

const zlib = require("zlib");
const file = process.argv[2];

fs.createReadStream(file)
    .pipe(zlib.createGzip())
    .pipe(fs.createWriteStream(`${file}.gz`));
```

#### `zlib.createGzip()` compresse le `stream` en entrée et le renvoie.

```bash
rm server.js
gunzip server.js.gz
```

On peut récupérer son fichier décompressé avec la commande `gunzip`.

## Mixer `pipe` et `event listener`

### afficher un message à la fin de la compression

```js
const fs = require("fs");

const zlib = require("zlib");
const file = process.argv[2];

fs.createReadStream(file)
    .pipe(zlib.createGzip())
    .pipe(fs.createWriteStream(`${file}.gz`))
    .on("finish", () => console.log("done"));
```

On ajoute un écouteur sur le `stream writable` : `.on("finish", callback)`.

### Indicateur de progression

On peut brancher un indicateur de progression sur le `stream Duplex` :

```js
const fs = require("fs");

const zlib = require("zlib");
const file = process.argv[2];

fs.createReadStream(file)
    .pipe(zlib.createGzip())
    .on("data", () => process.stdout.write("."))
    .pipe(fs.createWriteStream(`${file}.gz`))
    .on("finish", () => console.log("\ndone"));
```

## Composer avec les streams

Au lieu d'utiliser les événement, on peut créer un `Duplex stream`.

On utilise aussi le module `crypto` pour **encrypter** le fichier.

```js
const fs = require("fs");
const crypto = require("crypto");
const zlib = require("zlib");
const { Transform } = require("stream");

const file = process.argv[2];

const progressDisplay = new Transform({
    transform(chunk, encoding, callback) {
        process.stdout.write(".");
        callback(null, chunk);
    }
});

fs.createReadStream(file)
    .pipe(zlib.createGzip())
    .pipe(crypto.createCipher("aes192", "tintin_et_milou"))
    .pipe(progressDisplay)
    .pipe(fs.createWriteStream(`${file}.cpt`))
    .on("finish", () => console.log("\ndone"));

```

```bash
node zip.js bigFile.txt
...................................................................
done
```

### Pour Dézipper

```js
const fs = require("fs");
const zlib = require("zlib");
const crypto = require("crypto");
const { Transform } = require("stream");

const file = process.argv[2];

const progress = new Transform({
    transform(chunk, encoding, callback) {
        process.stdout.write(".");
        callback(null, chunk);
    }
});

fs.createReadStream(file)
    .pipe(crypto.createDecipher("aes192","tintin_et_milou"))
    .pipe(zlib.createGunzip())
    .pipe(progress)
    .pipe(fs.createWriteStream(file.replace(".cpt","")))
    .on("finish", () => console.log("\ndone"));
```

`crypto.createCipher` et `crypto.createDecipher` sont déprécié : 

il faut utiliser ``crypto.createCipheriv(algorithm, key, iv[, options])``.

