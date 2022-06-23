# 24 create `stream`

## `Writable stream`

On va utiliser le constructeur pour instancier un nouveau `writable stream`.

```js
const { Writable } = require("stream");

const outStream = new Writable({
    write(chunk, encoding, callback) {
        console.log("chunk : ", chunk.toString());
        callback();
    }
});

process.stdin.pipe(outStream);
```

`chunk` est un buffer.

`encoding` est optionnel.

C'est un `stream` d'écho.

```bash
node create-stream.js 
hello
chunk :  hello

titi
chunk :  titi
```

Ce qui revient au même que de faire :

```js
process.stdin.pipe(process.stdout);
```

#### `readable.pipe(writable)`

C'est la règle générale un `stream` en lecture peut *piper* un `stream` en écriture .



## `Readable stream`

```js
const { Readable } = require("stream");

const inStream = new Readable();

inStream.push("hello\n");

inStream.push("Kitty\n");

inStream.push(null);

console.log(inStream.read(5).toString());

setTimeout(() => {
    console.log(inStream.read(5).toString());
}, 1000);

setTimeout(() => {
    console.log(inStream.read(5).toString());
}, 2000);
```

#### `readable.push(data)`

Ajoute des données au `stream`.

#### `readable.read(size): buffer|null` 

Renvoie la quantité de données défini par `size` (optionnel) en Bytes, soit sous la forme d'un `buffer` soit `null` s'il n'y a plus de données.

## Exemple du cours

```js
const { Readable } = require("stream");

const inStream = new Readable({
    read(size) {
        setTimeout(() => {
            this.push(String.fromCharCode(this.currentCharacterCode++));
            if (this.currentCharacterCode > 90) {
                this.push(null);
            }
        }, 60);
    }
});

inStream.currentCharacterCode = 65;

inStream.pipe(process.stdout);

process.on("exit", () => {
    console.error(
        `\n\nthe current character is ${String.fromCharCode(
            inStream.currentCharacterCode - 1
        )}`
    );
});

process.stdout.on("error", process.exit);
```

On implémente la fonction `read(size)` dans le constructeur.

#### `process.on("exit", (code) => {})`

Permet d'exécuter du code à la sortie du processus.

#### `process.stdout.on("error", callback)`

Permet de gérer les erreurs de la sortie.

```bash
node readable.js | head -c3
ABC

the current character is D
```

`head -c3` permet de lire seulement les trois premiers caractères.

