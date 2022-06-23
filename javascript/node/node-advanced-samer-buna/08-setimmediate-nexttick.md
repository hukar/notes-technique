# 08 `setImmediate` et `nextTick`

## `SetImmediate` et `setTimeout`

`seImmediate` est une sorte de `setTimeout` de 0 seconde.

Il est prévalant à `setTimeout`.

```js
setTimeout(() => {
  console.log("set timeout 1");
}, 0);

setImmediate(() => {
  console.log("set Immediate 1");
}, 0);

setTimeout(() => {
  console.log("set timeout 2");
}, 0);

setImmediate(() => {
  console.log("set Immediate 2");
}, 0);
```

```bash
set Immediate 1
set Immediate 2
set timeout 1
set timeout 2
```

## Fonction synchrone et asynchrone

```js
const fs = require("fs");

const filesize = (filename, cb) => {
  if (typeof filename !== "string") {
    return cb(new TypeError("argument filename must be a string"));
  }

  fs.stat(filename, (err, stats) => {
    if (err) {
      return cb(err);
    }

    return cb(null, stats.size);
  });
};

filesize(__filename, (err, size) => {
  if (err) {
    return console.log(`my-error : ${err}`);
  }
  console.log(`filesize ${__filename} : ${size / 1024} KB`);
});

console.log("hello");
```

`fs.stat` est une fonction asynchrone qui va renvoyer l'objet `stats` avec toutes les infos sur le fichier.

`stats.size` étant la taille du fichier en Octet (diviser par 1024 pour avoir la taille en Kilo Octet).

Cette fonction est asynchrone si tout se passe bien :

```bash
node file-size.js 

hello
filesize /Users/kar/Documents/programmation/node/nodejs-advanced/wrapping-catching/file-size.js : 0.4765625 KB
```

Mais si jamais on passe un mauvais argument :

```js
filesize(123, (err, size) => {
  if (err) {
    return console.log(`my-error : ${err}`);
  }
  console.log(`filesize ${__filename} : ${size / 1024} KB`);
});

console.log("hello");
```

```bash
node file-size.js 
my-error : TypeError: argument filename must be a string
hello
```

L'erreur est traitée avant le `console.log` de `hello`, la fonction s'exécute de manière synchrone.

## `process.nextTick` pour rendre la fonction totalement asynchrone

`process.nextTick` s'exécute de manière asynchrone avant que l'`event loop` aille à une autre phase (avant toutes choses, en premier lieu).

```js
const filesize = (filename, cb) => {
  if (typeof filename !== "string") {
    return process.nextTick(
      cb,
      new TypeError("argument filename must be a string")
    );
  }

  fs.stat(filename, (err, stats) => {
    if (err) {
      return cb(err);
    }

    return cb(null, stats.size);
  });
};
```

```bash
node file-size.js 
hello
my-error : TypeError: argument filename must be a string
```

### `process.nextTick(cb, ...args)`

Notre fonction est totalement asynchrone.

### `process.nextTick` est exécuté immédiatement.

### `setImmediate` est le prochain 'truck' à être exécuté.

```js
process.stdout.write("START\n\n");

process.nextTick(() => {
  process.stdout.write("next tick 1 \n");
});

setImmediate(() => {
  process.stdout.write("set immediate 1 \n");
});
setImmediate(() => {
  process.stdout.write("set immediate 2 \n");
});
setImmediate(() => {
  process.stdout.write("set immediate 3 \n");
});

process.stdout.write("MIDDLE\n\n");

process.nextTick(() => {
  process.stdout.write("next tick 2 \n");
});

setImmediate(() => {
  process.stdout.write("set immediate 4 \n");
});

process.nextTick(() => {
  process.stdout.write("next tick 3 \n");
});

process.stdout.write("END\n\n");
```

```bash
node next-tick.js 
START

MIDDLE

END

next tick 1 
next tick 2 
next tick 3 
set immediate 1 
set immediate 2 
set immediate 3 
set immediate 4 
```

On abien d'abord le flux synchrone, puis tous les `process.nexTick` et puis tous les `setImmediate`.