# 09 event driven architecture 01

## Pure style `callback` de `Node.js`

```js
const fs = require("fs");

const readFileAsArray = (filename, cb) => {
  fs.readFile(filename, "utf8", (err, data) => {
    if (err) {
      return cb(err);
    }
    const lines = data.toString().split("\n");
    cb(null, lines);
  });
};

readFileAsArray("./numbers", (err, lines) => {
  if (err) {
    return console.log("my error : ", err);
  }

  let numbers = [];

  numbers = lines.map(line => line.split(" "));
  numbers = numbers
    .flat(2)
    .map(Number)
    .filter(nb => nb % 2 === 0);

  console.log(numbers);
});
```

On passe une `callback` en argument, cette `callback` récupère l'erreur en premier (pattern error first) et la `data` en deuxième.

## Style `Promise`

### Modèle des promesses avec les fonctions asynchrones

```js
const myFonction = () => {
    return new Promise((resolve, reject) => {
        asyncFunction(param, (err, data) => {
            if(err) {
                return reject(err);
            }
            
            resolve(data);
        })
    })
}
```

### Le code refactoré

```js
const fs = require("fs");

const readFileAsArray = filename => {
  return new Promise((resolve, reject) => {
    fs.readFile(filename, "utf8", (err, data) => {
      if (err) {
        return reject(err);
      }
      const lines = data.toString().split("\n");
      resolve(lines);
    });
  });
};

readFileAsArray("./numbers")
  .then(lines => {
    let numbers = [];

    numbers = lines.map(line => line.split(" "));
    numbers = numbers
      .flat(2)
      .map(Number)
      .filter(nb => nb % 2 === 0);

    console.log(numbers);
  })
  .catch(console.error);

```

## Style `mixte`

On peut aussi refactored le code pour qu'il fonctionne dans les deux styles :

```js
const fs = require("fs");

const readFileAsArray = (filename, cb = () => {}) => {
  return new Promise((resolve, reject) => {
    fs.readFile(filename, "utf8", (err, data) => {
      if (err) {
        cb(err);
        return reject(err);
      }
      const lines = data.toString().split("\n");
      cb(null, lines);
      resolve(lines);
    });
  });
};
```
on donne une valeur par défaut à la `callback` : `cb =() => {}`.
```js
readFileAsArray("./numbers")
  .then(lines => {
    let numbers = [];

    numbers = lines.map(line => line.split(" "));
    numbers = numbers
      .flat(2)
      .map(Number)
      .filter(nb => nb % 2 === 0);

    console.log(numbers);
  })
  .catch(console.error);
```
Pour le style `Promise`.
```js
readFileAsArray("./text", (err, lines) => {
  if (err) {
    return console.log("my-error :", err);
  }
  lines.forEach((line, index) => console.log("line n°", index, " :", line));
});

```
Pour le style `callback`.

## Style `async - await`

`await` est utilisable seulement dans une fonction précédée par le mot clé `async`.

```js
const evenNumbers = async () => {
  try {
    const lines = await readFileAsArray("./numbes");
    let numbers = [];

    numbers = lines.map(line => line.split(" "));
    numbers = numbers
      .flat(2)
      .map(Number)
      .filter(nb => nb % 2 === 0);

    console.log(numbers);
  } catch (err) {
    console.log("async await error catching", err);
  }
};

evenNumbers();
```

Le mot clé `await` permet d'écrire des traitements asynchrone dans un style synchrone, plus besoin d'utiliser `then` ou une `callback`.