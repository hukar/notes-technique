# 04 bis différence entre `exports` et `module.exports`

## exemple avec `exports`

`add.js`

```js
function add(a, b) {
  return a + b;
}

exports.add = add;
console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

`index.js`

```js
const { add } = require("./add");

console.log(add(5, 6));
console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

```bash
[nodemon] starting `node index.js`
exports : { add: [Function: add] } # le exports de add.js
module.exports : { add: [Function: add] } # le module.exports de add.js
11
exports : {} # le exports de index.js
module.exports : {} # le module.exports de index.js
```

### `exports` ne peut pas être une fonction

`add.js`

```js
exports = function add(a, b) {
  return a + b;
};

console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

`index.js`

```js
const add = require("./add");

console.log(add(5, 6));
console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

```bash
exports : [Function: add]
module.exports : {}
/Users/kar/Documents/programmation/node/upload-scratch/index.js:3
console.log(add(5, 6));
            ^

TypeError: add is not a function
```

## Avec `module.exports`

### Première syntaxe

`add.js`

```js
module.exports.add = function add(a, b) {
  return a + b;
};

console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

`index.js`

```js
const { add } = require("./add");

console.log(add(5, 6));
console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

```bash
exports : { add: [Function: add] }
module.exports : { add: [Function: add] }
11
exports : {}
module.exports : {}
```

### Deuxième syntaxe

`add.js`

```js
module.exports = function add(a, b) {
  return a + b;
};

console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

Ici on assigne directement la fonction à `module.exports`.

`index.js`

```js
const add = require("./add");

console.log(add(5, 6));
console.log("exports :", exports);
console.log("module.exports :", module.exports);
```

```bash
exports : {} # on voie que exports reste vide
module.exports : [Function: add]
11
exports : {}
module.exports : {}
```

L'objet `exports` n'est pas touché dans ce cas, alors que `module.exports` devient une fonction.

## Explication

`require` retourne `module.exports` pas `exports`.

Si au départ on a `exports = module.exports = {}`, si on assigne une fonction à `exports`, celui-ci ne pointe plus sur `module.exports` et `require` renvoie un objet vide `{}`.

### Code de la documentation officielle

```js
function require(/* ... */) {
  const module = { exports: {} };
  ((module, exports) => {
      
    // Le code de notre module, ici on défini une fonction
    function someFunc() {}
      
    exports = someFunc;
    // à ce point, "exports" n'est plus un raccourci vers "module.exports",
    // ce module va exporter un objet vide par défaut.
      
    module.exports = someFunc;
    // à ce point, le module va maintenant exporter la fonction someFunc,
    // à la place de l'objet vide par défaut.
  })(module, module.exports);
    
  return module.exports;
}
```

