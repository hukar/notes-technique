# 01 Module wrapper function

Lorsqu'on exécute un fichier dans `nodejs`, le code appelé est entouré (wrapped) par une fonction :

```js
(function(exports, require, module, __filename, __dirname) {
  
})
```

exemple :

```js
console.log(`__filename :${__filename}`);
console.log(`__dirname :${__dirname}`);
```

```bash
__filename :/Users/kms/Documents/programmation/node/node-crash-course/index.js
__dirname :/Users/kms/Documents/programmation/node/node-crash-course
```

### ES6 méthode d'import

```js
import Person from "./Person"
```

N'est pas implémentée dans `node`, à la place on a les modules `common js`

## 