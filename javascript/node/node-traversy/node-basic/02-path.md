## 02 Gérer les chemins `Path` 

```js
const path = require("path"); // core module

// Base file name
console.log(path.basename(__filename));
> path_demo.js

// Directory name
console.log(path.dirname(__filename));
> /Users/kms/Documents/programmation/node/node-crash-course/reference

// File extension
console.log(path.extname(__filename));
> .js

// Create path object
console.log(path.parse(__filename));
> {
  root: '/',
  dir: '/Users/kms/Documents/programmation/node/node-crash-course/reference',
  base: 'path_demo.js',
  ext: '.js',
  name: 'path_demo'
}

// Concatenate paths
// ../test/hello.html
console.log(path.join(__dirname, "test", "hello.html"));
> /Users/kms/Documents/programmation/node/node-crash-course/reference/test/hello.html
```

`path.join` est utile pour créer des chemins juste sur n'importe quelle machine (Windows, Unix)

## 