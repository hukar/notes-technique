#  04 `Require`

## `require("something")`

`require` fonctionne en 5 étapes

1. Resolving (résoudre le chemin)
2. Loading
3. Wrapping
4. Evaluating
5. Caching (mise en cache si il y a un deuxième appel du même module)

## `module`

```js
console.log(module);
```

```bash
$ node require.js 
Module {
  id: '.',
  path: '/Users/kms/Documents/programmation/node/node-advanced-buna',
  exports: {},
  parent: null,
  filename: '/Users/kms/Documents/programmation/node/node-advanced-buna/require.js',
  loaded: false,
  children: [],
  paths: [
    '/Users/kms/Documents/programmation/node/node-advanced-buna/node_modules',
    '/Users/kms/Documents/programmation/node/node_modules',
    '/Users/kms/Documents/programmation/node_modules',
    '/Users/kms/Documents/node_modules',
    '/Users/kms/node_modules',
    '/Users/node_modules',
    '/node_modules'
  ]
}
```

```bash
> module. # tab tab
module.__defineGetter__      module.__defineSetter__
module.__lookupGetter__      module.__lookupSetter__
module.__proto__             module.hasOwnProperty
module.isPrototypeOf         module.propertyIsEnumerable
module.toLocaleString        module.toString
module.valueOf               

module._compile              module.constructor
module.load                  module.require

module.children              module.exports
module.filename              module.id
module.loaded                module.parent
module.path                  module.paths
```

dans `module.paths` se trouvent tous les chemins où `require` va chercher le module demandé.

Si on réclame un module qui n'est pas dans ces chemins une erreur est levée :

```js
require("index.js");
```

```bash
$ node require.js 
internal/modules/cjs/loader.js:797
    throw err;
    ^

Error: Cannot find module 'index.js'
Require stack:
- /Users/kms/Documents/programmation/node/node-advanced-buna/require.js
    at Function.Module._resolveFilename (internal/modules/cjs/loader.js:794:15)
    at Function.Module._load (inte # ...
```

Il suffit d'ajouter le dossier où se trouve le module à `.paths` pour pouvoir appeler directement le module :

```js
module.paths.push(__dirname);

// console.log(module);
require("index.js");
```

Maintenant il n'y a plus d'erreur.

On va plutôt ajouter son code au dossier `node_modules` que de ralentir chaque appel à `require` en allongeant la liste des dossiers.

### `require.resolve` juste checker l'existence d'un module

`require.resolve` n'importe pas le fichier, il va juste vérifier que l'accès existe :

```js
require.resolve("index.js");
```

## Ordre de recherche

`require` va toujours chercher du plus proche au plus loin.

```js
console.log("./require.js");
require("mycode.js");
```

```bash
$ node require.js 
./require.js
./node_modules/mycode.js
```

Si maintenant je retire `mycode.js` du dossier `node_modules`, il va le chercher dans les répertoire `node_modules ` plus éloignés.

```bash
$ node require.js 
./require.js
~/node_modules/mycode.js
```

#### On peut mettre son code dans un répertoire avec un fichier `index.js`.

`./require.js`

```js
console.log("./require.js");
require("salut");
```

`./node_modules/salut/index.js`
```js
console.log('./node_modules/salut/index.js');
```

```bash
$ node require.js 
./require.js
./node_modules/salut/index.js
```

#### On peut modifier le fichier chargé par défaut `package.json`

Dans `node_modules/salut`

`start.js`

```js
console.log("./node_modules/salut/start.js");
```

`package.json`

```json
{
    "name": "salut",
    "main": "start.js"
}
```

```bash
$ node require.js 
./require.js
./node_modules/salut/start.js
```

#### `require` retourne `module.exports`
`find.js`
```js
console.log("./lib/find-me/find.js");
console.log(module);

exports.id = "find-me";
```

`require.js`

```js
console.log("./require.js");
console.log(module);

const findMeExport = require("./lib/find-me");
console.log("findMeExport", findMeExport);
```

```bash
findMeExport { id: 'find-me' }
```

#### `setImmediate`

Au moment où on exécute le code, `require.js` n'est pas encore chargé :

```bash
./require.js
Module {
  id: '.',
  path: '/Users/kms/Documents/programmation/node/node-advanced-buna',
  exports: {},
  parent: null,
  filename: '/Users/kms/Documents/programmation/node/node-advanced-buna/require.js',
  loaded: false,
  children: [],
  paths: [
    '/Users/kms/Documents/programmation/node/node-advanced-buna/node_modules',
    '/Users/kms/Documents/programmation/node/node_modules',
    '/Users/kms/Documents/programmation/node_modules',
    '/Users/kms/Documents/node_modules',
    '/Users/kms/node_modules',
    '/Users/node_modules',
    '/node_modules'
  ]
}
```

Pour voire le `module.loaded` à `true`, on utilise `setImmediate` pour se retrouver au prochain moment de l'`event loop`:

`find.js`

```js
setImmediate(() => {
    console.log("./lib/find-me/find.js");
    console.log(module);

    exports.id = "find-me";
});
```

```bash
./lib/find-me/find.js
Module {
  id: '/Users/kms/Documents/programmation/node/node-advanced-buna/lib/find-me/find.js',
  path: '/Users/kms/Documents/programmation/node/node-advanced-buna/lib/find-me',
  exports: {},
  parent: Module {
    id: '.',
    path: '/Users/kms/Documents/programmation/node/node-advanced-buna',
    exports: {},
    parent: null,
    filename: '/Users/kms/Documents/programmation/node/node-advanced-buna/require.js',
    loaded: true,
    children: [ [Circular] ],
    paths: [
      '/Users/kms/Documents/programmation/node/node-advanced-buna/node_modules',
      '/Users/kms/Documents/programmation/node/node_modules',
      '/Users/kms/Documents/programmation/node_modules',
      '/Users/kms/Documents/node_modules',
      '/Users/kms/node_modules',
      '/Users/node_modules',
      '/node_modules'
    ]
  },
  filename: '/Users/kms/Documents/programmation/node/node-advanced-buna/lib/find-me/find.js',
  loaded: true,
  children: [],
  paths: [
    '/Users/kms/Documents/programmation/node/node-advanced-buna/lib/find-me/node_modules',
    '/Users/kms/Documents/programmation/node/node-advanced-buna/lib/node_modules',
    '/Users/kms/Documents/programmation/node/node-advanced-buna/node_modules',
    '/Users/kms/Documents/programmation/node/node_modules',
    '/Users/kms/Documents/programmation/node_modules',
    '/Users/kms/Documents/node_modules',
    '/Users/kms/node_modules',
    '/Users/node_modules',
    '/node_modules'
  ]
}
```

On voit que pour le module `parent` , `loaded` est à `true` => le code est chargé.

```bash
findMeExport {}
```

L'objet `exports` n'a pas reçu sont `id`, tout simplement il ne faut pas utiliser l'object `exports` dans un `timer`, car le flux synchrone ne recevra alors pas son contenu.

## Référence circulaire

`lib/m1.js`

```js
exports.id = "m1";

exports.content = [1];
const m2 = require("./m2"); // loaded true
console.log(m2);

exports.content.push(11);
exports.content.push(111);
```

`lib/m2.js`

```js
exports.id = "m2";

exports.content = [2];
exports.content.push(22);
exports.content.push(222);

const m1 = require("./m1"); // loaded false
console.log("m1 is not loaded yet", m1);
```

`index.js`

```js
const m1 = require("./lib/m1");

console.log(m1);
```

```bash
m1 is not loaded yet { id: 'm1', content: [ 1 ] }
{ id: 'm2', content: [ 2, 22, 222 ] }
{ id: 'm1', content: [ 1, 11, 111 ] }
```

Quand `m2.js` appelle `m1.js`, celui-ci n'est pas totalement chargé.

## `JSON` et `c++ Addons`

Quand on utilise `require`, il va essayer de trouver dans cet ordre :

```
require("something");

1. try something.js
2. try something.json
3. try something.node // binary file
```

### Fichier de configuration `JSON`

`config.json`

```json
{
    "url": "localhost",
    "port": [405, 406, 407],
    "tmp": "./tmp"
}
```

`index.js`

```js
const config = require("./config");

console.log(config);
```

```bash
$ node index.js
{ url: 'localhost', port: [ 405, 406, 407 ], tmp: './tmp' }
```

`Node` parse les données du fichier `json` en `javascript`.

### "hello world" en `c++`

#### Tous les codes sont dans la documentation `node` en ligne

créer un dossier `addon-src`, dedans `hello.cc` le code `c++`

```cpp
// hello.cc
#include <node.h>

namespace demo {

using v8::FunctionCallbackInfo;
using v8::Isolate;
using v8::Local;
using v8::NewStringType;
using v8::Object;
using v8::String;
using v8::Value;

void Method(const FunctionCallbackInfo<Value>& args) {
  Isolate* isolate = args.GetIsolate();
  args.GetReturnValue().Set(String::NewFromUtf8(
      isolate, "world", NewStringType::kNormal).ToLocalChecked());
}

void Initialize(Local<Object> exports) {
  NODE_SET_METHOD(exports, "hello", Method);
}

NODE_MODULE(NODE_GYP_MODULE_NAME, Initialize)

}  // namespace demo
```

le fichier `binding.gyp` pour dire quel fichier *builder*.

```json
{
  "targets": [
    {
      "target_name": "addon",
      "sources": [ "hello.cc" ]
    }
  ]
}
```

ensuite on instale `node-gyp`

```bash
npm i -g node-gyp
```

Puis dans le dossier `addon-src`

```bash
$ cd addon-src
$ node-gyp configure
gyp info it worked if it ends with ok
gyp info using node-gyp@6.1.0
gyp info using node@12.13.0 | darwin | x64
gyp info find Python using Python version 3.8.1 found at "/Library/Frameworks/Python.framework/Versions/3.8/bin/python3"
gyp info spawn /Library/Frameworks/Python.framework/Versions/3.8/bin/python3
gyp info spawn args [
gyp info spawn args   '/usr/local/lib/node_modules/node-gyp/gyp/gyp_main.py',
gyp info spawn args   'binding.gyp',
gyp info spawn args   '-f',
# ...
```

```bash
$ node-gyp build
gyp info it worked if it ends with ok
gyp info using node-gyp@6.1.0
gyp info using node@12.13.0 | darwin | x64
gyp info spawn make
gyp info spawn args [ 'BUILDTYPE=Release', '-C', 'build' ]
  CXX(target) Release/obj.target/addon/hello.o
  SOLINK_MODULE(target) Release/addon.node
gyp info ok
```

On obtient dans le dossier `addon-src/build/release` le fichier `hello.node`.

On copie ce binaire dans le `node_modules`:

```bash
cp ./build/release/addon.node ../node_modules
```

On crée un fichier `index.js` :

```js
const addon = require("addon");

console.log(addon.hello());
```

```bash
$ node index.js
world
```

BOOM on sait jamais si un jour je veux créer des addons en `c++`.

#### Voire les extensions permisent dans `require`

```bash
> require.extensions
[Object: null prototype] {
  '.js': [Function],
  '.json': [Function],
  '.node': [Function],
  '.mjs': [Function]
}
```

`mjs` extension utilisée par `Node` pour distinguer les modules `CommonJ` des modules `ES6`.

```bash
> require.extensions[".js"].toString()
'function(module, filename) {\n' +
  "  if (experimentalModules && filename.endsWith('.js')) {\n" +
  '    const pkg = readPackageScope(filename);\n' +
  "    if (pkg && pkg.type === 'module') {\n" +
  '      throw new ERR_REQUIRE_ESM(filename);\n' +
  '    }\n' +
  '  }\n' +
  "  const content = fs.readFileSync(filename, 'utf8');\n" +
  '  module._compile(stripBOM(content), filename);\n' +
  '}'
```

Pour les fichiers `.js`, `Node` compile le fichier.



```bash
> require.extensions[".json"].toString()
'function(module, filename) {\n' +
  "  const content = fs.readFileSync(filename, 'utf8');\n" +
  '\n' +
  '  if (manifest) {\n' +
  '    const moduleURL = pathToFileURL(filename);\n' +
  '    manifest.assertIntegrity(moduleURL, content);\n' +
  '  }\n' +
  '\n' +
  '  try {\n' +
  '    module.exports = JSON.parse(stripBOM(content));\n' +
  '  } catch (err) {\n' +
  "    err.message = filename + ': ' + err.message;\n" +
  '    throw err;\n' +
  '  }\n' +
  '}'
```

Pour les fichiers `.json`, `Node` `parse` le fichier.



```bash
> require.extensions[".node"].toString()
'function(module, filename) {\n' +
  '  if (manifest) {\n' +
  '    const content = fs.readFileSync(filename);\n' +
  '    const moduleURL = pathToFileURL(filename);\n' +
  '    manifest.assertIntegrity(moduleURL, content);\n' +
  '  }\n' +
  "  // Be aware this doesn't use `content`\n" +
  '  return process.dlopen(module, path.toNamespacedPath(filename));\n' +
  '}'
```

Pour les fichiers `.node`, `Node` utilise `process.dlopen`.