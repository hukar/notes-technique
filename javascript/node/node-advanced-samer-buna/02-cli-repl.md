## 02 `CLI` et `REPL`

`R`ead `E`val `P`rint `L`oop

```bash
$ node
Welcome to Node.js v12.13.0.
Type ".help" for more information.
> 
```

### Autocompletion

en appuyant deux fois sur `tab`

```bash
> const arr = [];
undefined 
```

```bash
> arr.
arr.__defineGetter__      arr.__defineSetter__      arr.__lookupGetter__      arr.__lookupSetter__      arr.__proto__
arr.hasOwnProperty        arr.isPrototypeOf         arr.propertyIsEnumerable  arr.valueOf               

arr.concat                arr.constructor           arr.copyWithin            arr.entries               arr.every
arr.fill                  arr.filter                arr.find                  arr.findIndex             arr.flat
arr.flatMap               arr.forEach               arr.includes              arr.indexOf               arr.join
arr.keys                  arr.lastIndexOf           arr.map                   arr.pop                   arr.push
arr.reduce                arr.reduceRight           arr.reverse               arr.shift                 arr.slice
arr.some                  arr.sort                  arr.splice                arr.toLocaleString        arr.toString
arr.unshift               arr.values                

arr.length
```

```bash
> arr.s
arr.shift   arr.slice   arr.some    arr.sort    arr.splice 
```

Si on tape quelques lettres l'autocompletion réduit les choix possibles.

### L'objet `global` est chargé avec plusiseurs modules dans le `repl`.

```bash
> global.
global.__defineGetter__      global.__defineSetter__      global.__lookupGetter__      global.__lookupSetter__      global.__proto__
global.hasOwnProperty        global.isPrototypeOf         global.propertyIsEnumerable  global.toLocaleString        global.toString
global.valueOf               

global.constructor           

global.Array                 global.ArrayBuffer           global.Atomics               global.BigInt                global.BigInt64Array
global.BigUint64Array        global.Boolean               global.Buffer                global.DataView              global.Date
global.Error                 global.EvalError             global.Float32Array          global.Float64Array          global.Function
global.GLOBAL                global.Infinity              global.Int16Array            global.Int32Array            global.Int8Array
global.Intl                  global.JSON                  global.Map                   global.Math                  global.NaN
global.Number                global.Object                global.Promise               global.Proxy                 global.RangeError
global.ReferenceError        global.Reflect               global.RegExp                global.Set                   global.SharedArrayBuffer
global.String                global.Symbol                global.SyntaxError           global.TextDecoder           global.TextEncoder
global.TypeError             global.URIError              global.URL                   global.URLSearchParams       global.Uint16Array
global.Uint32Array           global.Uint8Array            global.Uint8ClampedArray     global.WeakMap               global.WeakSet
global.WebAssembly           global._                     global._error                global.assert                global.async_hooks
global.buffer                global.child_process         global.clearImmediate        global.clearInterval         global.clearTimeout
global.cluster               global.console               global.crypto                global.decodeURI             global.decodeURIComponent
global.dgram                 global.dns                   global.domain                global.encodeURI             global.encodeURIComponent
global.escape                global.eval                  global.events                global.fs                    global.global
global.globalThis            global.http                  global.http2                 global.https                 global.inspector
global.isFinite              global.isNaN                 global.module                global.net                   global.os
global.parseFloat            global.parseInt              global.path                  global.perf_hooks            global.process
global.punycode              global.querystring           global.queueMicrotask        global.readline              global.repl
global.require               global.root                  global.setImmediate          global.setInterval           global.setTimeout
global.stream                global.string_decoder        global.tls                   global.trace_events          global.tty
global.undefined             global.unescape              global.url                   global.util                  global.v8
global.vm                    global.worker_threads        global.zlib 
```

### `_` rappel la dernière valeur

```bash
> Math.random()
0.4282317662865909

> _
0.4282317662865909

> const a = _
undefined

> a
0.4282317662865909
```

On peut même attribuer `_` à une variable.

### Les utilitaires commençant par `.`

```bash
> .
break   clear   editor  exit    help    load    save    

> .help
.break    Sometimes you get stuck, this gets you out
.clear    Alias for .break
.editor   Enter editor mode
.exit     Exit the repl
.help     Print this help message
.load     Load JS from a file into the REPL session
.save     Save all evaluated commands in this REPL session to a file
```

#### `.editor`

Permet d'écrire plusieurs lignes.

`ctrl + d` pour sortir.

```bash
> .editor
// Entering editor mode (^D to finish, ^C to cancel)
function addThree(nb) {
return nb + 3
}
# ctrl + d
undefined 
> addThree(5)
8
```

### `.break`

Si on copie-colle du code non finie, cela nous permet de rattraper le `repl` :

```bash
> function toto(name) {
...     console.log("toto like " + name);
... .break
> 
```

### `.load`

Permet de charger un script :

```bash
> .load /Users/kms/Documents/programmation/node/node-advanced-buna/cli.js
function toto(name) {
    console.log("toto like " + name);
    }
    
undefined
> toto("titi")
toto like titi
```

### `.save`

Permet de récupérer les commandes tapés dans un fichier (enregistré par défaut dans `Users/my_name`).

```bash
> .save session.js
Session saved to: session.js # /Users/kms/session.js
```

## Custom `repl`

On peut customiser le `repl` grâce à sa méthode `.start`.

`myrepl.js`

```js
const repl = require("repl");

const r = repl.start({
    ignoreUndefined: true,
    replMode: repl.REPL_MODE_STRICT
});
```

D'abord dans le `repl` normal :

```bash
$ node
Welcome to Node.js v12.13.0.
Type ".help" for more information.

> let a;
undefined
> a;
undefined
> three = 3
3
```

Maintenant dans le `custom repl` :

```bash
$ node myrepl.js 
> let a; # ignoreUndefined
> a; # ignoreUndefined
> three = 3 # REPL_MODE_STRICT
Thrown:
ReferenceError: three is not defined
> 
```

On peut ajouter des librairies au contexte du `repl `:

```js
const repl = require("repl");

const r = repl.start({/* ... */});

r.context.lodash = require("lodash");
```

```bash
$ npm i lodash
$ node myrepl.js

> lodash.union([5],[5,6,7]);
[ 5, 6, 7 ]
```

## Option de `node`

Pour avoir toutes les options de `node` :

```bash
$ node --help | less
```

### `-c` `--check` 

vérifie la syntaxe sans exécuter le script.

```bash
$ node error.js 
/Users/kms/Documents/programmation/node/node-advanced-buna/error.js:5
console.log(crazy("zozo".tutu);
                             ^

SyntaxError: missing ) after argument list
    at Module._compile (internal/modules/cjs/loader.js:892:18)
    at Object.Module._extensions..js (internal/modules/cjs/loader.js:973:10)
    at Module.load (internal/modules/cjs/loader.js:812:32)
    at Function.Module._load (internal/modules/cjs/loader.js:724:14)
    at Function.Module.runMain (internal/modules/cjs/loader.js:1025:10)
    at internal/main/run_main_module.js:17:11
```

```bash
$ node -c error.js 
/Users/kms/Documents/programmation/node/node-advanced-buna/error.js:5
console.log(crazy("zozo".tutu);
                             ^

SyntaxError: missing ) after argument list
    at new Script (vm.js:84:7)
    at checkSyntax (internal/main/check_syntax.js:78:3)
    at internal/main/check_syntax.js:42:3
```

### `-p` `--print`

évalue le code et l'affiche :

```bash
$ node -p "4 + 4"
8
```

```bash
$ node -p "process.arch"
x64
```

```bash
$ node -p "os.cpus()"
[
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 1650080, nice: 0, sys: 990960, idle: 13103060, irq: 0 }
  },
  # ...
]
```

On peut récupérer les arguments supplémentaire grâce à `process.argv` :

```bash
$ node -p "process.argv" test 33
[ '/usr/local/bin/node', 'test', '33' ]
```

```bash
$ node -p "process.argv.slice(1)" toto 65
[ 'toto', '65' ]
```

