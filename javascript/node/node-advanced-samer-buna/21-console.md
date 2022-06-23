# 21 Console

```js
> console
{
  log: [Function: bound consoleCall],
  warn: [Function: bound consoleCall],
  dir: [Function: bound consoleCall],
  time: [Function: bound consoleCall],
  timeEnd: [Function: bound consoleCall],
  timeLog: [Function: bound consoleCall],
  trace: [Function: bound consoleCall],
  assert: [Function: bound consoleCall],
  clear: [Function: bound consoleCall],
  count: [Function: bound consoleCall],
  countReset: [Function: bound consoleCall],
  group: [Function: bound consoleCall],
  groupEnd: [Function: bound consoleCall],
  table: [Function: bound consoleCall],
  debug: [Function: bound consoleCall],
  info: [Function: bound consoleCall],
  dirxml: [Function: bound consoleCall],
  error: [Function: bound consoleCall],
  groupCollapsed: [Function: bound consoleCall],
  Console: [Function: Console],
  profile: [Function: profile],
  profileEnd: [Function: profileEnd],
  timeStamp: [Function: timeStamp],
  context: [Function: context],
  // ... Symbols
}
```

## `console.Console` 

Une classe `Console` est disponible pour attribuer un `stream` soit même à `console`.

`console` est une instance configurée sur `stdout` et `stderr` de la classe `Console`.

Si on veut diriger `console` plutôt vers un `socket` ou vers des fichiers on va instancier soit même la classe `Console`.

```js
const fs = require("fs");

const out = fs.createWriteStream("./out.txt", "utf8");
const err = fs.createWriteStream("./err.txt", "utf8");

// out et err sont des wrtitableStream
const myconsole = new console.Console(out, err);

myconsole.log("hello ");
myconsole.log(" kitty ...");

myconsole.error("aye !!");
myconsole.error("ouye");
```

cela va créer un fichier `out.txt` et `err.txt`

`out.txt`

```
hello 
 kitty ...
```

`err.txt`

```
aye !!
ouye
```

## `util`

La classe `Console` utilise le module `util` et `util.format` pour l'affichage qui est une copie de `printf`.

```js
> console.log("%s a mangé %d tartines","Rosa", 567)
Rosa a mangé 567 tartines

> util.format("%s a mangé %d tartines","Rosa", 567)
'Rosa a mangé 567 tartines'
```

`%j` pour retourner du `json`.

Pour les objet `console.log` utilise `util.inspect`

```js
> console.log(module)
Module {
  id: '<repl>',
  path: '.',
  exports: {},
  parent: undefined,
  filename: null,
  loaded: false,
  children: [],
  paths: [
    // ...
  ]
}
undefined
> util.inspect(module)
'Module {\n' +
  "  id: '<repl>',\n" +
  "  path: '.',\n" +
  '  exports: {},\n' +
  '  parent: undefined,\n' +
  '  filename: null,\n' +
  '  loaded: false,\n' +
  '  children: [],\n' +
  '  paths: [\n' +
	// ...
  '  ]\n' +
  '}'
> 
```

## `console.dir(obj, { depth: n })`

`util.inpect` permet de précisé la profondeur d'inspection des objets en options.

Il faut utiliser `console.dir` pour avoir la même option :

```js
> util.inspect(global, {depth: 0})
'Object [global] {\n' +
  '  global: [Circular],\n' +
  '  clearInterval: [Function: clearInterval],\n' +
  '  clearTimeout: [Function: clearTimeout],\n' +
  '  setInterval: [Function: setInterval],\n' +
  '  setTimeout: [Function],\n' +
  '  queueMicrotask: [Function: queueMicrotask],\n' +
  '  clearImmediate: [Function: clearImmediate],\n' +
  '  setImmediate: [Function]\n' +
  '}'
> console.dir(global, {depth: 0})
Object [global] {
  global: [Circular],
  clearInterval: [Function: clearInterval],
  clearTimeout: [Function: clearTimeout],
  setInterval: [Function: setInterval],
  setTimeout: [Function],
  queueMicrotask: [Function: queueMicrotask],
  clearImmediate: [Function: clearImmediate],
  setImmediate: [Function]
}
```

## `console.info`

Un alias de `console.log`.

## `console.error`

Fonctionne comme `console.log` mais en se dirigeant vers `stderr` au lieu de `stdout`.

## `console.warn`

C'est un alias de `console.error`.

## `console.assert`

Teste une affirmation (vrai ou fausse) :

```js
> console.assert(3 == "3")
undefined

> console.assert(3 === "3")
Assertion failed
undefined
```

## `console.trace`

Affiche la pile d'appelle :

```js
> console.trace("heho")
Trace: heho
    at repl:1:9
    at Script.runInThisContext (vm.js:126:20)
    at REPLServer.defaultEval (repl.js:401:29)
    at bound (domain.js:420:14)
    at REPLServer.runBound [as eval] (domain.js:433:12)
    at REPLServer.onLine (repl.js:717:10)
    at REPLServer.emit (events.js:215:7)
    at REPLServer.EventEmitter.emit (domain.js:476:20)
    at REPLServer.Interface._onLine (readline.js:316:10)
    at REPLServer.Interface._line (readline.js:693:8)
```

## `console.time`

Affiche le temps écoulé entre les deux appelles :

```js
> console.time("boo")
undefined
> console.timeEnd("boo")
boo: 8942.627ms
undefined
```

## `util.deprecate`

Entourer une fonction avec `util.deprecate` permet d'ajouter un `warning` lors de l'exécution de cette fonction :

```js
const util = require("util");

const oldPuts = (...args)=> {
    for(let i = 0; i < args.length; i++) {
        process.stdout.write(args[i] + "\n");
    }
}
const puts = util.deprecate(oldPuts
, "deprecated : prefer console.log");

puts("hello","toto");
```

```bash
node puts.js
hello
toto
(node:11021) DeprecationWarning: deprecated : prefer console.log
```

