# 10 event driven architecture 2

## event emitter

Beaucoup de classes dans `Node.js` héritent de `events`.

```js
const events = require("events"); // import

class Logger extends events {} // extends

const logger = new Logger(); // init

logger.on("bip", () => { console.log("someone sy bip !"); });  // add listener

logger.emit("bip"); // emit
```



## Code synchrone



```js
const EventEmitter = require("events");

class WithLog extends EventEmitter {
  execute(taskFunc) {
    console.log("before executing");
    this.emit("begin");
    taskFunc();
    this.emit("end");
    console.log("after executing");
  }
}

whitLog.on("begin", () => console.log("about to execute"));
whitLog.on("end", () => console.log("done to execute"));

whitLog.execute(() => {
  console.log("***=== executing task ===***");
});
```

```
node sync-event.js 

before executing
about to execute
***=== executing task ===***
done to execute
after executing
```

## Code asynchrone

```js
const fs = require("fs");
const EventEmitter = require("events");
```

```js
class WithTime extends EventEmitter {
  execute(asyncFunc, ...args) {
    console.time("execute");
    this.emit("begin");
    asyncFunc(...args, (err, data) => {
      if (err) {
        return this.emit("error", err);
      }

      this.emit("data", data);
      console.timeEnd("execute");
      this.emit("end");
    });
  }
}
```
```js
const withTime = new WithTime();

withTime.on("begin", () => console.log("about to execute"));
withTime.on("end", () => console.log("done to execute"));

withTime.execute(fs.readFile, __filename);
```

```
about to execute
execute: 7.087ms
done to execute
```

### Pour mesurer la durée d'une exécution de code

`console.time("name_flag")`, puis à la fin de ce qu'on veut mesurer `console.timeEnd("name_flag")`.

## `eventEmitter.on("eventName",...args)`

Dans la fonction `.on`, on peut passer autant d'argument que l'on veut, ils seront reçus par la fonction `callback`.

```js
this.emit("data", data, "an other argument");
```

et plus loin :

```js
withTime.on("data", (data, secretArgument) => {
  console.log("data length", data.length);
  console.log("secret argument", secretArgument);
});
```

On retrouve les deux arguments dans la fonction `callback`

## La gestion des erreurs

Si on ne veut pas que le script soit interrompu, on doit gérer l'objet `error`

### sans gestion

```js
withTime.execute(fs.readFile, "");
withTime.execute(fs.readFile, __filename);
```

```
node async-events.js 

(node:53192) Warning: Label 'execute' already exists for console.time()
events.js:187
      throw er; // Unhandled 'error' event
      ^

Error: ENOENT: no such file or directory, open ''
Emitted 'error' event on WithTime instance at:
    at ReadFileContext.callback (/Users/kar/Documents/programmation/node/nodejs-advanced/event-driven/async-events.js:10:14)
    at FSReqCallback.readFileAfterOpen [as oncomplete] (fs.js:250:13) {
  errno: -2,
  code: 'ENOENT',
  syscall: 'open',
  path: ''
}
```

Une erreur est levée et le code s'arrête au premier appelle à `withTime.execute`.

### Avec gestion

```js
withTime.on("error", err => {
  console.log("my-error", err);
});
```

```js
withTime.execute(fs.readFile, "");
withTime.execute(fs.readFile, __filename); 
```

```
node async-events.js 

(node:53206) Warning: Label 'execute' already exists for console.time()
my-error [Error: ENOENT: no such file or directory, open ''] {
  errno: -2,
  code: 'ENOENT',
  syscall: 'open',
  path: ''
}

data length 746
secret argument an other argument
execute: 10.699ms
```

On voit que le traitement contenu malgré l'erreur levée.

### Alternative `process.on("uncaughtException", cb)`

```js
process.on("uncaughtException", err => {
  console.log("my-error", err);

  process.exit(123);
});

process.on("exit", code => {
  console.log("code", code);
});
```

Bien penser à sortir du processus `Node.js` avec `process.exit(code)`.

Comme on sort du processus qu'une seule fois on devrait plutôt utiliser `.once` au lieu de `.on`.

### Exemple avec `.once`

```js
const EventEmitter = require("events");

class CreateAnimal extends EventEmitter {
  animal = [];

  addCorn() {
    this.emit("corn", this.animal);
  }

  ejectAnimal() {
    return this.animal;
  }
}

const createChimere = new CreateAnimal();
const createUnicorn = new CreateAnimal();

createChimere.on("corn", animal => {
  animal.push("corn");
});

createUnicorn.once("corn", animal => {
  animal.push("corn");
});

createChimere.addCorn();
createChimere.addCorn();
createChimere.addCorn();

createUnicorn.addCorn();
createUnicorn.addCorn();
createUnicorn.addCorn();

console.log(createChimere.ejectAnimal());
console.log(createUnicorn.ejectAnimal());
```

```js
[ 'corn', 'corn', 'corn' ]
[ 'corn' ]
```

S'il y a plusieurs `callback` associées à un événement, elle sont appelées dans l'ordre d'ajout :

```js
createUnicorn.once("corn", animal => {
  animal.push("corn");
});

createUnicorn.once("corn", animal => {
  console.log("animal ::", animal);
});

createUnicorn.addCorn();

console.log("eject animal :", createUnicorn.ejectAnimal());
```

```js
animal :: [ 'corn' ]
eject animal : [ 'corn' ]
```

On peut associer au même événement un `callback` sur `.on` et une autre sur `.once` :

```js
createUnicorn.once("corn", animal => {
  animal.push("corn");
});

createUnicorn.on("corn", animal => {
  console.log("animal ::", animal);
});

createUnicorn.addCorn();
createUnicorn.addCorn();

console.log("eject animal :", createUnicorn.ejectAnimal());
```

```js
animal :: [ 'corn' ]
animal :: [ 'corn' ]
eject animal : [ 'corn' ]
```



## `.prependListener`

Ajoute un `listener` au début de la liste des traitements :

```js
createUnicorn.on("corn", animal => {
  animal.push("corn");
});

// ajouter un traitement au début
createUnicorn.prependListener("corn", animal => {
  console.log("ajout d'une couleur pour la corne");
  animal.push("silver");
});

createUnicorn.addCorn();

console.log("eject animal :", createUnicorn.ejectAnimal());
```

```
ajout d'une couleur pour la corne
eject animal : [ 'silver', 'corn' ]
```

On voit que la couleur déclarée après dans la liste des `listener` est bien ajoutée au début.

## `.removeListener`

Supprimer un `listener`.

Il faut une référence vers le traitement pour pouvoir le retirer :

```js
// on crée des références vers les différents traitements
const trait1 = animal => {
  animal.push("corn");
};

const trait2 = animal => {
  console.log("ajout d'une couleur pour la corne");
  animal.push("silver");
};

createUnicorn.on("corn", trait1);

// ajouter un traitement au début
createUnicorn.prependListener("corn", trait2);

createUnicorn.addCorn();

createUnicorn.removeListener("corn", trait1);

createUnicorn.addCorn();

console.log("eject animal :", createUnicorn.ejectAnimal());
```

```
ajout d'une couleur pour la corne
ajout d'une couleur pour la corne
eject animal : [ 'silver', 'corn', 'silver' ]
```

On voit que le traitement d'ajout d'une corne a été supprimé

### `createUnicorn.removeListener("corn", trait1);`

