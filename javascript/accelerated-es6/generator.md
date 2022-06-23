# `generator`

## `generator` définition

C'est une fonction qui ne se termine pas tout de suite, utilisation du mot clé `yield` : produire.

Ce mot fonctionne un peu comme un `return` mais pour plusieurs valeurs.

```js
function *createFruit() {
    yield "banana";
    yield "strawberry";
    yield "pinapple";
}

const it = createFruit();

it.next(); //? { value: 'banana', done: false }
it.next(); //? { value: 'strawberry', done: false }
it.next(); //? { value: 'pinapple', done: false }
it.next(); //?  { value: undefined, done: true }
```

la fonction `generator` renvoie un itérateur.

`function *createFruit()`

`function*createFruit()`

`function * createFruit()`

`function* createFruit()` sont toutes quatre des syntaxes valables.

## Assigner un `generator` à un `iterator`

```js
function*createFruit() {
    yield "banana";
    yield "strawberry";
    yield "pinapple";
}

const o = {
    [Symbol.iterator]: createFruit
};

for(n of o) {
    console.log(n); //? banana strawberry pinapple
}
```

Une fonction `generator` retourne un `iterator`, ce que prend la propriété `[Symbol.iterator]`.

### Ajouter un paramètre

```js
function *gen(end) {
    for(let i =0; i < end; i++) {
        yield i;
    }
}

const it = gen(2);

it.next(); //? { value: 0, done: false }
it.next(); //? { value: 1, done: false }
it.next(); //?  { value: undefined, done: true }
```

### `throw` et `return`

```js
function *gen(end) {
    for(let i =0; i < end; i++) {
        try {

            yield i;
        } catch(e) {
            console.error("my-error", e);
        }
    }
}

const it = gen(2);

console.log(it.next()); //?
console.log(it.throw("boum!")); //?
console.log(it.return("hey coco (^-^)")); //?
console.log(it.next()); //?
```

```bash
{ value: 0, done: false }
my-error boum!
{ value: 1, done: false }
{ value: 'hey coco (^-^)', done: true }
{ value: undefined, done: true }
```

`throw` génère une erreur, mais si celle-ci est gérée par un `try and catch`, la valeur est renvoyée après normalement.

`return` écrase une valeur renvoyée.

## Générateur asynchrone : `async generator`

```js
async function* randomGen(end) {
    for (let i = 0; i < end; i++) {
        await new Promise(resolve => setTimeout(resolve, 600));
        yield Math.floor(Math.random() * 10) + 1;
    }
}

(async function () {
    for await (let nb of randomGen(12)) {
        console.log("nb :", nb);
    }
})();
```

> L'opérateur **`await`** permet d'attendre la résolution d'une promesse ([`Promise`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Promise)). Il ne peut être utilisé qu'au sein d'une fonction asynchrone (définie avec l'instruction [`async function`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Instructions/async_function)).

C'est pour cela qu'il faut entourer sa boucle `for await of` avec une fonction `async` (asynchrone).

