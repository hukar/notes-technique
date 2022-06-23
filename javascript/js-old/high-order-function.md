# Fonction de premier ordre

Elle doit donc :

- Prendres une ou plusieurs fonction en argument
- pouvoir retourner une fonction

## Avec une fonction classique

```js
function add(x) {
    return function(y) {
        return x + y;
    }
}

const add3 = add(3);

add3(5); //? 8
```

## Syntaxe avec les `arrow function`

```js
const addX = x => y => x + y;

add2 = addX(2);

add2(3); //? 5
```

on peu lire pour `x` retourne `=>` la fonction `y => x + y;` pour y retourne x + y

```js
x => (y => x + y);
```

Un exemple avec une fonction en argument :

```js
const lessThan12 = x => x < 12;

const addIfCond = cond => nb => cond(nb) ? nb + 2 : 0;

addIfCond(lessThan12)(5); //? 7
```

## Exemple complet

```js
const has = p => o => o.hasOwnProperty(p);
const sortBy = p => (a, b) => a[p] > b[p] ? 1 : -1;

const users = [
    { name: "Vi", age: 24 },
    { name: "Al", age: 23 },
    { name: "Ro", age: 78, pets: "to" },
    { name: "Th", age: 21 },
    { name: "Mo", age: 83, pets: "ki" },
    { name: "Ma", age: 37 },
    { name: "Lu", age: 31, pets: "po" }
```

```js
const d = users.filter(has("pets")).sort(sortBy("age")); 
//? [ { name: 'Lu', age: 31, pets: 'po' }, { name: 'Ro', age: 78, pets: 'to' }, { name: 'Mo', age: 83, pets: 'ki' } ]

const dd = users.filter(has("pets")).sort(sortBy("name")); 
//? [ { name: 'Lu', age: 31, pets: 'po' }, { name: 'Mo', age: 83, pets: 'ki' }, { name: 'Ro', age: 78, pets: 'to' } ]
```

