# `iterator` 

## `iterator` définition

C'est un objet sur lequel on peut appliquer une boucle (itérer en boucle).

## Utilisation des `iterator`

```js
const arr = [1, 2, 3];

console.log(typeof arr[Symbol.iterator]); // function

console.log(arr[Symbol.iterator].toString()); // function values() { [native code] }
```

On peut assigner l'exécution de cette fonction :

```js
const it = arr[Symbol.iterator]();
```

Cette fonction retourne un itérateur du tableau :

```js
console.log(it); // Object [Array Iterator] {}
```

Cet objet possède une unique méthode `next`.

```js
console.log(it.next()); // { value: 1, done: false }
console.log(it.next()); // { value: 2, done: false }
```

Si on exécute encore deux fois `next`:

```js
console.log(it.next()); // { value: 3, done: false }
console.log(it.next()); // { value: undefined, done: true }
```

Il faut dépasser la dernière valeur pour avoir `done` à `true`.

### Modifier le comportement de `next`

```js
arr[Symbol.iterator] = () => ({
    next: () => ({
            value: 10,
            done: true
        })
    });
```

```js
const it = arr[Symbol.iterator]();

console.log(it.next()); // { value: 10, done: true }
console.log(it.next()); // { value: 10, done: true }

console.log(it.next()); // { value: 10, done: true }
console.log(it.next()); // { value: 10, done: true }
```

Maintenant le résultat est toujours ` { value: 10, done: false }`.

On peut développer une logique plus complexe :

```js
const arr = [1, 2, 3];

arr[Symbol.iterator] = function () {
    let nextValue = 10;

    return {
        next() {
            nextValue++;

            return {
                done: nextValue > 15,
                value: nextValue,
            };
        },
    };
};

for (let nb of arr) {
    console.log(nb);
}
```

La boucle `for ... of ...` fonctionne sur les itérables uniquement.

```bash
11
12
13
14
15
```

## Créer un `custom iterateable object`

### Le pattern

```js
const obj = {
    [Symbol.iterator]: function() {
        
        return {
            next: function() {
                
                return {
                    done: boolean,
                    value: val
                }
            }
        }
    }
}
```

`Symbol.iterator` est un identifiant unique permettant de dire si un objet est itérable ou non.

### Implémentation réel

```js
const person = {
    name: "hukar",
    hobbies: ["tennis", "cooking"],
    [Symbol.iterator]: function () {
        let i = 0;
        const hobbies = this.hobbies;
        return {
            next: function () {
                return {
                    done: i >= hobbies.length,
                    value: hobbies[i++],
                };
            },
        };
    },
};

for (let elt of person) {
    console.log(elt);
}
```

```bash
tennis
cooking
```

En utilisant les `itérateur` :

```js
const person = {
    name: "hukar",
    hobbies: ["tennis", "cooking"],
    [Symbol.iterator]: function () {
        const it = this.hobbies[Symbol.iterator]();

        return {
            next: () => it.next(),
        };
    },
};

for (let elt of person) {
    console.log(elt);
}
```

```bash
tennis
cooking
```

On récupère l'itérateur de `hobbies` avec `this.hobbies[Symbol.iterator]()` et on utilise sa méthode `next`.

