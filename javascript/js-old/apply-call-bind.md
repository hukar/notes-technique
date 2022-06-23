#  `call`, `apply` et `bind`

```js
const o = {
    name: "O",
    greet: function(color, animal) {
        console.log(
            `hello I'm ${this.name}, I love ${color}, and my friend is ${animal}`
        );
    }
};

o.greet("blue", "Dolphin");
```

```
hello I'm O, I love blue, and my friend is Dolphin
```



## `call(newThis, par1, par2, ...)` 

`call prend une nouvelle valeur pour `this` et les paramètres de la fonction un par un.

```js
const a = o.greet.call({ name: "Jane Doh" }, "green", "Minou");
```

```
hello I'm Jane Doh, I love green, and my friend is Minou
```

## `apply(newThis, [p1, P2, ...])`

La seule différence avec `call`, c'est que apply prend un tableau de paramètres.

```js
const b = o.greet.apply({ name: "Hubert Reeve" }, ["pink", "Pouicky"]);
```

```
hello I'm Hubert Reeve, I love pink, and my friend is Pouicky
```

## `bind(newThis, [p1, p2, ...])`

`bind` n'exécute pas la fonction mais renvoie une nouvelle fonction ayant `newThis` comme valeur interne de `this` et s'exécutant avec les paramètres passés au tableau.

```js
const c = o.greet.bind({ name: "Maurice" }, ["Yellow", "Giffon"]); //?

c();
```

```
hello I'm Maurice, I love Yellow,Giffon, and my friend is undefined
```

### Valeur de retour

```js
console.log(a);  // call
console.log(b);  // apply
console.log(c);  // bind
```

```
undefined
undefined
[Function: bound greet] # fonction liée à greet
```

