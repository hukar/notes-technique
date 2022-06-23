# 02 Modern Javascript

`var` : la portée est `Function Scope`.

`let` : la portée est `Block Scope`.



## `this` et `arrow function`

```js
const toto = {
  speak01: function() {
    console.log(this);
    console.log(`my name is  ${this.name}`);
  },
  speak02: () => {
    console.log(this);
    console.log(`my name is ${this.name}`);
  },
  name: "toto"
}

toto.speak01();
```

```bash
{
  speak01: [Function: speak01],
  speak02: [Function: speak02],
  name: 'toto'
}
my name is  toto
```

```js
toto.speak02()
```

```bash
{
  module: Module {
    id: '.',
    path: '/home/runner/SparseDecentField',
    // ... objet global
}
my name is undefined
```

Une `arrow function` n'a pas sa propre valeur de `this`.

Celui ci vaut donc ce qu'il est défini dans le scope où l'`arrow function` est invoquée.



## Objet littéral

```js
const mystery = "pocoloco";
const inverseOfPi = 1 / Math.PI;

const toto = {
  p1: 20,
  p2: 20,
  f1() {
    // ...
  },
  f2: () => {
    // ...
  },
  [mystery + 123]: 99,
  inverseOfPi,
}

console.log(inverseOfPi);
console.log(toto.mystery);
console.log(toto.pocoloco123);
```

```
0.3183098861837907
undefined
99
```

- écriture de méthode simplifié
- propriété calculée `[...]`
- syntaxe courte pour assigner une variable ayant le même nom qu'une propriété



## Destructuring

Pour les tableaux, on peut laisser des espaces vides avec les virgules et ainsi sauter des éléments :

```js
const [first,,,forth] = [1, 2, 3, 4];

console.log(first, forth);
```

```
1 4
```

#### Spread operator `...`

```js
const [firstItem, ... restOfItems] = [1, 2, 3, 4];
console.log(firstItem);
console.log(restOfItems);
```

```
1
[2, 3, 4]
```

### avec des objet

```js
const a = {
  name: "Polo",
  age: 39,
  hobbies: [
    "tennis",
    "cooking"
  ]
};

const {name,...otherProperties} = a;

console.log(otherProperties);
```

```
{ age: 39, hobbies: [ 'tennis', 'cooking' ] }
```

#### Copie de tableau et d'objet

```js
const a = {
  name: "Polo",
  age: 39,
  hobbies: [
    "tennis",
    "cooking"
  ]
};

const copieA = {...a};
```

```js
const b = [3, "titi", {"name": "toto"}];

const copieB = [...b];
```

#### ! la copie n'est qu'au premier niveau

```js
copieA.name = "Killa Robot 🤖";
copieA.hobbies.push("Killing dolphin 💀");
console.log(a);
```

```bash
{
  name: 'Polo',
  age: 39,
  hobbies: [ 'tennis', 'cooking', 'Killing dolphin 💀' ]
}
```

`hobbies` étant une référence, il est partagé par `a` et `copieA`.

```js
copieB[0] = 46;
copieB[2].name = "burger joe 🍔";

console.log(b);
console.log(copieB);
```

```bash
[ 3, 'titi', { name: 'burger joe 🍔' } ]
[ 46, 'titi', { name: 'burger joe 🍔' } ]
```



## Syntax du constructeur

```js
console.log(new Date);
console.log(new Date());
```

```bash
2020-08-14T05:51:06.620Z
2020-08-14T05:51:06.620Z
```

En javascript les parenthèses du constructeur sont facultatives si il n'y a pas de paramètres.



## Class

```js
class Person {
  constructor(name) {
    this.name = name;
  }
  
  greet() {
    console.log(`hello I'm ${this.name}`);
  }
}

class Student extends Person {
  constructor(name, level) {
    super(name);
    this.level = level;
  }
  
  greet() {
    console.log(`hello my name is ${this.name} and my level is ${this.level}`);
  }
}

const o1 = new Person("Mouch");
const o2 = new Student("Pola", "1st Grade");
const o3 = new Student("Toby", "2nd Grade");
o3.greet = () => console.log("I'm special 😈");

o1.greet();
o2.greet();
o3.greet();
```

```
hello I'm Mouch
hello my name is Pola and my level is 1st Grade
I'm special 😈
```



## Asynchronous

### `then`

```js
const fetchData = () => {
  fetch("https://api.github.com").then(resp => {
    resp.json().then(data => {
      console.log(data);
    });
  });
};

fetchData();
```

Cela crée des imbrication difficiles à lire.

### `await` `async`

```js
const fetchData = async () => {
  const resp = await fetch("https://api.github.com");
  const data = await resp.json();
  
  console.log(data);
};

fetchData();
```

Permet d'écrire du code asynchrone dans un style synchrone.

