# Rappel sur la valeur de `this ` en javascript

Voic quelques exemples :

## Fonction Constructeur

```js
function Dog(name, age) {
    this.name = name;
    this.age = age;

    console.log("Dog");
    console.log(this);

    this.ouaf = function () {
        console.log("ouaf");
        console.log(this);
    }
}

const d = new Dog("doggy", 45);

d.ouaf();
```

```
Dog
Dog { name: 'doggy', age: 45 }
ouaf
Dog { name: 'doggy', age: 45, ouaf: [Function] }
```

Dans le cas d'une fonction constructeur `this`  pointe sur l'objet créé.

Avec une ***arrow function*** aussi.

```js
function Cat(name) {
    this.name = name;

    console.log("cat");
    console.log(this);


    this.miaou = () => {
        console.log("miaou");
        console.log(this);
    }
}
```

## Fonction normale

Ici il y a une différence à utiliser `"use strict"` ou non.

### Sans `"use strict"`

```js
function nothing() {
    console.log("nothing");
    console.log(this);
}

nothing();
```

```
nothing
Object [global] {
  DTRACE_NET_SERVER_CONNECTION: [Function],
  DTRACE_NE ...
```

Si on utilise `"use strict"` on obtient :

```
undefined
```





Avec les ***arrow function*** :

```js
const titi = () => {
    console.log("titi");
    console.log(this);
}

titi();
```

`this ` pointe sur le contexte englobant

```
titi
{}
```

## Une fonction dans une méthode

```js
function Dog(name, age) {
    this.name = name;
    this.age = age;

    console.log("Dog");
    console.log(this);

    function anonymous() {
        console.log("anonymous function");
        console.log(this);
    }

    const arrow = () => {
        console.log("arrow function");
        console.log(this);
    };

    this.ouaf = function () {
        console.log("ouaf !");
        anonymous();
        arrow();
    }
}

const d = new Dog("Booga", 11);

d.ouaf();
```

```
Dog
Dog { name: 'Booga', age: 11 }
ouaf !
anonymous function
Object [global] {
  DTRACE_NET_SERVER_CONNECTION: [Function],
  DTRACE_NET_STRE....
  
arrow function
Dog { name: 'Booga', age: 11, ouaf: [Function] }
```

On voit ici que le contexte englobant de l'arrow function est l'objet créé par Dog, this a donc une valeur plus attendu.

De nouveau dans une fonction anonyme, `this ` pointe sur l'objet global.

### Avec `"use strict";`

```
Dog
Dog { name: 'Booga', age: 11 }
ouaf !
anonymous function
undefined
arrow function
Dog { name: 'Booga', age: 11, ouaf: [Function] }
```

`this` prend la valeur `undefined`.

## Exemple de la doc react

En javascript les deux exemples ci-dessous ne sont pas équivalent

```js
const test = {
  name: "jojo",
  hello() {
    return `hello I'm ${this.name}`;
  }
};

test.hello(); //? hello I'm jojo
```

```js
const hello = test.hello;
hello(); //? hello I'm undefined 
```

Il faut lier l'objet :

```js
const hello = test.hello.bind(test);
hello(); //? hello I'm jojo
```

