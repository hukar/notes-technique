# `symbol`

C'est un nouveau type primitif de javascript.

C'est un identifiant unique, il ne peut pas être réécrit.

Il n'est pas non plus itérable.

```js
let symbol = Symbol("string about me");

symbol;

console.log(symbol);
// Symbol(string about me)

console.log(typeof symbol);
// "symbol"

let anotherSymbol = Symbol("string about me");
console.log(symbol == anotherSymbol);
// false
```

## Utilisation dans un objet `[symbol]`

Les propriétés dont le nom est de type symbol ne sont pas itérable :

```js
let symbol = Symbol("string about me");

const o = {
    name: "titi",
    [symbol]: 22
}

for(let prop in o) {
    console.log("prop :", prop);
}
// prop : name

const props = Object.entries(o);

console.log(props);
// [ [ 'name', 'titi' ] ]
```

#### `Object.entries(obj)` renvoie un tableau de tableau clé/valeur des propriétés de l'objet.

On voit que le nom de propriété de type `symbol` n'est pas reprise.

```js
console.log(o[symbol]);
// 22
```

Les `symbols` permettent d'ajouter des données qui ne seront pas reprise lors d'itération, c'est une possibilité de **metaprogrammation**.

## Plusieurs `symbols` peuvent partager le même ID

```js
const symbol1 = Symbol.for("age");
const symbol2 = Symbol.for("age");

console.log(symbol1 === symbol2);
// true
```

Exemple

```js
const symbol = Symbol.for("age");

function makeAge(person) {
    // ici ageSymbol aura le même identifiant que symbol
    const ageSymbol = Symbol.for("age");
    person[ageSymbol] = 37;
}
const person = {
    name: "hukar"
};

makeAge(person);

console.log(person[symbol]);
// 37
```

## Définir une étiquette `Symbol.toStringTag`

Javascript utilise les symboles en interne de son fonctionnement en rendant accessible des valeurs.

Par exemple le nom de la classe lors d'un `toString` :

```js
class Person {};

const person = new Person();

console.log(person.toString()); // [object Object]

Person.prototype[Symbol.toStringTag] = "Popo";

console.log(person.toString()); // [object Popo]
```

Bien sûr on aurait pu mettre `Person` comme `tag`.

## Well know symbols : les symboles connus

On peut trouver une liste des symboles connus utilisés par javascript sur `mdn`:

https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Symbol

### Autre exemple : `Symbol.toPrimitive`

Pour rappel `Symbol.toPrimitive` est juste un identifiant unique.

```js
const numbers = [1, 2, 3];

console.log(numbers + 1); // "1,2,31"

numbers[Symbol.toPrimitive] = () => 999;

console.log(numbers + 1); // 1000
```

Ce symbole permet de définir un comportement lorsque javascript transforme une valeur en primitive, comme lors de *cast* forcé.

