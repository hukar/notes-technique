# Les iterator

## Définition

C'est un objet qui permet de parcourir les éléments d'un autre objet, le plus souvent un conteneur (liste, arbre, ...).

Cela permet d'écrire du code indépendant de la structure de données.

```js
const dragons = ["cool dragon", "angry dragon", "nasty dragon"];

const iterator = dragons[Symbol.iterator](); //?

iterator.next(); //? { value: 'cool dragon', done: false }
iterator.next(); //? { value: 'angry dragon', done: false }
iterator.next(); //? { value: 'nasty dragon', done: false } 
iterator.next(); //? { value: undefined, done: true }
```

En-dessous du capot, la boucle `for of` appelle la méthode `iterator.next`

```js
for (const dragon of dragons) {
  dragon; //?  cool dragon angry dragon nasty dragon
}
```

On peut utiliser `for of` avec tout type d'objet implémentant `iterator`

### Avec les chaînes de caractère

```js
const iterator2 = "une belle histoire"[Symbol.iterator](); //? { [Iterator] }

iterator2.next(); //? { value: 'u', done: false } 
iterator2.next(); //? { value: 'n', done: false } 
iterator2.next(); //? { value: 'e', done: false } 
iterator2.next(); //? { value: ' ', done: false } 
```

```js
for (const char of "ma belle histoire") {
  char; //? m, a,  , b, e, l, l, ...
}
```

### Construire une collection itérable

```js
import randomItem from "./random-item";  // renvoie au hazar un item d'un tableau

const makeDragon = () => {
  const dragonSizes = ["big", "medium", "large"];
  const dragonAbilities = ["fire", "ice", "lightning", "water"];

  return `${randomItem(dragonSizes)} ${randomItem(dragonAbilities)} dragon`;
};
```



```js
const dragonArmy = {
  [Symbol.iterator]: () => {
    return {
      next: () => {
        const enoughDragonSpawned = Math.random() > 0.75;
        if (!enoughDragonSpawned) {
          return {
            value: makeDragon(),
            done: false
          };
        }
        return { done: true };
      }
    };
  }
};
```

```js
for (const dragon of dragonArmy) {
  dragon; //? medium ice dragon, large water dragon, ...
}
```

`[Symbol.iterator]` est une **factory** qui renvoie un objet ayant une seule méthode `next`

**factory** : fonction renvoyant un objet `() => { return { ... }; }`

`next` renvoie soit { value: myValue, done: false } tant qu'il y a des valeurs à renvoyer, soit { done: true }, pour dire que la collection est fini.

Le **spread operator** fonctionne aussi avec les itérateurs :

```js
[...dragonArmy]; //? [ 'large ice dragon', 'big lightning dragon' ]
```

### écriture `[Symbol.iterator]:` 

Un `Symbol` en javascript est ce qu'on pourrait appeler un `atom` dans d'autres langages.

C'est un type de donnée primitif.

```js
const shadow = Symbol(); //? Symbol()
typeof shadow; //? Symbol
```

Du coup on peut les utiliser comme clé:

```js
const titi = Symbol();

const o = {
    titi: "titi",

}

o.titi; //? titi
```

```js
const titi = Symbol();

const o = {
    [titi]: "titi",

}

o[titi]; //? titi
```

Par contre :

```js
const titi = Symbol();
[titi]; //? [ Symbol() ]

const o = {
    [titi]: "titi",

}

o.titi; //? undefined
```

Depuis ES6 (ECMAScript 2015) on peut avoir des noms de propriété calculés à l'exécution.

