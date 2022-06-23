# Game of life avec React

## Correction de Free Code Camp

John Horton Conway est l'inventeur du jeu de la vie.

C'est un automate cellulaire

## Les règles

- Si une cellule morte a exactement trois voisines vivantes elle devient vivante à l'étape suivante **reproduction**.
- Si  une cellule a exactement deux voisines vivantes elle reste dans son état actuel à l'étape suivante.
- Si une cellule a strictement **moins de deux** ou **plus de trois** voisines vivantes elle meure à l'étape suivante (**surpopulation** ou **isolement**)

## La problématique du tableau à deux dimensions

### Initialisation

On voudrait initialiser un tableau à deux dimensions avec la valeur false :

```js
const g = [];

for (let i = 0; i < 3; i++) {
    g[i] = [];
    for (let j = 0; j < 3; j++) {
        g[i][j] = false;
    }
}

g; // [ [ false, false, false ], [ false, false, false ], [ false, false, false ] ] 
```

La manière classique fonctionne parfaitement.

#### Problème de la méthode fonctionnelle

```js
const h = Array(3).fill(Array(3).fill(false));
h; // [ [ false, false, false ], [ false, false, false ], [ false, false, false ] ] 
```

On pourrait croire que c'est la même chose mais modifions la valeur `[0][0]`

```js
g[0][0] = true;
g; // [ [ true, false, false ], [ false, false, false ], [ false, false, false ] ]
```

```js
h[0][0] = true;
h; // [ [ true, false, false ], [ true, false, false ], [ true, false, false ] ]
```

Dans le tableau `g` une seule valeur est modifié, alors que dans le tableau `h`, trois veleurs sont modifié !!

> **mdn** : Lorsque cette méthode reçoit un objet comme valeur, elle copiera l'objet passé et remplira le tableau avec une référence vers cette copie.

On a donc trois fois la même référence au même tableau.

Pour avoir à chaque une nouvelle instance de tableau on utilisera `map`

```js
const f = Array(3).fill().map(() => Array(3).fill(false));
f[1][2] = true; //?
f; // [ [ false, false, false ],[ false, false, true ],[ false, false, false ] 
```

Voici le comportement souhaité avec une écriture fonctionnelle !

Syntaxe alternative avec le **spread operator** :

```js
const f = [...Array(3)].map(() => Array(3).fill(false));
```



### Copie

Comment copié des tableaux imbriqués

#### `spread operator ...`

```js
const t = [[false],[false],[false]]

const tt = [...t]

tt.push("koko");

tt;  // [ [ false ], [ false ], [ false ], 'koko' ]
t;  // [ [ false ], [ false ], [ false ] ]
```

Pour l'instant le comportement est attendu mais :

```js
tt[0][0] = true;

tt;  // [ [ true ], [ false ], [ false ], 'koko' ]
t;  // [ [ true ], [ false ], [ false ] ]
```

Par contre ici le comportement n'est pas souhaité.

#### Pour une copie profonde on utilise `JSON.parse(JSON.stringify(arr))`

```js
const t = [[false],[false],[false]]

const tt = JSON.parse(JSON.stringify(t))

tt.push("koko");

tt; // [ [ false ], [ false ], [ false ], 'koko' ]
t;  // [ [ false ], [ false ], [ false ] ]

tt[0][0] = true;

tt;  // [ [ true ], [ false ], [ false ], 'koko' ]
t;  // [ [ false ], [ false ], [ false ] ]
```

