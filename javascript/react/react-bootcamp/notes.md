# Notes

## gyp ERR!

Si il y a une erreur de type :

```bash
gyp ERR! # ... et plein de truc comme ça.
```

Corriger en installant une version stable (lts) de node.

## Chaîner les expressions en javascript

```js
var glob = 7;

function addThree() {
    glob = glob + 3;
    return 1;
}

function soustractTen() {
    glob = glob - 10;
    return 1
}

function specialAddition(nb1, nb2) {
    glob = glob + nb1 + nb2;
}

specialAddition(addThree(), soustractTen()); 

glob; //? 2 => ((7 + 3) - 10) + 1 + 1 = 0 + 2 = 2
```

Si les expressions ont des effets de bords, la limite avec les instructions devient floue.

## Compléter de zéro

Si on veut ajouter des zéros devant un nombre plus petit que 100 voici une méthode :

```js
function completeZero(nb) {
  return nb <= 99 ? `00${nb}`.slice(-3) : nb;
}

let nb = 67;
completeZero(nb); //? 067

"hello".slice(-2); //? lo
```

`slice(-3)` sur une chaîne va renvoyer les trois derniers éléments. Ne modifie pas la chaîne d'origine.

## Reduce

```js
const pokemons = [
  { id: 123, exp: 50 },
  { id: 13, exp: 67 },
  { id: 41, exp: 128 },
  { id: 50, exp: 176 }
];

// Calcule de l'expérience totale avec reduce

const expTotal = pokemons.reduce((t, pokemon) => t + pokemon.exp, 0);
expTotal; //? 421
```

`reduce((accumulateur, valeur courante) => traitement, valeur initiale de accumulateur)`

#### ! ne pas oublier la valeur initiale de l'accumulateur.

## Problème avec NPM

`NPM` peut ne pas vouloir fonctionner avec des erreurs du type :

```bash
npm ERR! code EACCES
npm ERR! syscall open
npm ERR! path /Users/kms/.npm/_cacache/index-v5/0b/05/f22649356d0ed7c823c463cfd530d4264cb215bc57067d284588ed79b316
npm ERR! errno -13
npm ERR! 
npm ERR! Your cache folder contains root-owned files, due to a bug in
npm ERR! previous versions of npm which has since been addressed.
npm ERR! 
npm ERR! To permanently fix this problem, please run:
npm ERR!   sudo chown -R 501:20 "/Users/kms/.npm"

npm ERR! A complete log of this run can be found in:
npm ERR!     /Users/kms/.npm/_logs/2019-11-07T16_42_39_666Z-debug.log
```

Ce qui est bien, c'est que `NPM` donne la solution :

```bash
sudo chown -R 501:20 "/Users/kms/.npm"
```

Ce qui a fonctionné pour moi.