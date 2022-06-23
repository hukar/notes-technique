# les ternaires

Avec fonction anonymes

```js
isArthur&&isKing? function () {
    console.log("huhhuhu")
    alert("a new king with excalibur")
  } () : function () {
    console.log("hihihih")
    alert("there are not new king")
  } () // () pour invoquer directement la fonction
```

Avec parenthèses et virgule

```js
isArthur&&isKing?(
  console.log("huhhuhu"),
  alert("a new king with excalibur")):
  (console.log("hihihih"),
  alert("there are not new king"))
```

Ici les instructions sont contenues dans des parenthèses et séparées par une virgule

## nested ternary (ternaires imbriqués)

```js
var isCat = false
var isMale = true
var isDark = false


var textLog = !isCat||!isMale?"alors c'est quoi??"
                            :isDark?"haaa un chat noir"
                                    :"un chat quelconque"
console.log(textLog)

> alors c'est quoi??
```

# assignation d'une valeur avec OU

Utilistaion du ou logique pour assigner une valeur

```js
var sac = {
  addCloth: function (cloth) {
    this.cloths = this.cloths || []
    this.cloths.push(cloth)
    }
  }
  
sac.addCloth('baskets')
sac.addCloth('hat')
console.log(sac.cloths)
 
    
> [ 'baskets', 'hat' ]
```

## valeur de retour
OU renvoie la première valeur vraie ou la dernière valeur fausse

```js
var result1 = "" || undefined
console.log(result1)
> undefined

var result2 = undefined || false
console.log(result2)
> false
```

### valeurs fausses rappel
```js
0
-0
NaN
null
undefined
false
""
```

# new Function

Lorsqu'on utilise new Function, la fermeture ne fonctionne plus

```js
var x = 23

function noClo () {
  var x = 11
  return new Function('return x')
}

function clo () {
  var x = 44
  
  return function () {return x}
}

console.log(noClo()()) // équivalent à f1 = noClo();f1()
console.log(clo()())  // équivalent à f2 = clo();f2()

> 23 // affiche la variable global et non celle dans la fermeture
> 44
```

# fonction "constructeur" et switch

__rappel__ une fonction-constructeur est de la forme :  
function Perso (name,age) {this.name = name;this.age = age}


```js
// fonction constructeur
function Animal(specie) {
  this.specie = specie
  // on assigne une valeur à cry grace à un switch
  switch (specie) {
    case 'duke':
      this.cry = 'coincoin'
      break
    case 'pig':
      this.cry = 'groingroin'
      break
    case 'dog':
    case 'wolf':
      this.cry = 'wafwaf'
      break
    default:
      this.cry = 'salut'
  }
};
var canard = new Animal('duke')  // on instancie un animal
var chien = new Animal('dog')
```
__remarque 1:__ utilisation d'une majuscule pour la fonction-constructeur

__remarque 2:__ On peut mettre default n'importe où et mélangé les types dans les case

```js
function Animal(specie) {
  this.specie = specie
  switch (specie) {
    case 'duke':
      this.cry = 'coincoin'
      break
    default:  // le default peut être n'importe où 
      this.cry = 'salut'
      break // du coup un break est nécéssaire
    case 'pig':
      this.cry = 'groingroin'
      break
    case 'dog':
    case 'wolf':
    case 1:  // on met un entier avec des string
      this.cry = 'wafwaf'
      break
    
  }
};

var bouggi = new Animal("bloo")
var micho= new Animal(1)

console.dir(bouggi)
console.dir(micho)

> {specie: "bloo", cry: "salut"}
> {specie: 1, cry: "wafwaf"}
```

## attribution hiérarchique

```js
function CeremonialDagger (knight,rank) {
  this.length = 8
  this.owner = knight
  
  switch (rank) {
    case "king":
      this.diamod = 1
    case "high constable":
      this.amethyst = 2
    case "captain":
      this.emeralds = 1
    case "knight":
      this.rubies = 6  
  }
}

var kingDagger = new CeremonialDagger("francois","king")
console.dir(kingDagger)

var captainDagger = new CeremonialDagger("michel","captain")
console.dir(captainDagger)

var knightDagger = new CeremonialDagger("jo","knight")
console.dir(knightDagger)

> {length: 8, owner: "francois", diamod: 1, amethyst: 2, emeralds: 1, rubies: 6}

> {length: 8, owner: "michel", emeralds: 1, rubies: 6}

> {length: 8, owner: "jo", rubies: 6}
```