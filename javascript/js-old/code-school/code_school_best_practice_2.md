# Optimisation des boucles

```js
var lg = function (something) {
  console.log(something)
}
var treasureChest = {
  goldCoins: 10000,
  magicalItem: "crown of speep",
  necklaces: ["ruby","pearl","saphire","diamond"],
  openLid: function () {
   alert("creeeeeak")
  }
}

console.log("You've found the following necklaces")

for (var i = 0;i < treasureChest.necklaces.length;i++) {
  console.log(treasureChest.necklaces[i])
}

lg("\n second loop")
// on réduit ainsi presque de trois-quard les étapes
var x = treasureChest.necklaces.length
for (var i = 0;i < x;i++) {
  lg(treasureChest.necklaces[i])
}

lg("\n third loop")
// on rentre la declaration de x dans la boucle
for (var i = 0, x = treasureChest.necklaces.length;i < x;i++) {
  lg(treasureChest.necklaces[i])
}
// attention le bloc for ne ferme pas les variables i et x sont globales
// javascript does not scope to blocks
lg("i : "+i)
lg("x : "+x)

lg("\n fourth loop")

for (var i = 0, 
     x = treasureChest.necklaces.length,
     // on rentre aussi list dans la boucle
     list = treasureChest.necklaces;
     i < x;
     i++) {
  lg(list[i])
}

lg("\n fifth loop")
// tout est dans la boucle plus besoin d'accolades
for (var i = 0, 
     x = treasureChest.necklaces.length,
     list = treasureChest.necklaces;
     i < x;
     lg(list[i]),
     i++);
```

## for in

for in est optimisé pour les tableaux (?)

```js
var list = treasureChest.necklaces
for  (p in list) {
  lg(list[p])
}
```

Syntaxe plus propre

# Execution de javascript

Utilisation de la propriété HTML async

```html
<script src="monscript.js" async> 
```
Va charger le script de manière non bloquante (asynchrone)

## fonction waiting

```js
function waiting (mot) {
  var result = 0
  
  for (var i = 0; i < 10000000;i++) {
    result += Math.PI*Math.sin(mot.length)
  }
  console.log(mot+Math.floor(Math.random()*10))
}

waiting("boris")
```
Utilisation du prototype pour partagé les fonctions

```js
function Epicerie (adresse, proprietaire) {
  this.owner = proprietaire
  this.address = adresse
  
  this.openShop = function () {
    lg("épicerie ouverte")
  }
}
```

chaque objet instancié aura sa propre copie de openShop

On peut ranger les fonctions dans un prototype commun à tous les objets instanciés

```js
function Epicerie (adresse, proprietaire) {
  this.owner = proprietaire
  this.address = adresse
}

Epicerie.prototype = {
    closeShop: function () {
    lg("épicerie fermée")
  },
  
  openShop: function () {
    lg("épicerie ouverte")
  }
}

var boula = new Epicerie('maison bleue','boulahouane')

boula.closeShop()
```

# Création d'élément du DOM

## méthode gourmande

`document.createElement(nodeName)`

`document.createTextNode(text)`

`elt.appendChild(enfant)` ajoute l'élément à la fin de la liste des éléments enfants __retourne le noeud ajouté__.

```js
// création d'une liste et insertion dans le DOM
var ul = document.createElement('ul')
var body = document.body

body.appendChild(ul)

var fruits = ['banane','abricot','radis']

for (var i = 0,x = fruits.length; i < x; i++) {
  var item = document.createElement('li')
  
  item.appendChild(document.createTextNode(fruits[i]))
  ul.appendChild(item)
}
```

## utilisation d'un "fragment de document"
`document.createDocumentfragment()` crée un DocumentFragment un morceau de DOM non connecté et ne créant pas de reflow

__reflow :__ le calcul des positions et de la géométrie de chacun des éléments du document

```js
// version améliorée, on reduit le reflow avec 
// une seule insertion dans le DOM réel
var ul = document.createElement('ul')
var body = document.body

var fragment = document.createDocumentFragment()
fragment.appendChild(ul)


var fruits = ['banane','abricot','radis']

for (var i = 0,x = fruits.length; i < x; i++) {
  var item = document.createElement('li')
  
  item.appendChild(document.createTextNode(fruits[i]))
  ul.appendChild(item)
}

body.appendChild(fragment)
```

# Déclaration de variable

Chaque mot-clé __var__ provoque une recherche du parser javascript.
Cela peut être évité grace à l'utilisation de la virgule.

```js
var ul = document.createElement('ul')
var body = document.body
var fragment = document.createDocumentFragment()
var fruits = ['banane','abricot','radis']

// un seul var et des virgules
var ul = document.createElement('ul'),
	body = document.body,
	fragment = document.createDocumentFragment(),
	fruits = ['banane','abricot','radis']
```
Pas de déclaration de variable dans une boucle.

La remonté en haut avec les autres déclarations

## méthode join()

```js
var fruits = ['banane','abricot','radis']
console.log(fruits.join("\n * - "))

banane
* - abricot
* - radis
```
à préféré à une boucle avec `+=`

# time et timeEnd

`console.time(label)`

`console.timeEnd(label)` le label doit être le même

```js
function Usine (elt) {
  this.prop = elt
  this.blabla = function () {
    return [
      "b","l","a","b","l","a",
      "b","l","a","b","l","a",
      "b","l","a","b","l","a",
      "b","l","a","b","l","a",
      "b","l","a","b","l","a",
      "b","l","a","b","l","a",
      "b","l","a","b","l","a",
      "b","l","a","b","l","a"
    ]
  }
}

var monUsine = new Usine("titi")

console.time("first")
for (var i = 0;i < monUsine.blabla().length;i++) {
  var text = {
    myText: monUsine.blabla().join(" - ")
  } 
}
console.timeEnd("first")

console.time("second")

for (var i = 0,x = monUsine.blabla().length;i < x;i++) {
  var text = {
    myText: monUsine.blabla().join(" - ")
  } 
}
console.timeEnd("second")

// le second est presque toujours plus rapide
```

Cela nous permet de comparer deux morceaux de code
Le temps obtenu varie légèrement à chaque fois, pour plus de précision il faudrait faire la moyenne

# Utilisation de new Date()

`new Date()` renvoie la date à l'instant de l'appel

`+maDate` transforme la date en timeStamp

`+`opérateur de cast vers type Number

`new Number(maDate)` idem

```js
var d1 = new Date()
lg(d1)
lg(+d1)
for (var i = 0;i < monUsine.blabla().length;i++) {
  var text = {
    myText: monUsine.blabla().join(" - ")
  } 
}
var d2 = new Date()
lg(d2)
lg(+d2)
console.log(new Number(d2))

// Date 2017-06-09T13:03:06.210Z
// 1497013386210
// Date 2017-06-09T13:03:06.211Z 
// 1497013386211
// Number { 1497013386211 }
```

On peut aussi écrire directement `+new date()`

# Classe de test type benchmark

```js
function SpeedTest (testImplement, testParams, repetitions) {
  this.implement = testImplement
  this.params = testParams
  this.repetitions = repetitions || 100000
  this.average = 0
}

// sortir les méthodes dans le prototype

SpeedTest.prototype = {
  startTest: function () {
    var beginTime, endTime, sumTimes = 0

    for (var i = 0,x = this.repetitions;i < x;i++) {
      beginTime = +new Date()
      this.implement(this.params)
      endTime = +new Date()
      sumTimes += endTime - beginTime
    }

    this.average = sumTimes / this.repetitions
  }
}
```