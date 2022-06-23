# Attention aux comparaisons

```js
console.log("\n\n \t" == 0) // true
console.log("\n\n \t" === 0) // false

function Creature (name) {
  this.name = name
}

var jojo = new Creature("jojo"),jiji = {name: "jiji"}

console.log(jojo instanceof Creature) // true

console.log(jiji instanceof Creature) :: false
```
`===` comparaison strict

`instanceof` vérifie si un objet est l'instance d'une classe

## Prototype

```js
Ork.prototype = Creature.prototype 
// attention on ne crée pas un nouvel objet
// Les efles seront aussi des ork
// Il faut écrire Ork.prototype = Objet.create(Creature.prototype)
// ainsi on a un nouvel objet 

Elf.prototype = Object.create(Creature.prototype)
```

# Try & Catch

```js
try {
  alert(pipi)
} catch (error) {
  console.log("une erreur : ",error)
}

```

## Lancer une erreur

```js
function mimi () {
  if(arguments.length != 0)
    throw new Error("pas d'arguments svp!!!")
}

try {
  var pipi = ""
  mimi (pipi)
} catch (error) {
  console.log("une erreur : ",error)
}
```

## La première erreur lancé est interceptée

```js
function mimi (a) {
  if(arguments.length != 1)
    throw new Error("pas le bon nombre d'arguments!!!")
  if(Number.isInteger(a))  // <- c'est la première erreur envoyée
    throw new Error('pas d\'entier s\'il vous plait')
  if (a.length != 5)
    throw new Error("pas assez long")
}

try {
  var pipi = 5
  mimi (pipi) // erreur c'est un nombre entier 
} catch (error) {
  console.log("une erreur : ",error)
}
finally {
  console.log("popo pipi") // ce que l'on fait au final
}

> 	une erreur :  Error: pas d'entier s'il vous plait
   	at mimi (playground.js:7)
	popo pipi
```

## interest de finally

S'il n'y a pas de bloc catch, il faut un bloc finally et ses instructions sont bien exécutées.

```js
try {
  var pipi = 5
  mimi (pupu)
  momo(pipi) + dada
}
finally {
  console.log("popo pipi")
}

console.log("BIBI BOBO")

> popo pipi <- le code ede finally est bien exécuté
playground.js:14 Uncaught ReferenceError: pupu is not defined
    at playground.js:14
```

On voit dans ce cas que le dernier `console.log` n'est pas exécuté.

# With

Sans le mot-clé `with`

```js
var chateau = {
  regiment: {
    guerrier: {
      attaquer: function () {
        console.log("attaque")
      }
    }
  }
}

chateau.regiment.guerrier.attaquer()

> attaque
```

Le mot clé with prend le contexte de l'objet en paramètre comme scope

```js
var chateau = {
  regiment: {
    guerrier: {
      attaquer: function () {
        console.log("attaque")
      }
    }
  }
}

with (chateau.regiment.guerrier) {
  attaquer()
}

> attaque
```

## Attention à with

```js
var poulet = {
  nom: "polo",
  attaque: function () {
    console.log("poulet "+this.nom+" attaque")
  }
}


with (poulet) {
  attaque()
  function defendre() {
    console.log("poulet "+nom+" defendre")
  }
  defendre()
}

poulet.defendre()

>poulet polo attaque
playground.js:13 poulet polo defendre
playground.js:18 Uncaught TypeError: poulet.defendre is not a function
    at playground.js:18
```

L'appel à poulet.defendre() génère une erreur car le scope de with n'est pas vraiment l'intérieur de l'objet (!?)

```js
defendre()

> poulet polo defendre
```

defendre n'est donc pas une méthode de poulet mais une fonction dont le scope reste lié à poulet !

Le mieux c'est de définir une variable :

```js
var bassecour = {

  poullaier: {
    poulet: {
      nom: "polo",
      attaque: function () {
      console.log("poulet "+this.nom+" attaque")
      }
    }
  }

}


var o = bassecour.poullaier.poulet // on remplace le mot-clé with
  o.attaque()
  o.defendre = function () {
    console.log("poulet "+o.nom+" defendre")
  }
o.defendre()

bassecour.poullaier.poulet.defendre()
```

# problème avec eval

```js
var regiment = {
  text: ""
}

function assignRegiment (text) {
  eval("regiment.text='"+text+"'")
}

assignRegiment("artiste est le plus mal compris")

console.log(regiment.text)

> artiste est le plus mal compris
```

Mais si jamais la phrase contient un guillemet simple

```js
assignRegiment("l'artiste est le plus mal compris")

> VM470:1 Uncaught SyntaxError: Unexpected identifier
    at assignRegiment (playground.js:6)
    at playground.js:9
```

On peut minimiser l'utilisation d'`eval` :

```js
var list1 = {nom:"renée"}

var numeroList = 1
var newName = "m'chel"
eval("list"+numeroList).nom=newName

console.log(list1.nom)
> m'chel
```

Pour rendre ses données dynamiques il vaut mieux utiliser un tableau à la place de `eval`:

```js
var list = [{nom:"renée"},{nom:"micho"},{nom:"pipa"}]


var newName = "m'chel"

list[1].nom = newName
/* ici le point virgule est obligatoire après le for */
for (var i = 0;i < list.length;console.log(list[i].nom),i++);
> renée
> m'chel
> pipa
```

## JSON.parse et pas eval

```js
var structA = {
  name: "michel",
  age: 78,
  cars: [{name: "volvo",speed:123},{name: "citroen",speed:145},{name: "fiat",speed:234}],
  cat: {
    name: "coco",
    age: 8
  }
}

var newString = JSON.stringify(structA)  
// on obtient une chaîne de caractère

var o = JSON.parse(newString)  
// on récupère de manière sûre un objet

console.log(structA)
console.log(newString)
console.log(o)
```

# Les nombres

Problème avec les nombres à virgule flotante

```js
console.log(0.1 + 0.2 + 0.3)
> 0.6000000000000001
```

utilisation de `toFixed`

```js
console.log((0.1 + 0.2 + 0.3).toFixed(1))
> '0.6'
```

toFixed('nombre de décimales') retourne une chaîne de caractère

```js
console.log((0.1 + 0.2 + 0.3).toFixed(1) + (0.1 + 0.2).toFixed(2))
> '0.60.30' // il y a concaténation au lieu d'addition
```

Correction de la virgule flotante et du type avec `parseFloat` : 

```js
console.log(0.3 + 0.6)
console.log( parseFloat( (0.3 + 0.6).toFixed(1) ) )

> 0.8999999999999999
> 0.9
```

## attention à parseInt

`parseInt` accepte des bases de 2 à 36
 
```js
parseInt(4.89)
> 4 // pas d'arrondi au supérieur

parseInt(021)
> 17  // car il pense être en base 8 avec le zéro devant

parseInt(021,10)
> 17

parseInt("021",10)
> 21  // on spécifie la base pour enlever toutes ambiguïté
> // il faut aussi passer une chaîne car sinon l'erreur subsiste !!
```

Pour convertir un nombre en hexa :

```js
(12).toString(16)
> "c"
```

et inversement

```js
parseInt("c",16)
> 12
// ou bien plus simple encore
0xac
> 172
```

### de même pour la base 8

```js

(42).toString(8)
> "52"
052  // zéro cinquante deux
> 42
```

## vérifier un nombre

Attention le type de Not A Number c'est number ?! :

```js
typeof NaN
> 'number'
```

Différents résultats avec `NaN`

```js
NaN === NaN
> false

isNaN(NaN)
> true

isNaN(6)
> false
 
isNaN("rets")
> true

isNaN("45")
> false
```

Une fonction pour être sûre qu'une valeur est un nombre :

```js
var testIsNumber = function (data) {
  return typeof data === "number" && !isNaN(data)
}

testIsNumber(5)
> true
testIsNumber("6")
> false
testIsNumber(NaN)
> false
   
```

Checher le nombre de digit d'un entier (comme pour un code postal)

```js
(56789).length
> undefined
(54326).toFixed(0).length
> 5
```