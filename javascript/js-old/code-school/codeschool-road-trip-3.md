# closure

```javascript
function habitePays (pays) {
	return function (nom) {
		console.log(nom+" habite "+pays)
	}
}

var habiteFrance = habitePays("france")
habiteFrance("rené")

> "rené habite france"
```

Les closure permettenr de créer des fonctions capable de construire d'autres fonctions.

Elles définissent un scope fermé (fermeture = closure)

# Hoisting

```javascript
function getMystery () {
	function chooseMystery () {
		return 12;
	}
	return chooseMystery();

	function chooseMystery () {
		return 7;
	}
}

getMystery()
> 7
```

Dans un scope sont d'abord remonté 
1. la déclaration de variable, 
2. puis les déclaration de fonction, 
3. puis enfin le code

```javascript
function getMystery () {
	function chooseMystery () {
		return 12;
	}
	// remonté ici
	function chooseMystery () {
		return 7;
	}
	
	return chooseMystery();
	
}
```

```javascript
function getMystery () {
	var chooseMystery = function () {
		return 12;
	}
	return chooseMystery();

	function chooseMystery () {
		return 7;
	}
}

getMystery()
> 12
```

d'abord `var chooseMystery = undefined`

ensuite `function chooseMystery () { return 7; }`

puis 

```javascript
	var chooseMystery = function () {
		return 12;
	}
	return chooseMystery();
```
# Object
Supprimer une propriété d'un objet :

```javascript
delete monObjet.maPropriete;
```

# prototype

Une fonction ajouté au prototype de String pour lui permettre de compter le nombre d'une lettre donnée dans une chaîne.

```javascript
String.prototype.countLetter = function (letter) {
  var myCount = 0;
  for (var i =0;i < this.length;i++) {
    if (this[i] == letter) {
      myCount++;
    }
  }
  return myCount;
};

 console.log("renée est gentil".countLetter("e"));
 > 4
```
### Différence entre \_\_proto__ et prototype

**\_\_proto__** est l'objet réel qui est utilisé dans la chaîne de recherche pour résoudre les méthodes, etc. 

**prototype** est l'objet qui est utilisé pour construire \_\_proto__ lorsque vous créez un objet avec `new`
### créer un nouvel objet par prototypage

```javascript
var shoe = {color: "red",size: 39,material: "leather"};

var magicShoe = Object.create(shoe);
magicShoe.magic = "abracadabra";
magicShoe.weapon = "lightning";

 shoe.isPrototypeOf(magicShoe)
=> true
   magicShoe.isPrototypeOf(shoe)
=> false
```

**Objet.create()** permet de créer des objets par prototypage.

On peut aussi utiliser l'attribut **\_\_proto__**

```javascript
var animal = {pattes: 4,specie: "chat",name: "chichi"}

var chat = Object.create(animal)

chat
> Object
	__proto__:Object
		name :"chichi"
		pattes:4
		specie:"chat"
		
// on peut écrire la même chose de cette manière
var animal = {pattes: 4,specie: "chat",name: "chichi"}

var chat = {__proto__:animal}
```

### isPrototypeOf()

```javascript
animal.isPrototypeOf(chat)
> true
Object.prototype.isPrototypeOf(chat)
> true
chat.isPrototypeOf(animal)
> false
```

### getPrototypeOf
Récupère le prototype d'un objet

```javascript
var animal = {pattes: 4,specie: "chat",name: "chichi"}

var chien = Object.create(animal)

Object.getPrototypeOf(chien)
> Object {pattes: 4, specie: "chat", name: "chichi"}

Object.getPrototypeOf(chien) === animal
> true
```

### setPrototypeOF
Modifie le prototype d'un objet

```javascript
var chien = {pattes: 4,crie: "ouaf!"}
var oiseau = {pattes: 2, chant: "lalalala"}

var animal = Object.create(chien)

Object.getPrototypeOf(animal)
> [object Object] {
  	crie: "ouaf!",
  	pattes: 4
	}

animal.crie
> "ouaf!"
Object.setPrototypeOf(animal,oiseau)
> [object Object] {
  	chant: "lalalala",
  	pattes: 2
	}
animal.crie
> undefined
animal.chant
> "lalalala"
```
### Créer un constructeur

```javascript
function animal (pattes,specie,name) {
  this.pattes = pattes
  this.specie = specie 
  this.name = name
  }

var chat = new animal(4,"chat","ronron")
```

Cela permet de spécifier les attributs dès l'instanciation

### fonctionnement du construceur

```javascript
var o = new Toto();
JavaScript exécutera des instructions équivalentes à :

var o = new Object();
o.[[Prototype]] = Toto.prototype;
Toto.call(o);
```

`Toto.call(o)` attribue **o** à la valeur de this 

### Constructeur + prototype

#### méthodes dans le constructeur et méthode dans le prototype

##### 1 dans le constructeur

```javascript
var nini = function (name,age) {
    this.name = name
    this.age = age
    this.scaring = function () {console.log("HU aaaHuu... "+this.name)}
}

var a = new nini("renée",34)
var b = new nini("pascal",27)


a.scaring == b.scaring
> false
```

Chaque instance possède ses propres méthodes.

##### 2 dans le prototype du constructeur

```javascript
var nini = function (name,age) {
    this.name = name
    this.age = age
}

nini.prototype = {
    scaring: function () {console.log("HU aaaHuu... "+this.name)}
}

var a = new nini("renée",34)
var b = new nini("pascal",27)

a.scaring == b.scaring
> true
```
Chaque instance partage les mêmes méthodes, se trouvant dans le prototype du constructeur.
____


```javascript
function animal (pattes,specie,name) {
  this.pattes = pattes
  this.specie = specie 
  this.name = name
  }
  
animal.prototype = {
  gogoSay: function () { console.log("gogo "+this.name) },
  yoyoSay: function () { console.log("yoyo "+this.specie) },
  suricate: "mimi"
}

animal.prototype.express = "yo pico!!"

var chat = new animal(4,"chat","ronron")

chat.gogoSay()
> gogo ronron

chat.yoyoSay()
> yoyo chat

chat.hasOwnProperty("express")
> false
chat.express
> 'yo pico!!'
```

**Toute fonction déclaré possède nativement une propriété « prototype » initialement vide**

```javascript
var bibi = function () {}

bibi.prototype
> [object Object] { ... }
```

un objet construit avec une fonction constructeur possède un prototype **nomFonction.prototype** qui est en fait son attribut **\_\_proto__**

```javascript
chat.__proto__ === animal.prototype
> true
```

### Propriété privée

On peut utiliser `var`dans la fonction **constructeur**
pour rendre une propriété `private`

```javascript
function animal (name, nounou) {
// on définie une propriété publique avec this.
  this.name = name
  // une propriété privée est définie avec var
  var nounou = nounou
// un accesseur
  this.nounou = function () { return nounou)}
 }
  
 var chat = new animal("ronron","mimi")
  
  chat.nounou()
> "mimi"
```

On crée un accesseur `this.nounou`

### constructor

accède à la fonction constructeur de l'objet

```javascript
var animal = function (name,nounou) {

  this.bibi = nounou
  this.name = name

  }

var chat = new animal("ronron","mimi")
chat.constructor == animal
> true
``` 
#### instanceof


```javascript

chat instanceof animal
> true 
```

### Boucle for-in

```javascript
var aquarium = {
fish01: {gender: "fish",name: "puppi"},
fish02: {gender: "fish",name: "color"},
vegetable: {gender: "algue", size: "giant"},
action01 : function () {console.log("do something")},
action02 : function () {console.log("do also something")}
}

var superAqua = Object.create(aquarium)
superAqua.super = "super"
superAqua.action03 = function () {console.log("niak niak")}

for (bibi in superAqua) {
    console.log("bibi : "+bibi)
    superAqua[bibi].gender ==  "fish" && console.log("du poisson!!")
    console.log("typeof superAqua[bibi] : "+typeof superAqua[bibi])
    console.log("superAqua[bibi] : "+superAqua[bibi])

}
```

`for (nameProperty in object)` énumère les noms des propriété de l'objet

`superAqua[bibi].gender == "fish" && console.log("du poisson!!")`  manière abrégé d'écrire :    
`if (superAqua[bibi].gender == "fish") {console.log("...")}`

`typeof superAqua[bibi]`renvoie le type de la propriété, on peut ainsi filtrer les méthodes   