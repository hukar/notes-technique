# namespace

On peut créer un namespace en utilisant un objet :

```js
var CAVESOFCLARITY = {
  stalactites: 4235,
  stalagmites: 3924,
  bats: 345,
  treasureChests: 3,
  openChest: function() {
  this.treasureChests--;
  alert('DA DADADA DAAAAAAA!');
  }
 };
```

Le nom du namespace est par convention en capital

## création d'un module

Form générale d'un module :

```js
var MON_MODULE = function () {
	var attribut_prive = "attribut privé"
	var methode_prive = function () { ... }
	
	return {
		attribut_public: "je suis une valeur public",
		methode_public: function () { ... }
	}
}()
```

ON l'utilise comme ceci :

```js
MON_MODULE.methode_public()
MON_MODULE.attribut_public
```

exemple de code complet

```js
var ROBOT = function(){
  var name = "alfred"  // ATTRIBUT PRIVE

  var randomizeTab = function (tab) {
    var size = tab.length,
        ind

    var newTab = [],
        indTab = []

    for (var i = 0;i < size;i++) {


      while(true) {
        ind = Math.floor(Math.random()*size)
        if (indTab.indexOf(ind) == -1) {
          indTab.push(ind)
          newTab[ind] = tab[i]
          break
        }
      }
    }

    return newTab
  }  // METHODE PRIVE

  var transformName = function(){
    var tabName = name.split("")
    tabName = randomizeTab(tabName)

    name = tabName.join("")
  }  // METHODE PRIVE

  return {
    displayName: function () {
      console.log(name)
    },  // METHODE PUBLIC
    changeName: transformName  // METHODE PUBLIC
  }
}()

ROBOT.displayName()
> alfred
ROBOT.changeName()

ROBOT.displayName()
> rfaled  // ou autre chose...
```

# Global Import

importer des variables globales dans son module

- évite que le programme recherche la variable dans tout les scopes jusqu'au scope global
- permet de savoir d'où viens la variable, ajoute de la clarté au code

```js
var meteoGlobal = 2

var METEOBOT = (function (meteo) {
  var mymeteo = meteo

  return {
    display: function () {
      if (mymeteo == 1) {
        console.log("il fait beau")
      } else if (mymeteo == 2) {
        console.log("il pleut :(")
      }
    },
    hocusPocus: function () {
      mymeteo = 1
    }
  }
})(meteoGlobal)

METEOBOT.display()
console.log(meteoGlobal)
> il pleut :(
> 2

METEOBOT.hocusPocus()
METEOBOT.display()
console.log(meteoGlobal)
> il fait beau
> 2
```

## augmentation

Procédé permettant d'augmenter un module

```js
var POLICEFORCE = (function () {
  var policeArmy = 300,
      honorBadge = 17

      return {
        killEvilPerson: function () {
          honorBadge++
          console.log("honor bardge :"+honorBadge)
        },
        deadOfPoliceman: function () {
          policeArmy--
          console.log('effectif :'+policeArmy)
        }
      }
})()

POLICEFORCE.killEvilPerson()
POLICEFORCE.killEvilPerson()
POLICEFORCE.deadOfPoliceman()
POLICEFORCE.deadOfPoliceman()

> honor bardge :18
> honor bardge :19
> effectif :299
> effectif :298

POLICEFORCE = (function (oldNS) {
  var warMachine = 6


    oldNS.exploseWarMachine = function () {
      warMachine--
      // policeArmy--
      // erreur : ReferenceError: policeArmy is not defined
      console.log("war machine :"+warMachine)
    }

    return oldNS

})(POLICEFORCE)

POLICEFORCE.exploseWarMachine()
POLICEFORCE.exploseWarMachine()
POLICEFORCE.killEvilPerson()
POLICEFORCE.deadOfPoliceman()
> war machine :5
> war machine :4
> honor bardge :20
> effectif :297
```