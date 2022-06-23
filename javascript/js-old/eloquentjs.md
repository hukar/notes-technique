# Eloquent js
 Connaitre le type d'une donnée : `typeof`
 
 Il y a six types de base
 
 1. string
 2. number
 3. boolean
 4. object
 5. function
 6. undefined

```javascript
Math.min(4,6)
> 4
```

```javascript
Math.max(4,6)
> 6
```

## conversion de type

```javascript
NaN == NaN  // donne false
var titi = NaN
isNaN(titi)  // donne true
```

```javascript
"5" + 5
> "55"
Number("5") + 5
> 10
"5" * 5
> 25
```

## opérateur logique OU || - ET &&

__a || b__ : renvoie true si __a__ __true__ sinon renvoie __b__

Ce mécanisme permet d'attribuer une valeur par default

```javascript
var nom = prompt("votre nom")

alert(nom || "anonyme")
``` 

De même `true && "coucou"`renvoie la valeur __coucou__

## Fonction

### Fermeture

Une fonction définie dans une autre fonction garde un accès à l'environement dans lequel elle a été définie

```javascript
function ajouterMotFactory (mot) {
  function sePresenter (nom) {
    console.log("mon mot est : "+mot+" mon nom est :"+nom)
  }
  return sePresenter
}

var sePresenterCasserole = ajouterMotFactory("casserole")
var sePresenterRasoir = ajouterMotFactory("rasoir")
sePresenterCasserole("maurice")  // mon mot est : casserole mon nom est :maurice
sePresenterRasoir("maurice")  // mon mot est : rasoir mon nom est :maurice
```
Le mot définie pour chaque fonction de type sePresenterMot est celui utilisé par la nouvelle fonction

### Récursive

une fonction qui s'apelle elle-même est dite récursive

```javascript
function puissance (base,exposant) {
	if (exposant == 1) {
		return 1
	} else {
		return base * puissance(base,exposant-1)
	}
}
```
base * (puissance(base,exposant-1) = (base * puissance(base,exposant-2) etc...  
> base * (base * (base * (base * 1)))

### Chronologie

Les fonctions sont stockées par le programme __avant__ de lire le reste du code

```javascript
console.log(blabla())

function blabla () {
  return "blablabla..."
}
```

La fonction `blabla` est stockée avant l'exécution du `console.log`  
Par contre si c'est une fonction anonyme :

```javascript
console.log(blabla())

var blabla = function () {
  return "blablabla..."

```
>TypeError: blabla is not a function. (In 'blabla()', 'blabla' is undefined)  

Blabla étant une variable elle doit être définie avnt d'être utilisée

## Objet et tableau

### Transformer une chaÎne de caractère en nom de fonction

Il est parfois utile d'utiliser une chaine de caractère pour appeler une fonction, on utilise alors la fonction `eval()`

```javascript
eval("nomFonction")(5)

// si le nom d'une fonction est stockée dans une variable
var toto = "nomDeFonction"
eval(toto)(67)
```

### Propriété

effacer une propriété

```javascript
delete objet.maPropriete

objet.maProprite
> undefined
```

accéder au propriété avec l'écriture tableau

```javascript
> var titi = {"mon gros minet":1,"couleur de plume": "jaune"}
< undefined
> titi["mon gros minet"]
< 1
> var minet = "mon gros minet"
< undefined
> titi[minet]
< 1
> "mon gros minet" in titi
< true
> "mon gros monet" in titi
< false
```
**l'opérateur in**

```javascript
> minet in titi
< true
```

