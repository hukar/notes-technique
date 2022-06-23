# ajax
---

redirection php :

```php
<?php  

header('location:form.php');
```

Donne un code **302** dans le retour de la requête HTTP

en javascript :

```javascript
document.location.href = "form.php"
```

Donne un code **200** contrairement à la redirection avec `header()` en php

### constructeur

```javascript
var httpRequest = new XMLHttpRequest()
```

### envoyer une requête

```javascript
httpRequest.open('METHOD','adresse'[,asynchrone (true ou false)])

httpRequest.open('GET','pays.php?continent=afrique'[,true])
httpRequest.send()
```
Le troisième paramètre est souvent à `true` pour avoir une requête **asynchrone** et ne pas bloquer le programme optionnel

### récupérer la réponse

```javascript
httpRequest.onreadystatechange = function () {

  if (req.readyState == 4) {
    var histoire = httpRequest.response
    var p = document.getElementsByTagName('p')[0]

    p.innerText = histoire
  }
}
```
On peut aussi utliser un eventListener :

```javascript
httpRequest.addEventListener('readystatechange',function () {
  if (readyState == 4) { //...do something... }
})
```

L'évènement **onreadystatechange** nous indique quand *readyState* change d'état
Les données sont arrivées quand `readyState == 4`
Le texte se trouve dans la propriété **response**

value|state|description
---|---|---
0|UNSET|la fonction open() n'est pas encore appelée
1|OPENED|open() est appelée
2|HEADERS_RECEIVED|send() est appelée et les headers sont près
3|LOADING|chargement des données
4|DONE|opération terminée

### la même chose en utilisant addEventListener et la gestion d'erreur

```javascript
var req = new XMLHttpRequest()
req.open('GET','http://localhost:8888/javascript/data.json')
req.send()

req.addEventListener('load',function () {
  console.log("status",req.status)
})

req.addEventListener('error',function () {
  console.log("erreur attrapée")
})
```
`req.response` et `req.responseText` : Le résultat de la requêtable
`req.status` **200** succés **404** non trouvé

### obtenir une réponse en json

Deux fonction permettent de passer de string à json et vice versa:
`JSON.parse(texte)` string->json
`JSON.stringify(objet)` json->string

```javascript
var req = new XMLHttpRequest()
req.open('GET','data.json')
req.send()

req.addEventListener('load',function () {
  var response = JSON.parse(req.response)
  console.dir (response)
})
```
Les tableaux sont gérés [{...},{...}]

# envoie de données : POST

Pour envoyer des données on utilise la méthode **POST**

```javascript
var xhr = new XMLHttpRequest()
xhr.open('POST','ajax.php')
```

Il faut définir un content type dans le header :

```javascript
xhr.setRequestHeader('content-type','application/x-www-form-urlencoded')
xhr.send("name=raimond&age=6")
```

La syntaxe est la même que celle des données de l'url en GET

### attention à l'encodage des données : encodeURIComponent()

```javascript
var text = "renée=tyfany&zerad=ytr&ffsg"

xhr.send('text='+text)
```

Cela donne comme réponse 

`array(3) { ["text"]=> string(13) "renée=tyfany"`  
`["zerad"]=> string(3) `  
`"ytr" ["ffsg"]=> string(0) "" } `
 
  on obtient trois élément alors que l'on passe qu'une seule chaîne de caractère


Maintenant avec encodeURIComponent() on obtient :

```javascript
var text = "renée=tyfany&zerad=ytr&ffsg"
var textEncode = encodeURIComponent(text)
xhr.send('text='+textEncode)
```

et le rendu est :

`array(1) { ["text"]=> string(28) "renée=tyfany&zerad=ytr&ffsg" }` plus qu'un élément


# Et puis il y a FormData()

Pour simplifier la syntaxe de setRequestHeader
On utilise sa méthode `append('key','value')`

```javascript
xhr.open('POST','ajax.php')

var text = "renée=tyfany&zerad=ytr&ffsg"

var data = new FormData()

data.append('text',text)
xhr.send(data)
```

on récupère la même chose que tout à l'heure : `array(1) { ["text"]=> string(28) "renée=tyfany&zerad=ytr&ffsg" }`

Utilisation avec un formulaire

```javascript 
var data = new FormData()

var mf = document.monForm
//  On peut appeler un formulaire par son attribut name
var input = mf[0]
//  On cible le premier input du formulaire à l'index 0

mf.onsubmit = function (e) {
  e.preventDefault()
  //  sinon le formulaire envoie les données en GET dans l'url

  data.append(input.name,input.value)
  xhr.send(data)
```

On peut encore simplifier le traitement dans le cas d'innombrables champs d'input, il suffit d'envoyer directement le formulaire lui-même :

```javascript
var mf = document.monForm
// var input = mf[0]

mf.onsubmit = function (e) {
  e.preventDefault()
  
  var data = new FormData(mf)
//  on injecte ici directement le formulaire dans le FormData

  // data.append(input.name,input.value)
  xhr.send(data)
```

> Envoie toutes les données d'un formulaire

```php
var_domp($_POST)

array(8) { ["ville"]=> string(4) "oran" 
			 ["age"]=> string(2)"43"
			 // checkbox à on si coché sinon n'apparait pas 
			 ["homme"]=> string(2) "on" 
			 ["gentil"]=> string(2) "on"
			 // valeur d'une série de bouton radio 
			 ["couleur"]=> string(3) "red" 
			 ["maried_bool"]=> string(2) "on"
			 // checkbox avec value = "oui je le suis" 
			 ["maried"]=> string(14) "oui je le suis" 
			 ["monTexte"]=> string(9) "salut too" } 
```

**On peut aussi envoyer tout ça de manière synchrone** 

```javascript
  var xhr2 = new XMLHttpRequest()
  
  xhr2.open('GET','addon.html',false)
  // on passe le troisième argument à false
  xhr2.send()

  container.innerHTML += xhr2.response
```
Cette façon de faire bloque la suite de l'exécution du code en cas de problème.

accéder directement à la valeur de action d'un formulaire :
`myForm.action`

```javascript
  xhr.open('POST',mf.action,true)
```
cela vaut : `mf.action : "http://localhost:8888/javascript/ajax.php"`  