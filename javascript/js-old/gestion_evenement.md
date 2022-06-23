# Gestion des évènements

Comment passer des arguments à une fonction dans addEventListener :

```javascript
var somme = function (evt) {
  console.log("a+b",evt.target.a+evt.target.b)
}



var btn = document.getElementsByTagName('button')[0]

btn.a = 6
btn.b = 7

btn.addEventListener('click',somme)

// on obtient bien 13

```

On passe par event.target et on met les valeur dans __element.valeur__



