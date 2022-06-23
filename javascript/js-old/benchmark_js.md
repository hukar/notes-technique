
## Fonction pour un mini benchmark
```javascript
var timer = function (label) {
  var start = new Date()

  return {
    stop : function () {
      var end = new Date()
      var time = end.getTime() - start.getTime()
      console.log("temp pour "+label+" : "+time)
    }
  }
}
```
On utilise comme ça :
on lance le timer
```javascript
var t01 = timer("selection par id")
```
on execute son code  
```javascript
var elt01 = document.getElementById('mon-par')
```
on stop le timer qui affiche le résultat

```javascript
t01.stop()
```

Pour optimiser le résultat on peut utiliser `performance.now()` précision d'une microseconde exprimé en milliseconde

`performance.now()` renvoie le temps écoulé depuis que le navigateur est prêt pour aller chercher le document
### exemple d'utilisation pour benchmarker :

```javascript
var t1 = performance.now()

var elt02 = document.querySelector('#mon-par')

var t2 = performance.now()

var diff = t2 - t1
console.log("result :",diff)
```

On peut aussi utiliser
```javascript
console.profile("mon label")
// code
console.profileEnd()
```
et après on regarde les outils chrome à **profile**

### et pour finir ...

**console.time()** qui s'utilise comme ceci :
```javascript
console.time("queryselector")
var elt02 = document.querySelector('#mon-par')
console.timeEnd("queryselector")
```
et qui affiche le temps dans la console directement

### petite fonction de _benchtest_

```javascript
var benchtest = function (fct,label) {
  var t1 = new Date().getTime()
  var cpt = 0
  console.log("t1",t1)
  while (true) {
    cpt++
    fct ()
    var t2 = new Date().getTime()
    if ( t2 >= t1 + 1000) {
      break
    }
  }
  console.log("nombre de fois que la fonction est exécutée en 1s : ")
  console.log(label,cpt)
  console.log("t2",t2)
}
```
__fct__ étant la fonction à tester
__label__ du texte à afficher dans la console

Résultat dans la console :
`t1 1480251256514
script.js:14`
`nombre de fois que la fonction est exécutée en 1s :`
`query selector 3801573`
`t2 1480251257514`
