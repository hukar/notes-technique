# setTimeout et setInterval

Petit script présenatnt l'utilisation conjointe de setTimeout (avec un petit o) et setInterval

>on passe une fonction avec variable via cette syntaxe ``"somme(4,8"``

```javascript
var somme = function (a,b) {
  console.log("a = b",a+b)
}

var monAction = function () {
  clearInterval(timer)
}

var timer = setInterval("somme(4,6)",1000)

setTimeout(monAction,5000)
```