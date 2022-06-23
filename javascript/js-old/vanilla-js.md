# Fonction jquery en VanillaJs
## Each()

```javascript
Array.prototype.forEach.call(pTab,function (p) {
  p.style.color = "red"
})
```
`[].forEach.call(pTab,function (p) { ... })` ne fonctionne pas

autre syntaxe __plus rapide au benchtest__ et plus claire je trouve

```javascript
for (var i = 0;i < pTab.length;i++) {
  pTab[i].style.color = "blue"
}
```

## reverse()
inverse l'ordre d'une collection d'élément
```javascript
var collection = document.querySelectorAll('p')

for (var i = collection.length - 1; i >= 0;i--) {
  console.log(collection[i])
}
```

## attr()

rechercher la valeur d'un attribut

```javascript
monElement.getAttribute('href')
monImg.getAttribute('src')```


## trigger('change')

déclencher un événement sur un élément

```javascript
// ancienne façon
// var event = document.createEvent('HTMLEvents')

//nouvelle façon
var event = new Event('change')
event.initEvent('change', true, false)
verifSelect.dispatchEvent(event)```
