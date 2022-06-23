## append

```js
elt.appendChild(child)
```

## prepend

```js
elt.insertBefore(child,elt.firstChild)
```
## créé un élèment
```js
var elt = document.createElement('tagName')
```

## tester une expression régulière

```js
/^debut/.test(str) // retourne un booléen
```

Avec une variable

```js
new RegExp(maVar).test(str)
```

## retirer un attribut

```js
elt.removeAttribute(attributeName)
```

## provoquer un événement sur un élément

```js
elt.dispatchEvent(new Event("change"))
```
## retirer un mot d'un texte et par extension une classe

```js
	elt.className = elt.className.replace(" "+classStr,"")
```

Le premier paramètre peut être une expression régulière

# syntaxe auto invocation fonction

```js
function () {
  console.log("coco")
}()

> SyntaxError: Unexpected token (1:9)
```

Dans ce cas il faut utiliser des parenthèses englobantes

```js
(function () {
  console.log("coco")
})()

> coco
```

`( function () { ... } )( )`

En anglais cela s'appel : IIFE (Immediately Invokable Function Expressions)