# opérateur unaire +

Convertit des nombres

```js
console.log(+"0xA")
> 10
```

# Conversion binaire

```js
parseInt(1011,2)
> 11 // en base dix
```

# différence entre parseInt et +

```js
parseInt("1011adear",10) // 1011
+"1011adear" // NaN
```

# Chaîne de caractère

Une chaîne de caractère est considéré comme un objet, on peut donc utiliser des méthodes et des attributs

```js
"une phrase".replace("une","prout").toUpperCase() // PROUT PHRASE
"michele".charAt(3) // h
"quel longueur".length // 13
```

# Timer en seconde

```js
// s étant le temps du timer en seconde
var s = 10, t = parseInt(+new Date()/1000) + s

while(parseInt(+new Date()/1000)<t);
```

Avec une boucle for

```js
for(
		var t = parseInt(new Date()/10),tmax = t + 300;
		t < tmax;
		t = parseInt(+new Date()/10)
	);
```

# déclaration de variables

`let` portée de bloc

```js
if (true) {
	let mimi = "mimi"
	console.log(mimi)
}

console.log(mimi)

> mimi
> ReferenceError: mimi is not defined
```

`const` déclaration de constante

```js

const PI = 3.14
PI = 1

> 