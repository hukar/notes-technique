# 02 Moderne `Javascript`

On utilise l'extension `Live Server`.

Pour rendre l'utilisation de `var` obligatoire on doit ajouter `'use strict` en haut du script `js`.

```js
'use strict'
myname = "hukar" // lance une exception
```

```js
'use strict'
var myname = hukar // fonctionne correctement
```



## `let`

```js
for(var x = 0; x <= 11; x++) {}

console.log(x);
```

```bash
12
```

Ce comportement est dû au `hoisting` lié à `var`, on a en fait :

```cs
var x = 0

for(x = 0; x <= 11; x++) {}

console.log(x);
```

Pour éviter ce comportement on a le mot clé `let` :

```cs
for(let x = 0; x <= 11; x++) {}

console.log(x);
```

```js
ReferenceError: x is not defined
```

Ce qui est plus attendu.

`let` n'utilise pas le `hoisting` mais la protée du bloc dans lequel il est utilisé. (ici le bloc `for`)



## Arrow Function `this`

la valeur de `this` est binder avec l'environnement où l'`arrow function` est déclarée et non l'objet appelant comme avec une fonction classique.

Pour résumer l'`arrow function` n'a pas de `this` propre.

```cs
function Toto() {
    this.name = "toto";
    this.sayMyName = () => console.log(`I'm ${this.name}`)
}

var toto = new Toto();

toto.sayMyName();
```

```
I'm toto
```



## `Promises` et `async`

```cs
var data = new Promise((resolve, reject) => {
    setTimeout(() => {
        resolve({
            "userId": 1,
            "id": 1,
            "title": "delectus aut autem",
            "completed": false
            })
    }, 1500)
})

// data.then(console.log)

const executeData = async () => {
    let response = await data

    console.log(`response : ${JSON.stringify(response)}`)       
}

executeData()
```

Avec `fetch` :

```js
<h1>Hello Promise</h1>
<script>
  function executeRequest() {
  fetch('https://jsonplaceholder.typicode.com/todos/1')
    .then(response => response.json())
  	.catch(error => alert('Something bad happened'))
    .then(console.log)
	}

	executeRequest()
</script>
```

Avec `async` et `await`

```js
async function executeRequest() {
  const response = await fetch('https://jsonplaceholder.typicode.com/todos/1')
  const json = response.json()
  
  console.log(json)
}

executeRequest().catch(error => alert('Something bad happened !!!'))
```

On peut utiliser un `catch` sur la fonction finale pour gérer les exceptions.



## `Module`

### `export`

`person.js`

```js
const firstname = "Hukar";
const age = 45;
const hobby = "jiujitsu";

export { firstname, age, hobby };
```

`index.js`

```js
import { firstname, age, hobby } from "./person";

document.getElementById("app").innerHTML = `
<h1>Hello Vanilla!</h1>
<div>
  ${firstname} ${age} ${hobby}
</div>
`;
```

### `export as default`

```js
const firstname = "Hukar";
const age = 45;
const hobby = "jiujitsu";

export { firstname as default, age, hobby };
```

Maintenant `firstname` est l'export par défaut :

```js
import firstname, { age, hobby } from "./person";
```



### Utiliser des alias

Pour l'export par défaut, on peut choisir le nom que l'on veut.

Pour les imports par decomposition, on utilise le mot clé `as`.

```js
import myname, { age, hobby as sport } from "./person";

document.getElementById("app").innerHTML = `
<h1>Hello Vanilla!</h1>
<div>
  ${myname} ${age} ${sport}
</div>
`;
```



### Syntaxe alternative

```js
export const firstname = "Hukar";
export const age = 45;
export const hobby = "jiujitsu";
```

```js
import { firstname as myname, age, hobby as sport } from "./person";

document.getElementById("app").innerHTML = `
<h1>Hello Vanilla!</h1>
<div>
  ${myname} ${age} ${sport}
</div>
`;
```























