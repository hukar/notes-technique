# 11 rxjs mergeMap

On a deux input text, et dans un premier temps chaque subscribe écrase la valeur de l'autre :

```js
const input1 = document.querySelector('#input1');
const input2 = document.querySelector('#input2');
const span = document.querySelector('span');

const { fromEvent } = rxjs;
const { debounceTime, distinctUntilChanged, pluck } = rxjs.operators;

const obs1 = fromEvent(input1, 'input');

obs1
.subscribe(
	e => span.textContent = e.target.value
)

const obs2 = fromEvent(input2, 'input');

obs2
.subscribe(
	e => span.textContent = e.target.value
)
```

On veut maintenant plutôt merger les résultats :

```js
const { fromEvent } = rxjs;
const { mergeMap, map } = rxjs.operators;

const obs1 = fromEvent(input1, 'input');
const obs2 = fromEvent(input2, 'input');

obs1
.pipe(
	mergeMap(
  	event1 => obs2.pipe(
    	map(
    	event2 => event1.target.value +
      					' ' + 
                event2.target.value
    	)
    )
  )
)
.subscribe(
	combinedValue => span.textContent = combinedValue
)
```

Si on tape dans le premier `input` rien ne se passe, il faut entrer quelque chose dans le deuxième `input` pour voir le message entier affiché.

C'est l'Observable inclus dans le deuxième qui émet la donnée combinée.

C'est pour ça que rien ne s'affiche tant que l'on ne rentre rien dans le `input2` .

### `mergeMap` pour le premier et `map` pour le deuxième



