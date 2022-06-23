# 10 rxjs pluck

`pluck` est une simplification de `map` lorsque l'on travaille avec des objets et que l'on veut récupérer uniquement une propriété particulière.

Sans `pluck` :

```js
const input = document.querySelector('input');

const { fromEvent } = rxjs;
const { debounceTime, distinctUntilChanged, map } = rxjs.operators;

const obs = fromEvent(input, 'input');

obs
.pipe(
	map(e => e.target.value),
	debounceTime(2000),
  distinctUntilChanged()
)
.subscribe(
	x => console.log(x)
)
```

Avec `pluck` :

```js
const input = document.querySelector('input');

const { fromEvent } = rxjs;
const { debounceTime, distinctUntilChanged, pluck } = rxjs.operators;

const obs = fromEvent(input, 'input');

obs
.pipe(
	pluck('target', 'value'),
	debounceTime(2000),
  distinctUntilChanged()
)
.subscribe(
	x => console.log(x)
)
```

`pluck` prend en argument *"le chemin"* vers la propriété souhaitée, ici : `o.target.value`

`o` étant l'objet reçu dans le pipe.