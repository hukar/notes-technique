# 03 rxjs operators

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/rxjs_operators.png)

On utilise les operators comme un entonnoir à données (funnel).

```js
const btn = document.querySelector('button');

const { interval } = rxjs;
const { map, filter } = rxjs.operators;

const subscription = interval(1000)
.pipe(
	map(
  	x => x * 3
  ),
  filter(
  	x => x % 2 === 0 && x % 4 === 2
  )
)
.subscribe(
	data => console.log(data)
);

btn.onclick = () => subscription.unsubscribe();
```

on obtient :

```bash
6
18
30
42
54
```

`interval` : prend un nombre de milliseconde en argument et émet une suite d'entier en commençant par zéro.

`pipe` renvoie un observable.

## Autre exemple avec `throttleTime`

```js
const { interval } = rxjs;
const { map, throttleTime } = rxjs.operators;

const subscription = interval(1000)
.pipe(
	map(
  	x => x   
  ),
  throttleTime(1000)
)
.subscribe(obs);

btn.onclick = () => subscription.unsubscribe();
```

sortie :

```bash
0
2
4
6
```

`throttleTime` définie une durée pendant laquelle les nouvelles valeurs sont ignorées.

Ici on voit qu'une valeur sur deux est ignorée.