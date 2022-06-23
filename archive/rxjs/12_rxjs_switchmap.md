# 12 rxjs switchMap

Si on veut démarrer un Observable à partir d'un autre on serait tenté de faire ça :

```js
const btn = document.querySelector('button');

const { fromEvent, interval } = rxjs;
const { switchMap } = rxjs.operators;

const obs1 = fromEvent(btn, 'click');
const obs2 = interval(1000);

obs1
.subscribe(
	x => obs2.subscribe(x => console.log(x))
)
```

Le problème c'est qu'a chaque click sur le bouton, une nouvelle souscription est lancée alors qu'on voudrait arrêter l'ancienne avant de lancer la nouvelle.

Sortie :

```bash
7
12
8
2
8
2
8
2
8
2
9
8
2
9
8
13
9
3
9
3
9
3
9
3
10
9
3
10
```

## Utilisation de switchMap

```js
const btn = document.querySelector('button');

const { fromEvent, interval } = rxjs;
const { switchMap } = rxjs.operators;

const obs1 = fromEvent(btn, 'click');
const obs2 = interval(1000);

obs1
.pipe(
	switchMap(
  	event => obs2
  )
)
.subscribe(
	x => console.log(x)
)
```

`switchMap` se charge de désinscrire l'`Observable` avant de le relancer.

Sortie :

```bash
0
1
2
0 # click
1
0 # click
1
2
3
```

