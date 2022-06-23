# 09 rxjs scan vs redux

## Reduce sur un tableau

```js
const t = [0, 1, 2, 3];

const total = t.reduce(
	(acc, val) => acc + val, 
  	accStart = 6
);

console.log(total);
```

Sortie :

```bash
12
```



`reduce` prend deux arguments :

1. une fonction qui prend l'accumulateur et la valeur de la case en court du tableau

2. la valeur de départ de l'accumulateur

## Reduce sur un Observable

```js
const { of } = rxjs;
const { reduce } = rxjs.operators;

const obs = of(1, 2, 3);

obs
.pipe(
	reduce(
  	(acc, val) => acc + val,
  	accStart = 9
  )	
)
.subscribe(
	x => console.log(x)
);
```

Sortie :

```b
15
```

`of` crée un `Observable` avec une liste de valeurs, les valeurs sont émissent de manière synchrone.

`reduce` a la même syntaxe que pour un tableau classique.

## Scan

La différence avec reduce, c'est que scan va émettre chaque résultat intermédiaire.

```js
const { of } = rxjs;
const { scan } = rxjs.operators;

const obs = of(1, 2, 3);

obs
.pipe(
	scan(
  	(acc, val) => acc + val,
  	accStart = 9
  )	
)
.subscribe(
	x => console.log(x)
);
```

Sortie :

```bash
10 # 9 + 1
12 # 10 + 2
15 # 12 + 3
```

Alors que `reduce` n'a de sens que sur un `Observable` ayant une fin (completed), `scan` peut s'utiliser sur un `Observable` infini.