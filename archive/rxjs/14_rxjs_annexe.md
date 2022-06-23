# 14 rxjs annexe

## take

n'émet que le nombre d'élément en argument et se complete.

```js
const { interval } = rxjs;
const { take, map } = rxjs.operators;

interval(1000)
.pipe(
  take(3)
)
.subscribe(
  x => console.log(x),
  null,
  () => console.log('completed')
)
```

Sortie :

```bash
0
1
2
"completed"
```

