# 06 rxjs filter

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/rxjs-filter.png)

`filter` est un `operators` qui prend un **predicat** (f° évaluant une proposition et renvoyant un booléen).

## exemple

```js
const { interval } = rxjs;
const { filter } = rxjs.operators;

const obs = interval(1000);

const subs = obs
.pipe(
  filter(
    x => x % 3 === 0
  )
)
.subscribe(
	x => console.log(x)
)

const btn = document.querySelector('button');

btn.onclick = () => subs.unsubscribe();
```

