# 07 rxjs debounceTime

Á la différence de throttleTime qui ne prends un data que tout les n millisecondes, debounceTime, lui ne prend un data que lorsqu'il y a eu n millisecondes de calme.

```js
const { fromEvent } = rxjs;
const { debounceTime } = rxjs.operators;

const input = document.querySelector('input');

const obs = fromEvent(input, 'input');

obs
.pipe(
	debounceTime(2000)
)
.subscribe(
	x => console.log(x.target.value)
)
```

Ici, on attend 2 secondes où l'input reste inchangé avant d'afficher sa value.

Cela évite d'afficher à chaque fois que quelque chose est tapé.

Là, au contraire, on ne montre value que lorsque l'utilisateur a fini de taper.