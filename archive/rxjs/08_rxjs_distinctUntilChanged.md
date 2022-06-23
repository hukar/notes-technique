# 08 rxjs distinctUntilChanged

Cet `operators` permet de ne pas reprendre une émission si celle-ci est identique à la précédente.

```js
const { fromEvent } = rxjs;
const { debounceTime, distinctUntilChanged, map } = rxjs.operators;

const xhr = new XMLHttpRequest();
const url = 'https://ng-recipe-book-afe23.firebaseio.com/recipes.json';

const input = document.querySelector('input');

const obs = fromEvent(input, 'input');

obs
.pipe(
	map( x => x.target.value),
	debounceTime(2000),
  distinctUntilChanged()
)
.subscribe(
	x => {
  	console.log(x);
    xhr.open('GET', url);
    xhr.send();
  }
)
```

Cet exemple permet de rentrer du texte lorsque l'utilisateur ne tape plus depuis 2 secondes et que ce texte est différent du précédent.

Il est intéressant d'avoir ce comportement si on envoie une requête `ajax` dans le `subscribe`. 

Ici la requête n'est envoyée que après deux secondes d'inactivité **et** si l'entrée set différente de la précédente.