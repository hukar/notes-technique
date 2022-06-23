# 13 rxjs `BehaviorSubject`

`BehaviorSubject` est utilisé pour donner une valeur initial à `Subject`.

Lorsqu'on clique sur le bouton, on veut afficher *"clicked !"*, l'affichage initial étant *"not clicked !"*

Juste avec Subject :

```js
const btn = document.querySelector('button');
const output = document.querySelector('p');
const { fromEvent, Subject } = rxjs;

const fe = fromEvent(btn, 'click');
const sub = new Subject();

sub.subscribe(
	x => output.textContent = x
);

fe
.subscribe(
	() => sub.next('Clicked !')
)

sub.next('not clicked !');
```

Le problème qu'on doit émettre la valeur initial en dehors du `fromEvent`.

Avec `BehaviorSubject` :

```js
const textArray = [
	'clicked !',
  'hey',
  'joe',
  'what do you want ?',
  'my wife !'
];

const { fromEvent, BehaviorSubject } = rxjs;

const fe = fromEvent(btn, 'click');
const bs = new BehaviorSubject('not clicked !');

bs.subscribe(
	x => output.textContent = x
);

fe
.subscribe(
	() => bs.next(textArray.shift())
);
```

