# Observable

## librairie rxjs

Un `Observable` est comme une collection de data qui arrivent de manière asynchrone au lieu d'être stockées dans la mémoire.

Un `eventListener` est une forme d'`Observable`

Gestion d'événement traditionnelle:

```js
const button = document.getElementsByTagName('button')[0];

const handler = (e) => {
  alert('click');
  button.removeEventListener('click', handler);
}

button.addEventListener('click', handler);
```

Maintenant avec la librairie `ReactiveX` :

```js
const Observable = Rx.Observable;

const button = document.getElementsByTagName('button')[0];

const clicks = Observable.fromEvent(button, 'click');

clicks.forEach((e) => console.log('event ', e.x));
```

### Gestion classique des erreurs dans un forEach

```js
try {
  [2, 5, 0, 8, 9].forEach((i) => {    
    if (5/i === Infinity) throw 'pas de division par zéro';
    console.log(5/i);  // <- 1
  })

  console.log('DONE!')  // <- 3
} catch(e) {
  console.log(e)  // <- 2
}
```

1. Traitement de l'élément
2. Traitement de l'erreur
3. Tâche compléter

Si on fait la même chose avec un `Observable` :

```js
try{
  clicks.forEach((e) => {
    console.log('event ', e.x);
    if(e.x%2 === 0) throw 'pas de valeur paire acceptée';
  });
  
  console.log('DONE!');
} catch(err) {
  console.log(err);
}

console.log('suite du programme')
```

On obtient directement en console:

```bash
"DONE!"
"suite du programme"
```

Les erreurs ne sont jamais traitées, comme la collection est asynchrone le programme est déjà sorti du ***try and catch*** lorsque l'erreur dans le `forEach` se produit.

En fait `Observable.forEach` accepte trois *callback* :

```js
clicks.forEach(
  function onNext(ev) {
    console.log('event ', ev.x);
    throw 'pas de valeur paire acceptée';
  },
  function onError(error) {
    console.log(error);
  },
  function onCompleted() {
    console.log('COMPLETE!');
  } 
);
```

#### ! la gestion des erreurs ne fonctionne pas

Pour pouvoir arrêter l'abonnement à l'événement click (removeEventListener), on l'enregistre dans un variable et on la clos avec `monAbonnement.dispose()`:

```js
const subscription = clicks.forEach(  // <- on stocke l'abonnement dans subscription
  function onNext(ev) {
    console.log('event ', ev.x);
    // throw 'pas de valeur paire acceptée';
    if (ev.x%2 === 0) subscription.dispose(); // <- on annule l'abonnement si la valeur est paire
  },
  function onError(error) {
    console.log(error);
  },
  function onCompleted() {
    console.log('COMPLETE!');
  } 
);
```

## Transformer un Observable en un autre

```js
const clicks = Observable.fromEvent(button, 'click');

const points = clicks.map((e) => {
  return { x: e.x, y: e.y}
} );
```

Le nouvel `Observable` points '*emmet'* des points de façon asynchrone, on peut utiliser `forEach` dessus :

```js
const subscription = points.forEach(
  function onNext(p) {
    console.log('point ', JSON.stringify(p));
  },
  function onError(error) {
    console.log(error);
  },
  function onCompleted() {
    console.log('COMPLETE!');
  } 
);
```

#### ! un Observable est une collection de data asynchrone

