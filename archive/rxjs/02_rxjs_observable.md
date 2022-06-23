# 02 rxjs Observables, Observers et Subscriptions

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/observable-observer-01.png)

L'`Observable` est une collection de données asynchrone ou synchrone.

L'`Observer` est une collection de trois méthode appelées pour chaque nouvelle valeur de l'`Observable`.

## Subscribe()

La méthode `subscribe` prend soit une liste de fonction :

```js
subscribe(
  (e) => console.log(e.clientX),
  (error) => console.log('error : ' + error),
  () => console.log('completed')
);
```

ou un objet `observer` :

```js
const observer = {
	next: (data) => console.log(data.clientX),
  error: (err) => console.log(err),
  complete: () => console.log('completed')
};

subscribe(observer);
```

La méthode `subscribe` renvoie une `subscription`

## Créer un Observable

Observable possède la méthode create :

```js
const { Observable } = rxjs;

const observer = {
	next: (data) => console.log(data),
  	error: (err) => console.log(err),
  	complete: () => console.log('completed')
};

Observable.create(
	(obs) => {
  		obs.next('titi')
  	}
)
.subscribe(observer);
```

La méthode `create` prend une fonction anonyme qui a un `Observer` comme argument.

Pour émettre une donnée on utilise `obs.next(data)`

#### ! Quand une erreur est émise, l'Observable est finis

```js
const { Observable } = rxjs;

const observer = {
	next: (data) => console.log(data),
  	error: (err) => console.log('error : ' + err),
  	complete: () => console.log('completed')
};

Observable.create(
	(obs) => {
        obs.next('titi');
        obs.error('patate!!');
        obs.next('toto');
      }
)
.subscribe(observer);
```

En sortie :

```bash
titi
error : patate!!
```

#### Quand un Observer utilise complete(), l'Observable est finis

```js
const { Observable } = rxjs;

const observer = {
	next: (data) => console.log(data),
  	error: (err) => console.log('error : ' + err),
  	complete: () => console.log('completed')
};

Observable.create(
	(obs) => {
        obs.next('titi');
        obs.complete();
        obs.next('toto');
      }
)
.subscribe(observer);
```

`'toto'` ne s'affiche pas après un `obs.complete`

```bash
titi
completed
```

## Avec un Observable asynchrone

```js
const { Observable } = rxjs;

const observer = {
	next: (data) => console.log(data),
  	error: (err) => console.log('error : ' + err),
  	complete: () => console.log('completed')
};

Observable.create(
	(obs) => {
        obs.next('titi');
        setTimeout(() => {
            obs.complete();
        },2000);
        obs.next('toto');
  	}
)
.subscribe(observer);
```

On obtient la sortie :

```bash
titi
toto
completed
```

## Créer *from scratch* `fromEvent`

```js
const btn = document.querySelector('button');

const { Observable } = rxjs;

const observer = {
	next: (data) => console.log(data),
  	error: (err) => console.log('error : ' + err),
  	complete: () => console.log('completed')
};

Observable.create(
	(obs) => {
  	btn.onclick = (e) => {
    	obs.next(e);
    }
  }
)
.subscribe(observer);
```

On obtient ainsi le même comportement qu'avec `fromEvent`.

## !! Éviter les memory leak

Un observable sans méthode complete est susceptible de fonctionner à l'infini.

Dans une ***single page application*** cela peut avoir des conséquence sur la mémoire.

Si l'Observable n'est jamais complété, alors il faut se désabonner :

```js
const subscription = Observable.create(
	(obs) => {
  	btn.onclick = (e) => {
    	obs.next(e);
    }
  }
)
.subscribe(observer);

setTimeout(
() => subscription.unsubscribe()
,4000)
```

On enregistre la `souscription` dans une variable et on se dés-inscrit avec `unsubscribe`