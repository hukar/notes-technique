# 01 rxjs intro

On link la bibliothèque dans le html :

```html
<script src="https://unpkg.com/@reactivex/rxjs@6.3.3/dist/global/rxjs.umd.js"></script>
```

ensuite dans le code on utilise l'objet global `rxjs` :

```js
const btn = document.querySelector('button');

//btn.addEventListener('click', () => console.log('click !'))

const { fromEvent } = rxjs;  // affectation par décomposition
								
fromEvent(btn, 'click').subscribe(
  () => console.log('clicked!')
```

`const { fromEvent } = rxjs` est équivalent à `rxjs.fromEvent`

## permettre un seul click par seconde

### approche classique

```js
const btn = document.querySelector('button');

let dateBefore = Date.now();
const rate = 1000;

btn.addEventListener(
  'click',
  () => {
  
    if((Date.now() - dateBefore) > rate) {
        console.log('clicked !');
        dateBefore = Date.now();
    } else {
      console.log('false');
    }
  }  
)
```

### Avec l 'Observable : fromEvent

```js
const btn = document.querySelector('button');

const { fromEvent } = rxjs;
const { throttleTime } = rxjs.operators;


fromEvent(btn, 'click')
  .pipe(
    throttleTime(3000)
  )
  .subscribe(() => console.log('tickiclick'));
```

#### ! Un Observable renvoie un Observable

Si on veut la collection asynchrone de la valeur de X il suffit de rajouter `map` :

```js
const { fromEvent } = rxjs;
const { throttleTime, map } = rxjs.operators;


fromEvent(btn, 'click')
  .pipe(
    throttleTime(1000),
    map((e) => e.clientX) //  <- maintenant on a une collection de clientX
  )
  .subscribe((data) => console.log('tickiclick : '+data)); // data = clientX
```

`rxjs` a une approche en *entonnoir* du traitement de données asynchrones