# 19. angular observable

observable => data source

3 *'Boîtes'* (où le code peut s'exécuter) : Handle Data | Handle Error | Handle Completion

observer => code

## Utilisation de RxJs

Il faut installer un module de compatibilité :

```bash
npm install --save rxjs-compat
```



## Observable

Les imports :

```typescript
// tslint:disable-next-line:import-blacklist
import { Observable } from 'rxjs/Rx';
// tslint:disable-next-line:import-blacklist
import 'rxjs/Rx';
```

On doit désactiver le linter.

Pour utiliser certain opérateur de Observable on doit importer tout le package `rxjs/Rx`.

un Observable renvoyant un nombre à interval régulier :

```typescript
const myNumbers = Observable.interval(1000);

myNumbers.subscribe(
    (number: number) => console.log('number :',number)
);
```

```sh
1
2
3
...
```

Un Oservable de base :

```typescript
const myObservable = Observable.create(
      (observer: Observer<string>) => {
        setTimeout(() => {
          observer.next('first package');  // envoie une data
        }, 2000);
        setTimeout(() => {
          observer.next('second package');  // envoie une data
        }, 4000);
        // setTimeout(() => {
        //   observer.error('this does not work');  // emet une erreur
        // }, 5000);
        setTimeout(() => {
          observer.complete(); // complète l'observation
        }, 5000);
        setTimeout(() => {
          observer.next('third package');  // n'est pas affiché car après complete()
        }, 6000);
      }
    );

    myObservable.subscribe(
      (data: string) => console.log(data),
      (error: string) => console.log(error),
      () => console.log('completed')
    );
```

```typescript
myObservable.subscribe(
      (data: string) => 'en cours',
      (error: string) => 'erreur',
      () => 'terminé'
    );
```

On peut envoyer trois fonction en paramètre de subscribe pour dire ce que l'on doit faire si on est en cours d'observation, qu'il y a eu une erreur ou que l'observation est terminée.

## Éviter les fuites de mémoire

Dans le cycle de vie d'un composant, il faut obligatoirement se désabonner d'un observable avec `unsubscribe` dans `ngOnDestroy`.

```typescript
// import ...

import { Observable } from 'rxjs/Observable';
import 'rxjs/Rx';
import { Observer } from 'rxjs/Observer';
import { Subscription } from 'rxjs/Rx';

@Component({ /* ... */ })
export class HomeComponent implements OnInit, OnDestroy {  // on implémente onDestroy

  numSubscription: Subscription;  // on crée deux variable contenant l'abonnement

  dataSubscription: Subscription;

  ngOnInit() {
    const myNumbers = Observable.interval(1000);
    this.numSubscription = myNumbers.subscribe(/* ... */);

    const myObservable = Observable.create(/* ... */);
    this.dataSubscription = myObservable.subscribe(/* ... */);
  }

  ngOnDestroy(): void {
    this.numSubscription.unsubscribe();  // on annule l'abonnement
    this.dataSubscription.unsubscribe();
  }
}
```

## Utilisation de Subject (remplace les Event)

C'est un Observable et un Observer en même temps.

Création d'unservice `data.service .ts` :

```typescript
import { Subject } from 'rxjs/Subject';

export class DataService {
    dataSub = new Subject();
}
```

#### ! de bien importer `rxjs/Subject` et pas `rxjs`  car sinon toute la librairie est chargée

Dans un composant *émetteur* :

```typescript
addData(data: string) {
    this.dataService.dataSub.next(this.userName);
  }
```

pour envoyer une nouvelle valeur via `next`  (remplace emit)

 

Dans un composant *récepteur*

```typescript
this.dataService.dataSub.subscribe(
      (userName: string) => {
        this.userName = userName;
      }
    );
```

`sub.next(data)` pour émettre une donnée

`sub.subcribe((data) => { ... })` pour réagir à l'émission de la donnée

## Les opérateurs rxjs

Il faut l'import suivant pour les utiliser :

```typescript
import 'rxjs/Rx';
```



Les opérateurs renvoient un observable et permettent donc d'être chaînés :

```typescript
const myNumbers = Observable
        .interval(1000)
        .map(
          (data: number) => {
            return data + 1;
          }
        )
        .filter(
          (data: number) => data % 2 !== 0
        );
```

utilisation ici de map et filter

écriture plus compact :

```typescript
this.myNumbers = Observable
    	.interval(1000)
    	.map(x => x * 3)
    	.filter(x => x % 2 === 0);
```



## Se passer de rxjs-compat

Retirer la ligne de `package.json`

```json
	...
	"rxjs": "^6.0.0",
    "rxjs-compat": "^6.1.0",  // <- supprime cette ligne
    "zone.js": "^0.8.26"
  },
```



```sh
npm uninstall --save rxjs-compat
```

Pour désinstaller rxjs-compat

Quelques modifications dans les imports :

```typescript
import { interval, Subscription } from 'rxjs';
// au lieu de import { something } from 'rxjs/something'

import { map, filter } from 'rxjs/operators';
// au lieu de import "rxjs/Rx
```

et utilisation de pipe pour les *operators* :

```typescript
const myNumbers = interval(1000).pipe(
      map(
        (data: number) => {
          return data + 1;
        }
      ),
      filter(
        (data: number) => data % 2 !== 0
      )
    );
```

<u>note</u>: écriture raccourci de `Observable.interval` par juste `interval`

## ! attention memory leak

```typescript
ngOnDestroy() {
    this.subscription.unsubcribe();
}
```

## Annexe de notes sur RxJs Observable

un observable = un flux de données qui arrive de manière asynchrone

​			   = Une seule donnée asynchrone -> requête HTTP

​			   = plusieurs données -> web sockets

​			   = données synchrone

​			   = zéro donnée

### Trois types de notification

1. next = arrivée d'une nouvelle valeur
2. error = casse observable termine l'observable
3. complete = termine l'observable

### Opérateurs

Renvoie toujours un Observable.

### Souscription

```js
const subscription = observable.subscribe(
    value => console.log(value),
    error => console.log(error),
	() => console.log('complete')
)
// ou on passe un objet
const subscription = observable.subscribe({
    next: value => console.log(value),
    error: err => console.log(err),
	complete: () => console.log('complete')
})
```

annuler la souscription :

```js
subscription.unsubscribe();
```

