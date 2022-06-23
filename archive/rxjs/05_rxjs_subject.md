# 05 rxjs Subject

Si l'`Observable` a une approche passive des données, `Subject`, lui a une méthode `next` le rendant plus "actif".

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/rxjs-subject.png)

## next, complete, error

Subject possède trois méthode de gestion "*Active*" du flux de données.

### next

```js
const obs = {
	next: data => console.log(data),
  	error: err => console.log(err),
  	complete: () => console.log('COMPLETED !')
};

const { Subject } = rxjs;

const sub = new Subject();

sub.subscribe(obs);

sub.next('coucou');

sub.next('titi');

console.log(sub);
```

Sortie :

```bash
coucou
titi
a # sub
    closed:false
    hasError:false
    isStopped:false
    observers:[a] #length: 1 c'est obs
   	thrownError: null
```

### next + complete

```js
...
sub.next('coucou');

sub.complete();

sub.next('titi');

console.dir(sub);
```

Sortie :

```bash
coucou
COMPLETED !
a # sub
    closed:false
    hasError:false
    isStopped:true
    observers:[]
    thrownError: null
```

`"titi"` ne s'affiche pas .

`isStopped` est à true.

Le tableau d'`Observers` est vidé.

### next + error

```js
sub.next('coucou');

sub.error('aye');

sub.next('titi');

console.dir(sub);
```

Sortie :

```bash
coucou
aye
a # sub
    closed:false
    hasError:true
    isStopped:true
    observers:[]
    thrownError: "aye"
```

`"titi"` ne s'affiche pas .

`hasError` est à `true`.

`isStopped` est à `true`.

Le tableau d'`Observers` est vidé.

`thrownError` prend la valeur `"aye"`.