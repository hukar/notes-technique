# Routing dans angular

On configure les routes dans app.module.ts

```typescript
...
import { Routes, RouterModule } from '@angular/router';  // <- ici

...


const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent },
    { path: 'servers', component: ServersComponent },
];  // on définie les routes dans une constante



@NgModule({
...
  imports: [
    ...,
    RouterModule.forRoot(appRoutes)  // <- on charge les routes
  ],
  providers: [ServersService],
  bootstrap: [AppComponent]
})
export class AppModule { }




```

Pour afficher les routes dans le layout on utilise la directive `router-outlet`

```html
<div class="row">
    
    <div class="col mt-3">
        
        <router-outlet> </router-outlet>

	</div>
</div>
```

On effectue la navigation avec des routes :

```html
<ul class="nav nav-tabs">
    <li role="presentation" class="nav-item">
        <a href="" class="nav-link active">Home</a>
    </li>
    <li role="presentation" class="nav-item">
        <a href="users" class="nav-link">Servers</a>
    </li>
    <li role="presentation" class="nav-item">
        <a href="servers" class="nav-link">Users</a>
    </li>
</ul>
```

En utilisant href, la page se reload, ce qui n'est pas ce que l'on veut.

Il faut employer la directive `routerLink` :

```html
<ul class="nav nav-tabs">
    <li role="presentation" class="nav-item">
        <a routerLink="" class="nav-link active">Home</a>
    </li>
    <li role="presentation" class="nav-item">
        <a routerLink="servers" class="nav-link">Servers</a>
    </li>
    <li role="presentation" class="nav-item">
        <a routerLink="users" class="nav-link">Users</a>
    </li>
</ul>
```

notation alternative

```html
<a [routerLink]="['users']" class="nav-link">Users</a>
```

## Différence entre les path

```html
<a routerLink="servers">hello servers !</a>
<a routerLink="/servers">hello servers !</a>
<a routerLink="./servers">hello servers !</a>
<a routerLink="../servers">hello servers !</a>
```

on obtient les routes suivantes :

```sh
http://localhost:4200/users/servers
http://localhost:4200/servers
http://localhost:4200/users/servers
http://localhost:4200/servers
```

`"servers"` path relatif au path du  composant où il se situe

`"/servers"` et `"./servers"` path absolue

`"../servers"` remonte le path d'un niveau

<u>pour rappel</u>

`/`  root directory

`./` current diretory

`../` parent directory

## Relier une classe au path

Si on change de route, on aimerait pouvoir ajouter ou retirer la classe active à un élément de menu :

```html
<ul class="nav nav-tabs">
    <li role="presentation" class="nav-item">
        <a routerLink="" 
           routerLinkActive="active"
           [routerLinkActiveOptions]="{exact: true}"
           class="nav-link">Home</a>
    </li>
    <li role="presentation" class="nav-item">
        <a routerLink="servers"
           routerLinkActive="active" 
           class="nav-link">Servers</a>
    </li>
    ...
</ul>
```

On "Bind" la classe active grace à la directive `routerLinkActive`

comme le path `""` (qui est la route `/`) apparait dans `/servers` et `/users` et `/`, la route `/` est toujours active.

Pour corriger on utlise `[routerLinkActiveOptions]="{exact: true}"`

On met les crochets car le contenu est un objet javascript et non un string (crochet optionnel avec les string cf. directive advanced)

## Naviguer dans le code

On peut déclencher la navigation depuis une méthode :

```typescript
import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';  // <- 1. on importe le router

@Component({ ... })
export class HomeComponent implements OnInit {

  constructor(private router: Router) { }  // <- 2. on injecte sa dépendance

  ngOnInit() {
  }

  onLoadServers() {
    console.log('hello router');
    this.router.navigate(['/servers']);  // <- 3. on utilise la méthode navigate
  }

}
```

1. importer `Router` de `@angular/router`

2. injecter le routeur dans le *'constructor'*

3. utiliser la méthode `this.router.navigate(['path'])`

### option de navigate

  Avec this.router.navigate(['servers']), je n'obtiens pas d'erreur car par defaut navigate part de root ( `/` ).

Je peut utiliser la route active en option :

on importe ActivatedRoute

```typescript
import { Router, ActivatedRoute } from '@angular/router';
	...
constructor(private router: Router, private route: ActivatedRoute) { }
```

On définie des options dans navigate :

```typescript
 onReload() {
      this.router.navigate(['servers'], {relativeTo: this.route});
  }
```

## Route paramétrée

dans app.module.ts

```typescript
const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent },
    { path: 'users/:id/:name', component: UserComponent },  // <- ici
    { path: 'servers', component: ServersComponent },
];
```

Dans le composant :

```typescript
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';  // <- 1. import

@Component({ ... })
export class UserComponent implements OnInit {
  user: {id: number, name: string};

  constructor(private route: ActivatedRoute) { }  // <- 2. injection

  ngOnInit() {
    this.user = {
      id: this.route.snapshot.params['id'],  // <- 3. récupération de paramètre :param
      name: this.route.snapshot.params['name']  // <- 3. récupération de paramètre :param
    };
    console.log(this.user);
  }

}
```

Syntaxe du path : `path/:param1/:param2`

Récupération des paramètres avec `this.route.snapshot.params['monParam']`

## Définir une route paramètrée dans le html

```html
<a [routerLink]="['/users', 10, 'anna']">Anna 10</a>
```

On utilise les crochets car on passe un tableau : `['/users', 10, 'anna']`

On a  la route suivie des paramètres => `/users/10/anna`

## Recharger un composant avec de nouveaux paramètres

par défaut un composant charger ne se recharge pas s'il est de nouveau appelé par une route.

pour mettre à jour le composant avec les nouvelles valeurs de paramètre, il faut utiliser un **observable**.

Nouveaux code :

```typescript
import { ActivatedRoute, Params } from '@angular/router'; // <- deux imports

ngOnInit() {
    this.user = {
      id: this.route.snapshot.params['id'],
      name: this.route.snapshot.params['name']
    };
    this.route.params.subscribe(
      (params: Params) => {
        this.user.id = params['id'];
        this.user.name = params['name'];
      }
    );
  }
```

`this.route.params` est un observable qui exécute une fonction quand la valeur des paramètres change



### de même pour queryParams et fragment

```typescript
this.route.queryParams.subscribe(
    (queryParams: Params) => {
        console.log(queryParams);

        this.allowEdit = queryParams['allowEdit'] === '1' ? true : false;
    }
);
this.route.fragment.subscribe(
	(fragment: Params) => { ... }
);
```



## Ce que Angular fait derrière la scène

```typescript
import { Component, OnInit, OnDestroy } from '@angular/core';  // <- OnDestroy
import { ActivatedRoute, Params } from '@angular/router';
import { Subscription } from 'rxjs';  // <- Subscription

@Component({ ... })
export class UserComponent implements OnInit, OnDestroy {
            ...
    subscription: Subscription;  // <- on déclare une variable de type subscription

    constructor(private route: ActivatedRoute) { }

    ngOnInit() {
        this.user = {
            id: this.route.snapshot.params['id'],
            name: this.route.snapshot.params['name']
        };
    // on mémorise la subcription dans une variable
        this.subscription = this.route.params.subscribe(
            (params: Params) => {
                this.user.id = params['id'];
                this.user.name = params['name'];
                console.log('something change');
            }
        );
    }

    ngOnDestroy(): void {
        this.subscription.unsubscribe();  // <- enfin on se désabonne lors de la destruction du composant
    }
}

```

Pour que l'abonnement ne persiste pas après la destruction du composant Angular automatise toutes ces étapes, il n'est donc pas nécéssaire d'écrire ce code.

Ce code sera par contre nécéssaire pour ses propres observables.