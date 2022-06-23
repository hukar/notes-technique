#Route Guard

On veut exécuter du code avant le chargement d'un composant

## Protéger une route

On va créer un service qui implémente l'interface `CanActivate`

`auth-guard.service.ts` :

```typescript
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';
import { AuthService } from './auth.service';


@Injectable()
export class AuthGard implements CanActivate {

    constructor(private authService: AuthService, private router: Router) {}

    canActivate(route: ActivatedRouteSnapshot,
                state: RouterStateSnapshot)
    		   : Observable<boolean> | Promise<boolean> | boolean {
        return this.authService.isAuthenticated().then(
            (authenticated: boolean) => {
                if (authenticated) {
                    return true;
                } else {
                    this.router.navigate(['/']);
                }
            }
        );
    }
}
```

utilistation asynchrone => `Observable<boolean>` et `Promise<boolean>`

utilisation Synchrone => juste un `boolean` 

rappel : `@Injectable()`  permet d'injecter des services dans le `constructor` 

On retourne la `promise` , si authenticated est true on renvoie true sinon grace au `router`,

on *'navigue'* vers `/`

`auth.service.ts` :

```typescript
export class AuthService {
    loggedIn = false;

    isAuthenticated() {
        const promise = new Promise(
            (resolve, reject) => {
                setTimeout(
                    () => {
                        resolve(this.loggedIn);
                    },
                    800
                );
            }
        );

        return promise;
    }

    login() {
        this.loggedIn = true;
    }

    logout() {
        this.loggedIn = false;
    }
}
```

*cf Promise javascript*

Sevice simulant une demande d'authorisation asynchrone avec une `promise` et un `setTimeout` 

### On doit dire maintenant quelle(s) route(s) sont protégées par AuthGuard

dans `app-routing.module.ts`  :

```typescript
const appRoutes: Routes = [
    // ...
    { 
         path: 'servers', 
         canActivate: [AuthGard], // <- ici
         component: ServersComponent, 
         children: [
            { path: ':id', component: ServerComponent },
            { path: ':id/edit', component: EditServerComponent },
        ] 
    },
    // ...
];
```

On ajoute `canActivate: [AuthGuard]` , un tableau de tous les *'guardiens'* a exécuter sur cette route et ses enfants.

### Et il faut injecter les nouveaux services dans le module

`app.module.ts`  :

```typescript
//...
import { AuthService } from './auth.service';
import { AuthGard } from './auth-guard.service';

@NgModule({
    declarations: [ ... ],
    imports: [ ... ],
    providers: [
        ServersService,
        AuthService,  // <- ici
        AuthGard  // <- ici
    ],
    bootstrap: [AppComponent]
})
export class AppModule { }

```

## Protéger les enfants d'une route

Au lieu d'utiliser `CanActivate` on utilise plutôt `CanActivateChild` :

`auth-gard.service.ts` :

```typescript
export class AuthGard implements CanActivate, CanActivateChild {

    constructor(private authService: AuthService, private router: Router) {}

    canActivate(route: ActivatedRouteSnapshot,
        state: RouterStateSnapshot): Observable<boolean> | Promise<boolean> | boolean {
            return this.authService.isAuthenticated().then(
                (authenticated: boolean) => {
                    if (authenticated) {
                        return true;
                    } else {
                        this.router.navigate(['/']);
                    }
                }
            );
        }

    canActivateChild(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean | Observable<boolean> | Promise<boolean> {
        return this.canActivate(route, state);  // utilise la méthode canActivate
    }
}
```

On implémente la méthode qui utilise en fait la méthode canActivate de base.

Dans `app-routing.module.ts` :

```typescript
const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent, children: [
        { path: ':id/:name', component: UserComponent }
    ] },
    {   path: 'servers',
        // canActivate: [AuthGard],
        canActivateChild: [AuthGard],  // <- bloque les routes enfants
        component: ServersComponent,
        children: [
        { path: ':id', component: ServerComponent },
        { path: ':id/edit', component: EditServerComponent },
    ] },
    { path: 'not-found', component: PageNotFoundComponent },
    { path: '**', redirectTo: 'not-found' },
];
```

## Empécher de quitter une page

Parfois on ne veut pas quitter la page sans avoir fini une tâche.

On utilise CanDeactivate pour ça.

####1. On crée un service dans le répertoire du composent dont on veut prévenir la sortie accidentelle.

`can-deactivate-guard.service.ts` :

```typescript
import { Observable } from 'rxjs';
import { CanDeactivate, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';

export interface CanComponentDeactivate {
    canDeactivate: () => Observable<boolean> | Promise<boolean> | boolean;
}

export class CanDeactivateGuard implements CanDeactivate<CanDeactivateGuard> {
    canDeactivate(
        component: CanComponentDeactivate,
        currentRoute: ActivatedRouteSnapshot,
        currentState: RouterStateSnapshot,
        nextState?: RouterStateSnapshot
    ): Observable<boolean> | Promise<boolean> | boolean {
        return component.canDeactivate();
    }
}
```

On crée un contrat `canComponentDeactivate` , le service force le composant à implémenter `canDeactivate()`

####2. Ensuite on enregistre notre service dans `app.module.ts` :

```typescript
providers: [
        ServersService,
        AuthService,
        AuthGard,
        CanDeactivateGuard  // <- ici
    ],
```

#### 3.  On ajoute le paramètre canDeactivate dans app-routing

`app-routing.module.ts` :

```typescript
const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent, children: [
        { path: ':id/:name', component: UserComponent }
    ] },
    {   path: 'servers',
        // canActivate: [AuthGard],
        canActivateChild: [AuthGard],
        component: ServersComponent,
        children: [
        { path: ':id', component: ServerComponent },
        {
            path: ':id/edit',
            component: EditServerComponent,
            canDeactivate: [CanDeactivateGuard]  // <- ici
        },
    ] },
    { path: 'not-found', component: PageNotFoundComponent },
    { path: '**', redirectTo: 'not-found' },
];
```

On donne un tableau de services associés

#### 4. On implémente canDeactivate dans le composant

```typescript
@Component({ ... })
            
export class EditServerComponent implements OnInit, CanComponentDeactivate {

    server: {id: number, name: string, status: string};
    serverName = '';
    serverStatus = '';
    queryParams: Params;
    fragment: string;
    allowEdit = false;
    changesSaved = false;  // <- attribut pour tester la redirection

    constructor(private serversService: ServersService,
                private route: ActivatedRoute,
                private router: Router) { }

    ngOnInit() {
        this.queryParams = this.route.snapshot.queryParams;
        this.fragment = this.route.snapshot.fragment;

        this.route.queryParams.subscribe(
            (queryParams: Params) => {
                console.log(queryParams);

                this.allowEdit = queryParams['allowEdit'] === '1' ? true : false;
            }
        );

        this.route.params.subscribe(
            (params: Params) => {
                this.server = this.serversService.getServer(+params['id']);
            }
        );

        this.route.fragment.subscribe();
        const id = +this.route.snapshot.params['id'];
        console.log('params', this.route.snapshot.params);

        console.log('id : ', id);

        this.server = this.serversService.getServer(id);
        this.serverName = this.server.name;
        this.serverStatus = this.server.status;
    }

    onUpdateServer() {
        this.serversService.updateServer(this.server.id, {name: this.serverName, status: this.serverStatus});
        this.changesSaved = true;
        this.router.navigate(['../'], {relativeTo: this.route});
    }
	
// la logique de redirection est placée dans cette méthode
    canDeactivate(): Observable<boolean> | Promise<boolean> | boolean {
        if (!this.allowEdit) {
            return true;  // navigation ok!
        }
        if ((this.serverName !== this.server.name || this.serverStatus !== this.server.status) && !this.changesSaved) {
            // sinon on utilise une boite de confirmation
            return confirm('Do you want to discard the changes ?');
        } else {
            return true;
        }
    }
}
```

