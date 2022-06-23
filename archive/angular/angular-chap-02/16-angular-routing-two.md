## Passer des information par l'url

```html
<a
   [routerLink]="['/server', 5, 'edit']"
   [queryParams]="{uid: '1', name: 'roger'}"
   [fragment]="'easy'"
   fragment="loading"
   *ngFor="let server of servers">
    {{ ... }}
</a>
```

`[routerLink] = "['/server', 5, 'edit']"`  => path :  `/server/5/edit`

`[queryParams]="{uid: '1', name: 'roger'}"` => `/server/5/edit?uid=1&name=roger`

`[fragment]="'easy'"`  => `/server/5/edit?uid=1&name=roger#easy`

**ou** puisque l'on passe un string :

`fragment="loading"`  => `/server/5/edit?uid=1&name=roger#loading`



## La même chose dans le code

Dans le template

```html
<button class="btn btn-outline-warning" (click)="onLoadServers(1)">
    Load Servers 1
</button>
```



Dans le composant

```typescript
onLoadServers(id: number) {
    console.log('hello router');
    this.router.navigate(
        ['/server', id, 'edit'],  // <- la route de base /server/1/edit
        {
            queryParams: {  // <- les paramètre avec ?allowEdit=1&userName=roger
                allowEdit: '1',
                userName: 'roger'
            },
            fragment: 'loading'  // le fragment #loading
        }
    );
}
```

=> `/server/1/edit?allowEdit=1&userName=roger#loading`

## Récupérer les queryParams et le fragment

### Dans le snapshot

```typescript
// ...
import { ActivatedRoute } from '@angular/router';  // 1. on import le service ActivatedRoute

@Component({ ... })
export class EditServerComponent implements OnInit {
	// ...
    // 2. on l'injecte dans le 'constructor'
    constructor(private serversService: ServersService, private route: ActivatedRoute) { }

    ngOnInit() {
        console.log(this.route.snapshot.queryParams); // {edit: "1", name: "roger"}
        console.log(this.route.snapshot.fragment);  // loading
        this.server = this.serversService.getServer(1);
        this.serverName = this.server.name;
        this.serverStatus = this.server.status;
    }
    
    // ...
}

```

## Utiliser les routes imbriquer

Dans app.module :

```typescript
const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent },
    { path: 'users/:id/:name', component: UserComponent },
    { path: 'servers', component: ServersComponent, children: [
        { path: ':id', component: ServerComponent },
        { path: ':id/edit', component: EditServerComponent },
    ] },
];
```

De même avec `users`  :

```typescript
const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent, children: [
        { path: ':id/:name', component: UserComponent }
    ] },
    { path: 'servers', component: ServersComponent, children: [
        { path: ':id', component: ServerComponent },
        { path: ':id/edit', component: EditServerComponent },
    ] }
];

```

On utilise `children: [ tableau de routes imbriquées]`  

## Transférer les queryParams d'une route à l'autre

Si on veut conserver les queryParams d'une route à une autre, on peut utiliser l'option `queryParamsHandling` de navigate :

```typescript
onEdit() {
    this.router.navigate(['edit'], {relativeTo: this.route, queryParamsHandling: 'preserve'});
}
```

## Redirection

**! les routes sont résolues de haut en bas**



```typescript
const appRoutes: Routes = [
    // ... ,
    { path: 'not-found', component: PageNotFoundComponent },
    { path: '**', redirectTo: 'not-found' },
];
```

`**`  joker pour signifier n'importe quel chemin.

```typescript
{ path: '', redirectTo: '/somewhere-else', pathMatch: 'full' } 
```

`pathMatch: 'full'`  permet de ne cibler que le chemin `/`  et pas tous les chemins commençants par `/` .

## Créer un module de Routing

app-routing.module.ts :

```typescript
import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { PageNotFoundComponent } from './page-not-found/page-not-found.component';
import { EditServerComponent } from './servers/edit-server/edit-server.component';
import { ServerComponent } from './servers/server/server.component';
import { ServersComponent } from './servers/servers.component';
import { UserComponent } from './users/user/user.component';
import { UsersComponent } from './users/users.component';
import { HomeComponent } from './home/home.component';

const appRoutes: Routes = [
    { path: '', component: HomeComponent },
    { path: 'users', component: UsersComponent, children: [
        { path: ':id/:name', component: UserComponent }
    ] },
    { path: 'servers', component: ServersComponent, children: [
        { path: ':id', component: ServerComponent },
        { path: ':id/edit', component: EditServerComponent },
    ] },
    { path: 'not-found', component: PageNotFoundComponent },
    { path: '**', redirectTo: 'not-found' },
];


@NgModule({
    imports: [
        RouterModule.forRoot(appRoutes)
    ],
    exports: [
        RouterModule
    ]
})
export class AppRoutingModule {

}

```

Ne pas oublier de l'importer dans app.module.ts :

```typescript
// ...
import { AppRoutingModule } from './app-routing.module'; // <- ici

import { ... } from '...';


@NgModule({
    declarations: [ // ... ],
    imports: [
        BrowserModule,
        FormsModule,
        AppRoutingModule  // <- ici
    ],
    providers: [],
    bootstrap: [AppComponent]
})
export class AppModule { }

```

