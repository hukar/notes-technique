## Passer des data statique à une route

Dans `app-routing.module.ts` :

```typescript
{ 
    path: 'not-found', 
    component: ErrorPageComponent, 
    data: {
        	message: 'Page not found!'
    	  } 
},
```

On passe des data avec la propriété `data`



dans le composant :

```typescript
export class ErrorPageComponent implements OnInit {

  errorMessage: string;

  constructor(private route: ActivatedRoute) { }

  ngOnInit() {
    console.log(this.route);
    this.errorMessage = this.route.snapshot.data['message'];

    this.route.data.subscribe(
      (data: Data) => this.errorMessage = data['message']
    );
  }

}
```

On récupère le service `ActivatedRoute` dans le constructor.

Dans `ngOnInit`  on recupère le data avec `this.route.snapshot.data['dataname']`

et on peut s'abonner aux changements de cette valeur.



## passer des data dynamiques à une route

Pour précharger des données on va créer un resolver

server-resolver.service.ts :

```typescript
import { Resolve, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable } from 'rxjs';

import { ServersService } from '../servers.service';
import { Injectable } from '@angular/core';

interface Server {
    id: number;
    name: string;
    status: string;
}

@Injectable()
export class ServerResolver implements Resolve<Server> {

    constructor(private serversService: ServersService) {}

    resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot):
    Observable<Server> | Promise<Server> | Server {
        console.log('server-resolver');

        return this.serversService.getServer(+route.params['id']);
    }
}
```

**l'interface** permet d'avoir un type pour le retour de resolve

`@Injectable()` permet d'utiliser le service serversService

On récupère un server dans server-resolver et on le renvoie.

Dans `app.module.ts` on enregistre le service :

```typescript
  providers: [
        // ... ,
        ServerResolver
    ],
```

Dans `app-routing.module.ts` :

```typescript
{
        path: 'servers',
        // canActivate: [AuthGard],
        canActivateChild: [AuthGard],
        component: ServersComponent,
        children: [
            {
                path: ':id',
                component: ServerComponent,
                resolve: {
                    server: ServerResolver
                }
            },
            {
                path: ':id/edit',
                component: EditServerComponent,
                canDeactivate: [CanDeactivateGuard]
            },
        ]
    },
```

la propriété resolve prend un objet comme valeur, server prend la valeur de retour du ServerResolver.

Enfin dans le composant, on simplifie la logique d'obtention des données puisque c'est le resolver qui va s'en occuper :

```typescript
ngOnInit() {
        // const id = +this.route.snapshot.params['id'];
        // this.server = this.serversService.getServer(id);
        // this.route.params.subscribe(
        //     (params: Params) => {
        //         this.server = this.serversService.getServer(+params['id']);
        //     }
        // );

        this.route.data.subscribe(
            (data: Data) => this.server = data['server']
        );
    }
```

l'observable n'est plus sur les paramètres de la route mais sur les data.



## Hash mode pour les routes

Si les routes Angular génère des erreurs du serveur, il est possible d'ajouter un hashtag dans la route.

app-routing;module.ts :

```typescript
@NgModule({
    imports: [
        RouterModule.forRoot(appRoutes, {useHash: true})
        // RouterModule.forRoot(appRoutes)
    ],
    exports: [
        RouterModule
    ]
})
```

Voici à quoi ressemble du coup une URL :

```xml
http://localhost:4200/#/servers/2/edit?allowEdit=0
```

