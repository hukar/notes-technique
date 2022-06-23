# 22 Angular HTTP

## Utilisation de Firebase

Bien sélectionner la Realtime Database

Mettre les règles à true :

```json
{
  "rules": {
    ".read": true,
    ".write": true
  }
}
```

## Configuration

Dans `app.module.ts`

```typescript
import { HttpModule } from '@angular/http';
// ...

imports: [
	// ...,
    HttpModule
  ]
```

Dans un service utilisant http :

```typescript
import { Injectable } from '@angular/core';
import { Http } from '@angular/http';  // import

@Injectable({providedIn: 'root'})
export class ServerService {

    constructor(private http: Http) {}  // injection
}
```



## Envoie de données POST

Pour envoyer des données on utilise `http.post(url, data)`

```typescript

    storeServers(servers: any[]) {
        // renvoie un observable
        return this.http.post('https://hukar-udemy-angular-http.firebaseio.com/titi.json', servers);
    }
```

On ajoute `titi.json` à la fin de l'url fournie par firebase pour spécifier l'objet réceptacle (= titi) dans firebase ainsi que le type de donnée (= json).

À cette étape la requête n'est pas envoyer, on récupère juste un `Observable`

Dans le composant on peut utiliser le service :

`app.component.ts`

```typescript
import { Component } from '@angular/core';
import { ServerService } from './server.service';

@Component({...})
export class AppComponent {
  servers = [...];

  constructor(private serverService: ServerService) {}

  onAddServer(name: string) {
    this.servers.push({
      name: name,
      capacity: 50,
      id: this.generateId()
    });
  }
  private generateId() {
    return Math.round(Math.random() * 10000);
  }

  onSave() {
    this.serverService.storeServers(this.servers)
      .subscribe(
        (response) => console.log(response),
        (err) => console.log(err)
      );
  }
}
```

Pour lancer la requête, il faut utiliser subscribe sur l'`Observable`

On lui passe deux fonction anonymes en cas de réussite (1re F°) ou d'échec (2eme)

## Ajout d'un header

On peut envoyer aussi les informations du header :

`myService.service.ts`

```typescript
import { Injectable } from '@angular/core';
import { Http, Headers } from '@angular/http';


@Injectable({providedIn: 'root'})
export class ServerService {

    constructor(private http: Http) {}

    storeServers(servers: any[]) {
        const headers = new Headers({'Data': 'kiki'});  // on crée un header
        return this.http.post('https://hukar-udemy-angular-http.firebaseio.com/titi.json',
                servers,
                {
                    headers: headers  // on le passe dans les options de la requête
                });
    }
}

```

Le constructeur `Header` prend un objet avec les clés/valeurs du header.

`http.post` prend en troisième argument un objet en option dont `headers` représente le header custom.

## Récuperer des données GET

On utilise http.get :

`myService.service.ts`

```typescript
getServers() {
    const url = 'https://hukar-udemy-angular-http.firebaseio.com/titi.json';
    return this.http.get(url);
}
```

Dans le composant :

`app.component.ts`

```typescript
import { Response } from '@angular/http';
// ...
onGet() {
    this.serverService.getServers()
    .subscribe(
      (response: Response) => {
        const data = response.json();
        console.log(data);
      },
      (err) => console.log(err)
    );
  }
```

`response` reçoit un string (dans `_body`) renvoyé par le serveur.

On utilise la méthode `response.json()` pour le transformer en `json`

## Modifier une donnée PUT

identique à POST :

```typescript
updateServers(servers: any[]) {
    const url = 'https://hukar-udemy-angular-http.firebaseio.com/titi.json';
    return this.http.put(url, servers, {headers: headers});
}
```

## Utilisation des operator de RxJs

L'idée est de traiter la réponse une seule fois dans le service pour qu'elle soit utilisée dans tous les composants avec le bon formatage.

import :

```typescript
import { map } from 'rxjs/operators';
```

Méthode utilisant le `pipe` :

```typescript
 getServers() {
        const url = 'https://hukar-udemy-angular-http.firebaseio.com/titi.json';
        return this.http.get(url).pipe(
            map((response: Response) => {
                const data = response.json();
                for (const server of data) {
                    server.name = 'FETCHED_' + server.name;
                }
                return data;
            })
            );
    }
```

Grâce à map, on peut traiter les données reçue de manière centralisée dans le service.



## Gérer les erreurs

Les imports:

```typescript
import { map, catchError } from 'rxjs/operators';
import { throwError } from 'rxjs';
```

le code:

```typescript
getServers() {
    const url = 'https://hukar-udemy-angular-http.firebaseio.com/titi';
    return this.http.get(url).pipe(
        map((response: Response) => {...}),
        catchError(
            error => {
                return throwError('something wrong ... ' + String.fromCodePoint(0x1f600));
            }
        )
    );
}
```

operators `catchError`

`String.fromCodePoint(0x1F600)` affiche un emoji :

lien vers les emoji :

https://fr.wikipedia.org/wiki/Table_des_caract%C3%A8res_Unicode/U1F600

## Pipe async dans le template

ON n'est pas obligé d'utiliser subscribe dans le composant, mais à la place un pipe avec async dans le template.

Dans le service :

```typescript
getAppName() {
    const url = 'https://hukar-udemy-angular-http.firebaseio.com/appName.json';
    return this.http.get(url).pipe(
        map(d => d.json())
    );
}
```

Dans le composant :

```typescript
export class AppComponent {

  appName = this.serverService.getAppName();
}
```

dans le template:

```html
<h1>{{appName | async}}</h1>
```

`async` souscrit dans le template à l'observable récupéré par `appName` dans le composant.



## Création d'un service pour gérer les datas

On crée un service spécialisé dans la communication http.

data-storage.service.ts :

```js
import { Injectable } from '@angular/core';
import { Http } from '@angular/http';
import { RecipeService } from '../recipes/recipe.service';

@Injectable({providedIn: 'root'})
export class DataStorageService {
    constructor(private http: Http, private recipeService: RecipeService) {}

    storeRecipes() {
        const url = 'https://ng-recipe-book-afe23.firebaseio.com/recipes.json';
        const recipes = this.recipeService.getRecipes();
        return this.http.put(url, recipes);
    }

    fetchRecipes() {
        const url = 'https://ng-recipe-book-afe23.firebaseio.com/recipes.json';
        this.http.get(url)
            .subscribe({
                next: (response: Response) => {
                    const recipes: Recipe[] = response.json();
                    this.recipeService.setRecipes(recipes);
                },
                error: (err) => console.log(err)
            });
    }
}
```

`@Injectable` pour pouvoir utiliser le service `recipe.service` et `http`

architecture en trois étape : 

1. un service pour gérer les données : `recipe.service`
2. un service pour gérer la connection avec  les données save et fetch : `data-storage.service`
3. consommation du service dans le composant qui a la responsabilité du `subscribe` (qui lance la requête AJAX)

La méthode `json()` permet d'extraire du `json` de la réponse.

Dans `recipe.service` :

```js
setRecipes(recipes) {
    this.recipes = recipes;  // 1.
    this.recipesChanged.next(this.recipes.slice());  // 2.
}
```

1. on change la valeur de `this.recipes`
2. on envoie une copie de `this.recipes` avec `slice()` aux abonnés de `this.recipesChanged`

Dans le `recipe.service` :

```js
recipesChanged = new Subject<Recipe[]>();
```

Dans un composant :

```js
@Component({...})
export class RecipeListComponent implements OnInit, OnDestroy {
  recipes: Recipe[];
  subscription: Subscription;

  constructor(private recipeService: RecipeService) {}

  ngOnInit() {
    this.subscription = this.recipeService.recipesChanged
      .subscribe(
        (recipes: Recipe[]) => {
          this.recipes = recipes;
        }
      );
    this.recipes = this.recipeService.getRecipes();
  }

  ngOnDestroy() {
    this.subscription.unsubscribe();
  }
}
```

