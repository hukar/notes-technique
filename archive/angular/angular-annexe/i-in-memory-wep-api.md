# I in memory web api

installation via `npm` :

```bash
npm install angular-in-memory-web-api --save
```

Dans `app.module` :

```js
// ...
import { HttpModule } from '@angular/http';
import { HttpInMemoryWebApiModule } from 'angular-in-memory-web-api';

// ...
import { InMemoryService } from './service/in-memory.service';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    // ...,
    HttpModule,
    HttpInMemoryWebApiModule.forRoot(InMemoryService, { delay: 1500 })
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }

```

Pour `Http` on importe `HttpInMemoryWebApiModule`.

La methode `forRoot` prend un service implémentant l'interface `InMemoryDbService`,

elle prend aussi un objet d'option ici `delay` simule un temps de latence avant de recevoir les données.

Dans `in-memory.service` :

```js
import { Injectable } from "@angular/core";

import { InMemoryDbService } from 'angular-in-memory-web-api'; // <- import
import { User } from "../user.model";

@Injectable({providedIn: 'root'})
export class InMemoryService implements InMemoryDbService {
    createDb() {
        const users = [
            {id: 0,name: 'kiki'}
        ];
        return {users};
    }

    genId(users: User[]) {
        return users.length > 0 ? Math.max(...users.map(u => u.id)) + 1 : 0;
    }
}
```

Ici on crée la base de données.

On la contact avec une url de type :

```js
private userUrl = 'api/users/1';

testHttp() {
    const t = this.http.get(this.userUrl)
    .subscribe(
        x => console.log(x),
        err => console.log('Not found')
    );
}
```

