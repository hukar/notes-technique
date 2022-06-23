#ngModule

bootstrap : `main.ts`

```typescript
...
import { AppModule } from './app/app.module';
...

platformBrowserDynamic().bootstrapModule(AppModule)
  .catch(err => console.log(err));
```

redirige vers `app.module.ts` comme bootstrap de l'application

```typescript
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';

@NgModule({
  declarations: [
    AppComponent  // les directives ajoutées
  ],
  imports: [
    BrowserModule  // les modules disponnibles
  ],
  providers: [],
  bootstrap: [AppComponent]  // <- bootstrap est ici
})
export class AppModule { }
```

@ngModule est un decorator, il s'applique sur AppModule et prends un objet de configuration.