# 01 component

<img src="assets/component-schema.png" alt="component-schema" style="zoom:50%;" />

## Simple Component

<img src="assets/component-skeleton-101.png" alt="component-skeleton-101" style="zoom:50%;" />

## Décorateur

Une fonction qui ajoute des `metadata` à une classe ses membres ou ses méthodes.

Préfixé par `@`.



## Modules `Angular`

<img src="assets/angular-modules.png" alt="angular-modules" style="zoom:50%;" />



## Component Base Skeleton

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'pm-root',
  template: `
    <div>
      <h1>{{ pageTitle }}</h1>
    </div>
  `,
})
export class AppComponent {
  pageTitle: string = 'Hukar page for ever 🐵';
}
```



## Debug dans le navigateur

`Webpack` crée des fichiers `map` qui permettent de débuguer `Typescript` directement dans le navigateur :

<img src="assets/dev-tools.png" alt="dev-tools" style="zoom:50%;" />

On peut ajouter un `break point` :

<img src="assets/debug-break-point.png" alt="debug-break-point" style="zoom:50%;" />

## Compilation

Par le passé `Angular` utilisait une compilation `Just In Time` pour le développement et une compilation `Ahead of Time Compiler` (`AOT`) pour la production.

Depuis la `v9` la compilation `AOT` est suffisamment rapide pour être aussi utilisé pour le développement.

