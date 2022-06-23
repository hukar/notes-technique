# 01 Angular Material Introduction

##présentation

Il y a deux partie à Angular Material :

`@angular/cdk` : ensemble de helper et d'utilities : *Component Developement kit*

`@angular/material` : ensemble de composant eux-même basés sur `cdk` et sur la spécification `material design`

## Installation

```bash
npm install --save @angular/material @angular/cdk
```

`@angular/animation` est déjà installé :

`package.json` :

```json
"dependencies": {
    "@angular/animations": "^6.1.0", // <- ici
    "@angular/cdk": "^6.4.7",  // <- voilà le cdk
	// ...
    "@angular/material": "^6.4.7",  // <- voilà les composant material
    // ...
  },
```

### update version 7

On peut faire :

```bash
ng add @angular/material
```

#### Ensuite on crée un module pour gérer les imports de module

```bash
ng g m material
```

Chaque composant Material a son propre module pour que le code de l'application final ne comprenne que les composants utilisés

`material.module.ts`

```typescript
import { NgModule } from '@angular/core';

@NgModule({
    imports: [],
    exports: []
})
export class MaterialModule {}
```

Dans ce module on va gérer les composant/module utilisés.

Enfin dans `app.module.ts`

```typescript
...
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { AppComponent } from './app.component';
import { MaterialModule } from './material/material.module';

@NgModule({
  declarations: [...],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,  // le module pour les animations
    MaterialModule  // <- Notre module
  ],
  ...
})
export class AppModule { }
```

#### Ajouter un thème

dans `src/styles.css`  :

```css
/* You can add global styles to this file, and also import other style files */
@import '~@angular/material/prebuilt-themes/purple-green.css';
```

#### Importer hammerjs

```bash
npm install --save hammerjs
```

Dans `src/main.ts`  :

`hammer.js` sert pour le tactile

```typescript
import 'hammerjs';  // <- ici tout en haut

import { enableProdMode } from '@angular/core';
import { platformBrowser // ...
```

#### utiliser Material icon

Il faut télécharger les fichiers sur le site :

Puis j'ajoute le `css` fournit dans `src/styles.css`  :

```css
@font-face {
    font-family: 'Material Icons';
    font-style: normal;
    font-weight: 400;
    src: url(assets/material-icon/MaterialIcons-Regular.eot); /* For IE6-8 */
    src: local('Material Icons'),
         local('MaterialIcons-Regular'),
         url(assets/material-icon/MaterialIcons-Regular.woff2) format('woff2'),
         url(assets/material-icon/MaterialIcons-Regular.woff) format('woff'),
         url(assets/material-icon/MaterialIcons-Regular.ttf) format('truetype');
  }
  
  .material-icons {
    font-family: 'Material Icons';
    font-weight: normal;
    font-style: normal;
    font-size: 24px;  /* Preferred icon size */
    display: inline-block;
    line-height: 1;
    text-transform: none;
    letter-spacing: normal;
    word-wrap: normal;
    white-space: nowrap;
    direction: ltr;
  
    /* Support for all WebKit browsers. */
    -webkit-font-smoothing: antialiased;
    /* Support for Safari and Chrome. */
    text-rendering: optimizeLegibility;
  
    /* Support for Firefox. */
    -moz-osx-font-smoothing: grayscale;
  
    /* Support for IE. */
    font-feature-settings: 'liga';
  }
```

Syntaxe d'une icone :

```html
<i class="material-icons" style="font-size: 56px;color:deepPink">face</i>
```

C'est grâce à `ligatory`  qu'un mot est transformé en icône == élégant

## Ajout d'un composant

```bash
ng g c auth/signup
```

<span style="color:red;font-size: 20px">error !</span>

```bash
More than one module matches. Use skip-import option to skip importing the component into the closest module.

```

Il faut spécifié le bon module maintenant qu'il y en a deux.

```bash
ng g c auth/signup --module app.module
```

