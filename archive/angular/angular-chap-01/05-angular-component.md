## template html en ligne ou dans un fichier

###en ligne

dans `pipos.component.ts`

```typescript
import { Component, OnInit } from '@angular/core';

# decorator auquel on passe un objet de config
@Component({
  selector: 'app-pipos',
  template: `
  <app-pipo></app-pipo>
  
  <app-pipo></app-pipo>
  `,
  styleUrls: ['./pipos.component.css']
})
export class PiposComponent implements OnInit {

  constructor() { }

  ngOnInit() {
  }

}
```

Les guillemets penchés permettent de sauter des lignes

###avec un fichier externe

Il existe un fichier `pipo.componet.html`

```typescript
@Component({
  selector: 'app-pipos',
  templateUrl: './pipo.component.html',
  styleUrls: ['./pipos.component.css']
})
```

### De même css en ligne

Toujours dans `pipos.component.ts`

```typescript
@Component({
  selector: 'app-pipos',
  templateUrl: './pipos.component.html',
  //styleUrls: ['./pipos.component.css']
  styles: [`
    h3{
      color:#1599bc;
    }
  `]
})
```

### attribut selector

On peut aussi vouloir transformer le selecteur en **attribut** plutôt qu'en nom de balise :

dans `pipos.component.ts`

```typescript
@Component({
  //selector: 'app-pipos',
  selector: '[app-pipos]',
  templateUrl: './pipos.component.html',
  //styleUrls: ['./pipos.component.css']
  styles: [`
    h3{
      color:#1599bc;
    }
  `]
})
```

et maintenant dans le template apellant `app.component.html`

```html
<!-- <app-pipos></app-pipos> -->
<div app-pipos></div> 
```

Ou encore en **classe** :

```typescript
@Component({
  //selector: 'app-pipos',
  //selector:'[app-pipos]',
  selector:'.app-pipos',
  templateUrl: './pipos.component.html',
  //styleUrls: ['./pipos.component.css']
  styles: [`
    h3{
      color:#1599bc;
    }
  `]
})
```

et dans le template :

```html
<!-- <app-pipos></app-pipos> -->
<!-- <div app-pipos></div> -->
<div class="app-pipos"></div> 
```

### Créer un component

- Créer un dossier dans `src/app/my-stuck`

- Dans ce dossier, créer quatre fichier :

  1. my-stuck.component.ts
  2. my-stuck.component.thtml
  3. my-stuck.component.css
  4. my-stuck.component.spec.ts => pour les tests

- Dans `my-stuck.component.ts`:

  ```typescript
  // 1. importer Component
  import { Component } from "@angular/core";
  
  // 2. Configurer son composant gace au decorator
  @Component({
      selector: 'app-my-stuck', // balise, classe .nom ou attribut [nom]
      templateUrl: './my-stuck.component.html',
      styleUrls: ['./my-stuck.component.css']
  })
  
  //3. l'exporter
  export class MyStuckComponent {
      
  }
  ```

- Dans le fichier `src\app\app.module.ts`

  ```typescript
  import { BrowserModule } from '@angular/platform-browser';
  import { NgModule } from '@angular/core';
  
  import { AppComponent } from './app.component';
  # on importe le composant pour Typescript
  import { MyComponent } from "./my-stuck/my-stuck.component";
  
  
  @NgModule({
    declarations: [
      AppComponent,
      # ici on l'injecte dans l'application Angular
      MyStuckComponent
    ],
    imports: [
      BrowserModule
    ],
    providers: [],
    bootstrap: [AppComponent]
  })
  export class AppModule { }
  ```


# Imbriquer des composant - Nesting Component

dossier: lapin

> lapin.component.ts
>
> lapin.component.html
>
> dossier: lapins
>
> > lapins.component.ts
> >
> > lapin.component.html



Le dossier lapins est dans le dossier lapin, ils ne sont pas au même niveau.

Dans lapins.component.html on peut écrire :

```html
<app-lapin></app-lapin>

<app-lapin></app-lapin>

<app-lapin></app-lapin>
```

En fait quelque soit le niveau des composants d'un module, ils peuvent s'imbriquer les uns dans les autres.

## View encapsulation

Angular génére un attribut différent par composant pour encapsuler les styles :

```html
<p _ngcontent-c1> ... </p>
```

c1 montre que c'est l'élément 2 (on commence à c0)

Les styles du composant `myCard.component.css`  sont appliqués à cet attribut :

```css
p[_ngcontent-c1] {
    color: red;
}
```

ce procédé automatique permet d'encapsuler les styles dans le composant

On peut aussi désactivé ce comportement dans le composant :

```typescript
import { Component, ViewEncapsulation } from '@angular/core';  // <- import de ViewEncapsulation


@Component({
	selector: 'app-server-element',
	templateUrl: 'server-element.component.html',
	styleUrls: ['server-element.component.css'],
	encapsulation: ViewEncapsulation.None  // <- attribut encapsulation
})
```

L'enum ViewEncapsulation a trois valeur :

1. None => les propriété définie dans le css sont global
2. Emulated => par default
3. Native => utilise le shadow DOM

## Référence locale dans le template

`#monElement`  dans une balise :

```html
<input class="form-control" id="serverName" type="text" #serverNameInput>
```

Ailleurs dans le code du template on peut l'utiliser :

```html
<label for="serverName">Server Name : {{serverNameInput.value}}</label>
	...
<button class="btn btn-outline-secondary mr-2"
        (click)="addServer(serverNameInput)">
    Add Server
</button>
```

Envoyer via la fonction du template on récupère l'élément dans le composant typescript :

```typescript
public addServer(inputName: HTMLInputElement) {
    this.serverCreated.emit({
        serverName: inputName.value,
        serverContent: this.contentServer});
}
```

## Récupérer la référence avec ViewChild

On peut aussi utiliser le decorator `@ViewChild('nomDeMaReference')`  :

Dans le HTML la même chose

```html
<input id="serverContent" 
		class="form-control" 
		type="text"
		#serverContentInput> 
```

On crée une référence sur l'élément

Dans le composant :

```typescript
import { Component, EventEmitter, Output, ViewChild, ElementRef } from '@angular/core';
...
@ViewChild('serverContentInput') serverContentInput: ElementRef;
...
serverContent: this.serverContentInput.nativeElement.value
```

La méthode `nativeElement` renvoie l'élément référencé

## Récupérer la référence avec ContentChild

Si l'élément se trouve dans la vue parente, avec l'utilisation d'un ngContent par exemple, on utilise alors le decorator `@ContentChild`

html de l'élément :

```html
<div class="card mb-2">
    ...
    <ng-content></ng-content>
    ...
</div>
```

Dans la vue parente :

```html
<app-server-element *ngFor="let server of servers" 
						[element]="server"
						[name]="server.name">
		<p #contentParagraphe>	
			...
		</p>
	</app-server-element>
```

On récupère la référence avec  l'attribut `#monElement`

Enfin dans l'objet composant :

```typescript
import { Component,
		ElementRef,
		ContentChild} from '@angular/core';

export class ServerElementComponent {
	...
	@ContentChild('contentParagraphe') paragraphe: ElementRef;
	...
}
```

