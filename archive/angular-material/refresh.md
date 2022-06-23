# Refresh

```typescript
@NgModule({
  declarations: [
    AppComponent,
    AddedComponent
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

`bootstrap: [AppComponent]` signifie que depuis `index.html` c'est l'entrée pour les composants angular.

```html
<!-- index.html -->
<!doctype html>
<html lang="en">
<head>
  ...
</head>
<body>
  <app-root></app-root>
  <app-added></app-added>
</body>
</html>
```

dans ce cas ```<app-added>``` n'est pas reconnu par angular comme son composant.

## Template

```html
<input type="text" [(ngModel)]="productName">

<button (click)="productName='a pork'" [disabled]="isDisabled">change name</button>

{{ productName }}
```

`{{ myName }}` : interpolation texte

`(click)` : event binding

`[disabled]` : property binding

`[(ngModel)]="myName"` : two way binding

## Structural directive *

```html
<input *ngIf="!isDisabled" type="text" [(ngModel)]="productName">

<button (click)="onAddProduct()" [disabled]="isDisabled">change name</button>

<div *ngFor="let product of products, let i = index">
    {{ i + 1 }} -> {{ product }}
</div>
```

`*ngIf="condition"`

`*ngFor="let something of lotOfThings;let i = index"`

## @Input @Output

La communication entre composant ce fait grâce aux décorateurs `@Input` et `@Output`:

product component :

```typescript
export class ProductComponent implements OnInit {

  @Input() productName: string;
  @Output() productClicked = new EventEmitter;

  onProductClicked() {
    this.productClicked.emit();
  }
}
```

```html
<article (click)="onProductClicked()" class="product">
  ...
</article>
```

products component :

```typescript
export class ProductsComponent {
    products = ['tulip', 'rat'];

    onProductRemove(product) {
        this.products = this.products.filter(p => p !== product);
    }
}
```

```html
<app-product 
             (productClicked)="onProductRemove(product)" 
             *ngFor="let product of products" 
             [productName]="product"></app-product>
```

## Angular Form

Import :

`app.module`

```typescript
import { FormsModule } from '@angular/forms'
...
@NgModule({
    import: [
    FormsModule,
    ...
	],
    ...
})
```

Template :

```html
<form (ngSubmit)="onAddProduct(f)" #f="ngForm">
    <input type="text" ngModel name="productName" required>
    <button type="submit">change name</button>
</form>
```

`#f`  local reference

`ngForm`  Formulaire Angular (qui encapsule le formulaire du DOM)

`ngModel`  Relie l'élément de formulaire à `ngForm`

`required` détermine la valeur de `valid`

```typescript
onAddProduct(form) {
    if (form.valid) {
        this.products.push(form.value.productName);
    }
}
```



## Services

Les services sont de simple classe Typescript :

```typescript
export class ProductsService {
    private products = ['a hook'];

    addProduct(product) {
        this.products.push(product);
    }

    getProducts() {
        // return a copy not a ref to the original array
        return [...this.products];
    }

    deleteProduct(product) {
        this.products = this.products.filter(p => p !== product);
    }
}
```

On doit les injecter dans le module :

```typescript
import { ProductsService } from './products.service';

@NgModule({
 ...,
  providers: [ProductsService],
  ...
})
```

Puis dans le composant on l'injecte dans le constructeur :

```typescript
import { ProductsService } from '../products.service';  // <- ici import

@Component({...})
export class ProductComponent {

  constructor(private productsService: ProductsService) { }  // <- ici injection

  onProductClicked() {
    this.productsService.deleteProduct(this.productName);  // <- ici utilisation
  }
}
```

Pour prévenir un composant du changement des données on utilise `Subject` de `rxjs`

####Dans le service :

```typescript
import { Subject } from 'rxjs';  // <- 1

export class ProductsService {
    private products = ['a hook'];
    productsUpdated = new Subject();  // <- 2

    addProduct(product) {
        this.products.push(product);
        this.productsUpdated.next();  // <- 3
        // equal to emit
        // this is possible to pass data like that: next(this.products)
    }

    getProducts() {
        // return a copy not a ref to the original array
        return [...this.products];
    }

    deleteProduct(product) {
        this.products = this.products.filter(p => p !== product);
        this.productsUpdated.next();  // <- 3
    }
}
```

1. On importe Subject de `rxjs`
2. On crée un `Subject`
3. On notifie le changement en utilisant la méthode `next()`



#### Dans le composant :

```typescript
import { Subscription } from 'rxjs'  // on importe Subscription;

@Component({...})
export class ProductsComponent implements OnInit, OnDestroy {

    private productsSubscription: Subscription;  // on déclare une subscription

    constructor(private productsService: ProductsService) { }

    ngOnInit(): void {
		// on utilise la méthode subscribe de productsUpdated (Subject)
    // on passe une fonction qui sera éxecuté à chaque
        this.productsSubscription = this.productsService.productsUpdated.subscribe(() => {
            this.products = this.productsService.getProducts();
        });
    }

    ngOnDestroy(): void {
        this.productsSubscription.unsubscribe();
    }

```

Que se passe-t-il lorsqu'on écrit :

```typescript
this.productsSubscription = this.productsService.productsUpdated.subscribe(() => {
    this.products = this.productsService.getProducts();
});
```

en fait on transpile vers quelque chose comme :

```typescript
 var _this = this;
this.productsSubscription = this.productsService.productsUpdated.subscribe(() => {
    _this.products = _this.productsService.getProducts();
});
```

C'est comme ça que le Subject a la référence vers l'objet (il a l'objet  `_this`)

## Routage

### 1. Créer un module route

On crée un fichier `app-routing.module.ts` :

```typescript
import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';


// ici les composant routés
import { HomeComponent } from './home.component';
import { ProductsComponent } from './products.component';

// configuration des routes
const routes: Routes = [
    {path: '', component: HomeComponent},
    {path: 'products', component: ProductsComponent}
];

@NgModule({
    imports: [
        RouterModule.forRoot(routes)  // <- ici on passe nos routes au RouterModule
    ],
    exports: [RouterModule]
})
export class AppRoutingModule {}
```

### 2. On le référence dans `app.module.ts`

```typescript
import { AppRoutingModule } from './app-routing.module';

@NgModule({
  declarations: [...],
  imports: [
    ...,
    AppRoutingModule  // <- ici
  ],
  ...
})
```

### 3. Dans les composants

Plus besoin de renseigner l'attribut `selector` :

```typescript
import { Component } from '@angular/core';

@Component({
    template: '<h1>Home Component</h1>'
})
export class HomeComponent {}
```

### 4. Dans les templates

```html
<h1>My beautiful </h1>
<nav>
  <a routerLink="">Home</a>&nbsp;&nbsp;|&nbsp;&nbsp;
  <a routerLink="/products">Products</a>
</nav>

<router-outlet></router-outlet>
```

`routerLink`  relie aux routes définies dans angular sans générer un rechargement de la page contrairement avec un `href`.

`<router-outlet>` est la sortie du routage (là ou le composant lié à la route sera monté).

## Récupérer une référence du template dans le composant

Dans le template utilisation de `#`

```html
<form #f>
    ...
</form>
```

Dans le composant `thingy.component`

```typescript
import { ViewChild } from '@angular/core'

@ViewChild('f') myForm: HTMLFormElement;
```

## Utiliser le référence d'un élément de formulaire

```html
<mat-checkbox 
              labelPosition="before"
              ngModel
              name="conditions"
              required
              color="primary"
              #c="ngModel"
>
    Accept terms and consitions
</mat-checkbox>
<div 
     style="background-color: red;color: white;border-radius: 2px;box-shadow: -5px 7px 39px -5px rgba(125,125,125,1);padding:12px;"
     *ngIf="c.invalid && c.touched">
    Please response to the answer
</div>
```

`#c="ngModel"`  pour prendre la référence sur l'élément

`*ngIf="c.invalid && c.touched"`  pour effectuer un test sur cet élément qui a les même propriétés que `ngForm` (pristine, dirty, valid, invalid ... )