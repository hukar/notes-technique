# 04 Angular Material Form

## Formulaire

On utilise `mat-form-field` pour *wrapper* des élément input ou textarea.

Sur les éléments eux-même on utilise la directive `matInput` :

```html
<form>
    <mat-form-field>
        <input type="email" matInput placeholder="Your email">
    </mat-form-field>
    <mat-form-field>
        <input type="password" matInput placeholder="Your password">
    </mat-form-field>
  </form>
```

## Validation de formulaire

validator :

1. `email` : vérifie si c'est une adresse mail valide 

   a. `a@a`  = valide 

   b. `a@a.`  = invalide 

   c. `a@a.a`  = valide

2. `required`

3. `minlength="6"` : nombre de caractère minimum

#### afficher un indice

`<mat-hint>` compris dans le module `mat-form-field`

```html
<mat-form-field>
    <input 
           type="password" 
           matInput 
           placeholder="Your password" 
           ngModel 
           name="password"
           required
           minlength="6">
    <mat-hint>Should be at least six character long.</mat-hint>
</mat-form-field>
```

Cela affiche un indice en bas de l'input.

autre syntaxe comme directive de `mat-hint` :

```html
<mat-form-field hintLabel="Should be not empty.">
    <input 
           type="email" 
           matInput 
           placeholder="Your email" 
           ngModel 
           name="email"
           email
           required>
    <!-- <mat-hint>Should be not empty.</mat-hint> -->
</mat-form-field>
```

On peut combiner les deux pour un affichage dynamique du nombre de caractères tapés :

```html
<mat-form-field hintLabel="Should be at least six character long.">
    <input 
           type="password" 
           matInput 
           placeholder="Your password" 
           ngModel 
           name="password"
           required
           minlength="6"
           #ip>
    <mat-hint align="end">{{ip.value?.length}} / 6</mat-hint>
</mat-form-field>
```

`mat-hint` a la propriété `align` `start`, `end`

#### afficher un message d'erreur

`mat-error`  fournit par `mat-form-field`

On peut afficher plusieurs message d'erreur suivant ses propres conditions :

```html
<mat-form-field>
    <input 
           type="email" 
           matInput 
           placeholder="Your email" 
           ngModel 
           name="email"
           email
           required
           #ie="ngModel">
    <mat-error *ngIf="!ie.value?.length">email is required</mat-error>
    <mat-error *ngIf="!ie.valid">email is invalid</mat-error>
</mat-form-field>
```

`#ie="ngModel"` référence sur un objet angular *wrappant* l'input grace à ngModel

`*ngIf="!ie.value?.length"` sans le point d'interrogation on a une erreur :

```bash
Cannot read property 'length' of null
```

ou mieux encore avec la méthode `hasError()`  :

```html
<mat-error *ngIf="ie.hasError('required')">email is required</mat-error>
<mat-error *ngIf="!ie.hasError('required')">email is invalid</mat-error>
```

On peut faire la même chose  avec les autres validator :

```html
<mat-error *ngIf="ip.hasError('minlength')">password is too short</mat-error>
```

## Rendre le bouton 'submit' désactivé

```html
<button type="submit"
            mat-raised-button
            color="primary"
            [disabled]="f.invalid">Submit</button>
```

En *bindant* la propriété `disabled` sur la validité du NgForm `f.invalid` 

`f` étant une référence `#f="ngModel"`