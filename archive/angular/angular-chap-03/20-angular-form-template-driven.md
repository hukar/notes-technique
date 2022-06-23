# 20 angular form template driven

## Utiliser les formulaire angular

dans app.module.ts :

```typescript
import { FormsModule } from '@angular/forms';

// ...
  imports: [
    FormsModule
  ]
```

## Lire un formulaire

Dans le template :

```html
<form (ngSubmit)="onSubmit(f)" #f="ngForm">
    <label for="username">Username</label>
    <input 
           type="text" 
           id="username" 
           class="form-control"
           ngModel
           name="username"
     >
    <button class="btn btn-primary" type="submit">Submit</button>
</form>
```

`ngModel` est la directive pour donner à Angular accès à l'élément de formulaire

`name="username"` sera utiliser comme clé valeur par l'objet formulaire d'Angular

on utilise l'événement ngSubmit et on passe une référence de l'objet-form d'Angular : `(ngSubmit)="onSubmit(f)" #f="ngForm"`

Dans le composant :

```typescript
  onSubmit(f: NgForm) {
    console.log(f);
  }
```

## ngForm

`dirty` / `pristine`: le formulaire est vierge (pristine) ou rempli (dirty)

`touched` / `untouched`: le formulaire est touché (focus) ou non

`valid` / `invalid`: le champ ou le formulaire est valide ou non

etc.

Ajoute des classes au formulaire ou à ses élément :

```html
<form _ngcontent-c0="" novalidate="" class="ng-valid ng-touched ng-dirty">
    <input _ngcontent-c0="" 
           class="form-control ng-pristine ng-valid ng-touched" 
           id="username" 
           name="username" 
           ngmodel="" 
           type="text" 
      >
</form>
```



ng-touched, ng-valid, ng-pristine ...

## Accéder à ngForm avec ViewChild

```typescript
import { Component, ViewChild } from '@angular/core';
import { NgForm } from '@angular/forms';

@Component({...})
export class AppComponent {

  @ViewChild('f') signupForm: NgForm;

  // onSubmit(f: NgForm) {
  //   console.log(f);
  // }

  onSubmit() {
    console.log('signupForm', this.signupForm);
  }
}
```

## Directive de validation

Angular propose des directives de validation jouant sur la valeur de `valid` pour un `formControl` ou un `ngForm` :

```html
<input 
       type="email" 
       id="email" 
       class="form-control"
       ngModel
       name="email"
       required
       email
>
```

## Gérer la propriété disabled du bouton submit

Si on a accès à f grace à sa référence à ngForm `<form (ngSubmit)="onSubmit(f)" #f="ngForm">` , on a donc la possibilité d'écrire :

```html
<button 
        class="btn btn-primary" 
        type="submit"
        [disabled] ="!f.valid" // [disabled]="f.invalid"
 >Submit</button>
```

## Jouer sur les classes css ng-invalid, ng-dirty ...

Dans le fichier `my-app.component.css`  :

```css
input.ng-invalid{
  border: 2px solid red;
}
```

Si on veut être un peu plus subtil et ne pas déclencher la ligne rouge à l'apparition du formulaire on peutr ajouter la nécéssité d'avoir été "*touchée*" pour le `input` avec `ng-touched`  :

```css
input.ng-invalid.ng-touched{
  border: 2px solid red;
}
```

## Afficher un texte d'erreur pour entrée invalide

On va utiliser la référence sur un élément : `#username="ngModel"`

```html
<input 
       type="text" 
       id="username" 
       class="form-control"
       ngModel
       name="username"
       required
       #username="ngModel"
       >
<div class="alert alert-danger" *ngIf="username.invalid&&username.touched">
    Please enter valid user name
</div>
```

On utilise dans le test `username.invalid && username.touched`

## Valeur par défaut avec ngModel on-way-binding

On peut ajouter une valeur par défaut en utilisant ngModel :

```html
<select 
        id="secret" 
        class="form-control"
        [ngModel]="defaultQuestion"
        name="secret"
        >
    <option value="pet">Your first Pet?</option>
    <option value="teacher">Your first teacher?</option>
</select>
```

Et dans `app.component.ts` :

```typescript
defaultQuestion = 'pet'; // ou defaultQuestion = 'teacher';
```

## Two-way-binding avec un textarea

```html
<textarea 
          name="questionAnswer"
          class="form-control"
          rows="3"
          [(ngModel)]="answer"
></textarea>
```

## Regrouper des champs : ngModelGroup

On peut vouloir créer des groupe de champs à valider ensemble par exemple, et on peut alors avoir une référence sur ce groupeavec les même attributs que ngForm (valid, touched, dirty ...)

```html
<div 
     id="user-data" 
     ngModelGroup="userData"
     #userData="ngModelGroup"
>
```

on peut alors utiliser la validation du groupe pour afficher un message ou dans mon cas le bouton *submit* avec `*ngIf` : 

```html
<button 
        *ngIf="userData.valid"
        class="btn btn-primary" 
        type="submit"
        [disabled] ="!f.valid"
>Submit</button>
```

## Radio Button

```typescript
export class AppComponent {
// ...
  genders = ['male', 'female', 'x'];
// ...
}
```



```html
<div class="radio" *ngFor="let gender of genders">
    <label>
        <input 
               type="radio"
               name="gender"
               ngModel
               [value]="gender"
        >
        {{gender}}
    </label>
</div>
```

## setValue et patchValue

On peut changer les valeurs de tout le formulaire (obligatoirement tout le formulaire !) avec `setValue`  :

```typescript
suggestUserName() {
    const suggestedName = 'Superuser';
    this.signupForm.setValue({
        userData: {
            username: suggestedName,
            email: suggestedName + '@gmal.cop'
        },
        gender: 'x',
        secret: 'pet',
        questionAnswer: 'yo yo'
    });
}
```

Si on veut changer et réécrire la valeur d'un seul champs on utilise `form.patchValue` :

```typescript
suggestUserName() {
    const suggestedName = 'Superuser';
    this.signupForm.form.patchValue(
        {
            userData: {
                username: suggestedName
            }
        }
    );
}
```

## Reset un formulaire

le formulaire possède une méthode `reset()` qui remet à zéro le formulaire (`pristine: true`, `untouched: true`)

On peut passer un objet de valeurs pour ré-initiliser le formulaire.

```typescript
onSubmit() {
    console.log(this.signupForm.value);

    this.submitted = true;

    this.user.username = this.signupForm.value.userData.username;
    this.user.mail = this.signupForm.value.userData.email;
    this.user.secretQuestion = this.signupForm.value.secret;
    this.user.answer = this.signupForm.value.answer;
    this.user.gender = this.signupForm.value.gender;

    // this.signupForm.reset();
    this.signupForm.reset({
        userData: {
            username: 'akira',
            email: ''
        },
        gender: 'male',
        secret: 'pet',
        answer: 'yo yo'
    });
    console.log(this.signupForm); // pristine: true untouched: true
}
```

## Personnaliser le message d'erreurs grâce aux codes erreurs

```html
<div class="form-group">
    Email : 
    <input 
           type="text" 
           class="form-control"
           name="email"
           ngModel
           required
           email
           #email="ngModel">
    <p class="alert alert-warning" *ngIf="email.errors?.required">
        email required
    </p>
    <p class="alert alert-warning" *ngIf="email.errors?.email">
        email invalid
    </p>
</div>
```

Chaque controle (ainsi que le formulaire lui-même) possède un objet `errors {[key: string]: boolean}` où key est la clé associée à un booléen.

Ainsi on a :

```js
email
  errors: {'required': true}
```

On écrit `errors?.` sinon on obtient une erreur car errors n'a pas encore obtenu sa valeur :

```bash
ERROR TypeError: Cannot read property 'required' of null
```

#### écriture alternative avec `hasError`

```html
 <p class="alert alert-warning" *ngIf="form.hasError('required', 'email')">
     email required
</p>
<p class="alert alert-warning" *ngIf="form.hasError('email', 'email')">
    email invalid
</p>
```



