# 21  Formulaire: L'approche réactive

## Setup dans app.module

Plus besoin de `FormsModule` à la place on importe  `ReactiveFormsModule`

```typescript
// ...
import { ReactiveFormsModule } from '@angular/forms';

@NgModule({
  declarations: [ /# ... #/ ],
  imports: [
    // ...
    ReactiveFormsModule,
    // ...
  ],
  // ...
```

Dans le composant on declare un FormGroup :

```typescript
import { FormGroup } from '@angular/forms';

export class AppComponent {
  signupForm: FormGroup;
    // ...
```

## Création du formulaire côté typescript

```typescript
export class AppComponent implements OnInit {
  genders = ['male', 'female'];
  signupForm: FormGroup;

  ngOnInit(): void {
    this.signupForm = new FormGroup({
      'username': new FormControl('toto'),
      'email': new FormControl(null),
      'gender': new FormControl('male')
    });
  }
}
```

Initialiser signupForm dans `ngOnInit` pour être sûre que la vue est rendu.

Mettre les noms de propriété entre guillemets pour la *minification*.

C'est un `FormGroup` composé de `FormControl`

chaque `FromControl` prend une valeur par défaut : `new FormControl('toto')`

## Utilisation du form builder

D'abord importer le form builder :

```typescript
import { FormBuilder } from '@angular/forms';
```

Puis injecter le form builder dans le constructeur :

```typescript
export class AppComponent implements OnInit {

  constructor(private fb:FormBuilder) {}

}
```

Le form builder renvoie un FormGroup et prends une collection de contrôle :

```typescript
export class AppComponent implements OnInit {

  projectForm: FormGroup = this.fb.group({
    projectName: ['titi', Validators.required],
    email: ['', Validators.email],
    status: ['Stable']
  });
  
// ...
```



## Côté html

```html
<form [formGroup]="signupForm">
        
    <input
           type="text"
           id="username"
           formControlName="username"
           class="form-control">
</form>
```

`formControlName="username"`  est l'écriture simplifié de `[formControlName]="'username'"` possible lorsque l'on passe un `string` à un *binding*.

## Soumettre un formulaire

Dans le template :

```html
<form [formGroup]="signupForm" (ngSubmit)="onSubmit()">
```

Dans le composant :

```js
onSubmit() {
    console.log(this.signupForm);
}
```

Ici rien ne diffère de l'approche `template driven`.



## Validation de formulaire

Dans l'approche réactive, on ne trouve dans le HTML que `[formGroup]="signupForm"` et `formControlName="username"` pour synchroniser le formulaire avec sa représentation Typescript dans le composant. Les élément de validation se mettent dans le composant grâce à la classa `Validators`

####! huber@spfserver1 est un email valid

```typescript
import { FormGroup, FormControl, Validators } from '@angular/forms';  // <- les imports

this.signupForm = new FormGroup({
    'username': new FormControl('toto', Validators.required),
    'email': new FormControl('toto@hot.com', [Validators.email, Validators.required]),
    'gender': new FormControl('male')
});
```

La classe `Validators`  offrent plusieurs méthodes statiques de validation.

On peut passer plusieurs fonction de validation dans un tableau.

## Message erreur

```html
<span
      *ngIf="signupForm.get('username').invalid && signupForm.get('username').touched"
      class="help-block">Please enter a valid username</span>
```

On a accès à notre élément via `signupForm.get('nomelement')`.

Pareillement pour le formulaire en lui-même on a :

```html
<span
      *ngIf="signupForm.invalid && signupForm.touched"
      class="help-block"
      >form isn't valid!</span>
```

On peut toujours utiliser les classes générées par angular :

####Ajouter une ligne rouge autour des champs invalides

```css
/* Dans app.component.css */
.container {
  margin-top: 30px;
}

input.ng-invalid.ng-touched{
  border: 1px solid red;
}
```

## Grouper les `controls` : `FormGroup`

On peut imbriquer des `FormGroup` dans des `FormGroup` :

```js
this.signupForm = new FormGroup({
    'userData': new FormGroup({
        'username': new FormControl('titi', Validators.required),
        'email': new FormControl(null, [Validators.required, Validators.email])
    }),
    'gender': new FormControl('male')
});
```

Il faut changer le 'path' dans le html :

```html
<span
      *ngIf="signupForm.get('userData.username').invalid && signupForm.get('userData.username').touched"
      class="help-block"
      >username isn't valid!</span>
```

`get('userData.username')` on compose le 'path' avec `.`

`nomDuGroup.nomDuControle` 

## Un tableau de `controls` : `FormArray`

On peut vouloir créer un nombre dynamique de champs par exemple pour enregistrer les hobbies (0, 1, 10, +).

On utilise alors un tableau de champs de formulaire :

```js
import { FormGroup, FormControl, Validators, FormArray } from '@angular/forms';
// on importe bien FormArray

this.signupForm = new FormGroup({
    'userData': new FormGroup({
        'username': new FormControl('titi', Validators.required),
        'email': new FormControl(null, [Validators.required, Validators.email])
    }),
    'gender': new FormControl('male'),
    'hobbies': new FormArray([]) // on instancie un nouveau FormArray
});
```

Le constructeur de `FormArray` prend un tableau de `FormControl` en argument, ici le tableau est vide.

Une méthode se charge d'ajouter des `controls`.

```js
onAddHobby() {
    const control = new FormControl(null, Validators.required);
    (<FormArray>this.signupForm.get('hobbies')).push(control);
}
```

#### ! le cast est obligatoire pour utiliser `push` car la méthode `get` peut très bien renvoyer un `FormControl` seul et non un `FormArray` .

Ensuite dans le template :

```html
<div formArrayName="hobbies">
    <button 
            class="btn btn-warning"
            type="button"
            (click)="onAddHobby()">Add Hobby</button>
    <div 
         class="form-group"
         *ngFor="let hobbyControl of signupForm.get('hobbies').controls; let i = index">
        <input type="text" [formControlName]="i">
    </div>
</div>
```

`formControlName` fait ici référence à l'index du tableau *hobbies*.

---

> Pour rappel un élément de tableau en javascript n'est identifié que par un entier positif, aucune chaîne de caractère n'est permise comme index.
>
> ```js
> const t = ["titi", "toto"];
> t["deux"] = "tata"
> 
> console.log(t.length); // -> 2
> ```
> En fait *"deux"* devient une propriété de l'objet `t`, mais ne s'ajoute pas 
>
> ```js
> const t = ["titi", "toto"];
> t[2] = "tata"
> 
> console.log(t.length);  // -> 3
> ```
> Seul les index entiers positifs sont pris en compte
>
> ```js
> const t = ["titi", "toto"];
> t[2000] = "tata"
> 
> console.log(t.length); //-> 2001
> console.log(t[678]); // undefined
> ```
>
> Un index plus grand que le suivant rempli le tableau de `undefined`

---

On pourrait ne pas *'binder'*  `formControlName` et écrire ceci :

```html
<div 
     class="form-group"
     *ngFor="let hobbyControl of signupForm.get('hobbies').controls; let i = index">
    <input type="text" formControlName="{{i}}">
</div>
```

#### ! Binder un attribut permet de lui passer une expression angular, les variables sont donc disponibles par leur nom.

## Nos propres `Validators`

Un `validators` est une fonction.

On crée une propriété des valeurs interdites :

```js
forbiddenUsernames = ['titi', 'toto'];
```

Ensuite on écrit la fonction de validation :

```js
forbiddenNames(control: FormControl): {[s: string]: boolean} {
    if (this.forbiddenUsernames.indexOf(control.value) !== -1) {
      console.log('name is forbidden');
      return {'nameIsForbidden': true};
    }

    return null;
  }
```

`{[s: string]: boolean}` cette syntaxe définie un ensemble clé-valeur dont la clé est une chaîne de caractère et la valeur un booléen.

La fonction retourne `null` si l'entrée est permise et l'ensemble `{clé:valeur}` si elle ne l'est pas.

Puis on l'associe à un FormControl :

```js
'username': new FormControl('titi', [Validators.required, this.forbiddenNames.bind(this)]),
```

On utilise `bind` pour envoyer la référence du composant, car sinon lorsque la fonction est exécutée `this` pointe sur `undefined` et `this.forbiddenUsernames.indexOf(control.value)` génère une erreur :

```bash
 ERROR TypeError: Cannot read property 'forbiddenUsernames' of undefined
```

## Utiliser le code erreur de son `validators`

Notre `Validators` return un couple `{codeErreur: booléen}`, on va utiliser ce code erreur pour afficher un message spécifique.

<img style="height:300px" src="/Users/hukar/Documents/notes-techniques/cheat-sheet/img/validators_code.png"/>



Voilà où il se trouve dans le DOM.

Maintenant si c'est le `validators` required qui est concerné on a dans le DOM ceci :

<img style="height:300px" src="/Users/hukar/Documents/notes-techniques/cheat-sheet/img/validators_code_required.png"/>

L'objet `{'required': true}` est renvoyé par le `Validators` `required`.

Voici un exemple de code tirant partie du code d'erreur pour personnaliser le message affiché à l'utilisateur :

```html
<span
      *ngIf="signupForm.get('userData.username').invalid && signupForm.get('userData.username').touched"
      class="help-block"
      >
    <span
          *ngIf="signupForm.get('userData.username').errors['nameIsForbidden']">
        this name is forbidden<br>
    </span>
    <span
          *ngIf="signupForm.get('userData.username').errors['required']">
        this name is requirred<br>
    </span>
</span>
```

On récupère ce code avec `signupForm.get('userData.username').errors['nomDuCode']`

### Exemple de validation d'email complet

dans `component.ts` :

```js
export class AppComponent {

  private myForm: FormGroup;

  constructor() {
    this.myForm = new FormGroup({
     
      'email': new FormControl(null,[
        Validators.required,
        Validators.email,
        Validators.minLength(4)
      ])
    });
  }
}
```

Dans le template :

```html
<div class="form-group">
    Email : 
    <input 
           type="text" 
           class="form-control"
           name="email"
           formControlName="email">
    <div *ngIf="myForm.get('email').invalid && myForm.get('email').touched">

        <p class="alert alert-danger" *ngIf="myForm.get('email').errors['required']">
            email required
        </p>
        <p class="alert alert-warning" *ngIf="myForm.get('email').errors['email']">
            email invalid
        </p>
        <p class="alert alert-info" *ngIf="myForm.get('email').errors['minlength']">
            taille: {{myForm.get('email').errors['minlength'].actualLength}}/{{myForm.get('email').errors['minlength'].requiredLength}}
        </p>
    </div>
</div>
```

Pour récupérer une erreur par son nom : `myForm.get('monElement').errors['nomErreur']`

`minlength` possède deux propriétés `actualLength` => taille actuelle et `requiredLength` ici `= 4`

### ! ERROR TypeError: Cannot read property 'forbiddenProjectName' of null

Si on ne place pas `<div *ngIf="myForm.get('email').invalid && myForm.get('email').touched">` avant un `<p class="alert alert-info" *ngIf="myForm.get('email').errors['minlength']">` par exemple on obtient une erreur, car quand `errors` passe à `null` , `errors['quelqueChose']` provoque une erreur.

## `Validators` Asynchrone

Certain contrôle peuvent se faire sur le serveur, il faut alors un validators asynchrone. 

```js
forbiddenEmails(control: FormControl): Promise<any> | Observable<any> {
    return new Promise<any>(
      (resolve) => {
        setTimeout(
          () => {
            if(control.value == 'test@test.com') {
              resolve({'isForbiddenEmail': true});
            } else {
              resolve(null);
            }
          }
          ,1500);
      }
    );
  }
```

Ici on utilise une `Promise`

On ajoute alors le `validators` dans le `FormControl` :

```js
this.signupForm = new FormGroup({
    'email': new FormControl(
        null, 
        [Validators.required, Validators.email],
        this.forbiddenEmails
    )
});
```

En troisième argument du constructeur de `FormControl` on peut ajouter un `Validators` Asynchrone ou un tableau de `Validators` asynchrones

Dans le template on observe une nouvelle classe `ng-pending`, puis `ng-valid` ou `ng-invalid` la remplace après 1500ms. 

#### La même chose avec un `Observable`

```js
forbiddenEmails(control: FormControl): Promise<any> | Observable<any> {
    const s =  new Subject<any>();

    setTimeout(
      () => {
        if(control.value == 'toto@toto.com') {
          s.next({'isForbiddenEmail': true});
          
        } else {
          s.next(null);
        }
        s.complete();
      }
      ,3000);
    
      return s;
  }
```

## Problème avec le validator async

La fonction de validation asynchrone dans le composant :

```js
 usedProjectNameAsync(control: AbstractControl): Promise<{[k: string]: boolean} | null> | Observable<{[k: string]: boolean} | null> | null {
    
    return this.getProjectName(control.value)
    .pipe(
      map(
        x => {
          return x ? {'usedProjectNameAsync': true} : null
        }  
      )    
    );
  }
```

Le problème avec cette méthode, c'est qu'a chaque frappe une requête est envoyée au serveur.

### Une deuxième solution améliorée

```js
 usedProjectNameAsync(control: AbstractControl): Promise<{[k: string]: boolean} | null> | Observable<{[k: string]: boolean} | null> | null {
    
    const obsValidator =  this.getProjectName(control.value)
    .pipe(
      map(
        x => {
          return x ? {'usedProjectNameAsync': true} : null
        }  
      )    
    );

    return timer(1000).pipe(
      switchMap(
        x => obsValidator
      )
    );
  }
```

Ici on utilise un observable `timer` et switchMap qui désinscrit automatiquement l'ancien observable `obsValidator` avant de le réinscrire.

Du coup les requête sont envoyée maximum toute les une seconde.

## 2 `Observable` utiles: `statusChanges` et `valueChanges`

### statusChanges

se déclenche à chaque changement de status :

```js
this.signupForm.get('userData.email').statusChanges.subscribe(
    status => console.log(status)
);
```

ou sur tout le formulaire

```js
this.signupForm.statusChanges.subscribe(
    status => console.log(status)
);
```

### valueChanges

Se déclenche à chaque changement de valeur d'un champ (ou du formulaire) :

```js
this.signupForm.get('userData.email').valueChanges.subscribe(
    value => console.log(value)
);
```

Sortie :

```bash
s
se
ser
serz
```

## Initialiser les valeurs

###`setValue`

On peut initialiser son formulaire avec `setValue` :

```js
this.signupForm.setValue({
    'userData': {
        'username': 'bobo',
        'email': 'b@b'
    },
    'gender': 'female',
    'hobbies': [] // le tableau doit être vide car la création des control est ici dynamique
});
```

###`patchValue`

On peut initialiser/changer que quelques valeurs avec `patchValue` :

```js
this.signupForm.patchValue({
    'userData': {
        'username': 'bobo',
        'email': 'b@b'
    }
});
```

###`reset`

On peut aussi utiliser `resest()` pour remettre son formulaire à zéro :

```js
onSubmit() {
    console.log(this.signupForm);
    this.signupForm.reset();
}
```

ou bien avec des valeurs par default :

```js
onSubmit() {
    console.log(this.signupForm);
    this.signupForm.reset({
        'userData': {
            'username': 'bobo',
            'email': 'b@b'
        }
    });
}
```

