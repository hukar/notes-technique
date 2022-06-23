# 05 Angular Material Datepicker

## anatomie du datepicker

1. encadrer avec `mat-form-field`
2. déclarer un simple `input`
3. lui ajouter la directive `matInput`

4. un composant `mat-datepicker-toggle`, l'icône pour ouvrir le *datepicker*
5. le *datepicker* lui-même `mat-datepicker`
6. créer une référence dans `mat-picker`  => `#picker`
7. binder l'attribut for de `mat-datepicker-toggle` vers cette référence
8. une directive sur input `matDatepicker` binder vers `#picker`
9. ajouter `matSuffix` sur `mat-datepicker-toggle` pour pousser l'icône à droite

```html
<mat-form-field>
      <input 
        matInput
        placeholder="your birthdate"
        [matDatepicker]="picker">
        <mat-datepicker-toggle matSuffix [for]="picker"></mat-datepicker-toggle>
        <mat-datepicker #picker></mat-datepicker>
    </mat-form-field>
```

## restreindre la plage de date

Dans `material.module` :

```typescript
import { MatNativeDateModule } from '@angular/material'

imports: [MatNativeDateModule],
exports: [MatNativeDateModule]
```

Dans le template :

```html
<mat-form-field>
    <input 
           matInput
           placeholder="your birthdate"
           [matDatepicker]="picker"
           [max]="maxDate">
    <mat-datepicker-toggle matSuffix [for]="picker"></mat-datepicker-toggle>
    <mat-datepicker #picker></mat-datepicker>
</mat-form-field>
```

On lie (binding) l'attribut max

Dans le composant :
```typescript
maxDate;

ngOnInit() {
    this.maxDate = new Date();
    this.maxDate.setFullYear(this.maxDate.getFullYear() - 18)
}
```

Avec une date minimum

```html
<input 
        matInput
        placeholder="your birthdate"
        [matDatepicker]="picker"
        [max]="maxDate"
        [min]="minDate">
```

Dans le composant :

```typescript
maxDate;
minDate;

ngOnInit() {
    this.maxDate = new Date(2010, 8, 6); // 6 september 2010
    this.minDate = new Date(2010, 7, 3); // 3 augustus 2010
}
```

`Date(year, indexMonth, day)`