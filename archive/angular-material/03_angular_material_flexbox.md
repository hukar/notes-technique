#03 Angular Material Flexbox

Module Angular Flex-Layout

##Flexbox CSS

```sass
#container{
    display: flex;
    width: 100%;
    flex-direction: row; /* column; */ // par default c'est row
}

.child{
    /* width: 200Px; */
    height: 200px;
    flex: 1; // occupation d'un élément, par default 0 (élément écrasé)
}

#child_1{
    background-color: cornflowerblue;
}
#child_2{
    background-color: gold;
}
#child_3{
    background-color: lightcoral;
    flex: 2; // cet élément occupe deux fois plus de place
}
```

Main axis top -> bottom = column

Alignement vertical :

```sass
flex-direction: column;
justify-content: center;
```

cross axis = opposite of the Main axis

```sass
flex-direction: column;
justify-content: center;
align-items: flex-end; // center flex-start
```

Dans ce cas `align-items`  agit sur l'axe horizontal

Main axis = justify content

Cross axis = align-items

flex-direction: row => main axis left -> right

flex-direction: column => main axis top -> bottom

## package @angular/flex-layout

installation via NPM

```bash
npm install --save @angular/flex-layout@latest
```

référencer dans `app.module`

```typescript
import { FlexLayoutModule } from '@angulat/flex-layout'

...

@NgModule({
    ... ,
    imports: [
    ...,
    FlexLayoutModule
    ]
})
export class AppModule { }
```

#### Les directives du module

`fxLayout` par défaut row

`fxLayout="column"` 

`fxLayoutAlign="center center"` main axis + cross axis ici center sur y et center sur x

```html
<form fxLayout="column" fxLayoutAlign="center center">
    <mat-form-field>
        <input type="email" matInput placeholder="Your email">
    </mat-form-field>
    <mat-form-field>
        <input type="password" matInput placeholder="Your password">
    </mat-form-field>
  </form>
```

On peut dans `mycomponent.component.css` cibler directement un composant material-angular :

```css
mat-form-field{
    width: 300px;
}
```

css s'applique sur n'importe quel type de balise ou d'attribut de manière native.

```html
<ma-balise>hello</ma-balise>
<p chatAlors>Kitty</p>
<p>Popo</p>
```

css : 

```css
ma-balise{
  color: red;
}

p[chatAlors]{
  color: yellow;
}
```

On a bien *hello* en rouge, *kitty* en jaune et *popo* en noir par défaut.



### espace entre les éléments

`fxLayoutGap="14px"` 