# Directive advanced

| attribute directive                    | structurale directive             |
| -------------------------------------- | --------------------------------- |
| attribut normal                        | attribut normal avec une * devant |
| change juste l'élément où il se trouve | modifie le DOM                    |

## Directive d'attribut

```typescript
// dans un fichier basic-highlight.directive.ts
import { Directive, OnInit, ElementRef } from '@angular/core';

@Directive({
    selector: '[appBasicHighlight]'
})
export class BasicHighlightDirective implements OnInit {

    constructor(private elementRef: ElementRef) {}

    ngOnInit(): void {
        this.elementRef.nativeElement.style.backgroundColor = 'yellow';
        this.elementRef.nativeElement.style.fontWeight = 'bold';
    }

}
```

Le constructeur récupère l'élément ciblé par la directive.

Dans `selector: '[appBasicHighlight]'` les crochets veulent dire que la directive est un attribut.



Dans app.module.ts on enregistre sa directive :

```typescript
...
import { BasicHighlightDirective } from './basic-highlight/basic-highlight.directive';  // <- ici

@NgModule({
  declarations: [
    ...
    BasicHighlightDirective  // <- ici
  ],
  ...
})
export class AppModule { }
```

## créer un directive avec le CLI

```sh
ng generate directive ma-directive
# ou bien
ng g d ma-directive
```

## Deuxième méthode avec Renderer2

```typescript
import { Directive, OnInit, ElementRef, Renderer2 } from '@angular/core';

@Directive({
  selector: '[appBetterHighlight]'
})
export class BetterHighlightDirective implements OnInit {

  constructor(private eltRef: ElementRef, private renderer: Renderer2) { }

  ngOnInit(): void {
    this.renderer.setStyle(this.eltRef.nativeElement, 'background-color', 'blue');
    this.renderer.setStyle(this.eltRef.nativeElement, 'color', 'white');
  }
}
```

On importe `Renderer2`

On récupère dans le constructeur **la référence à l'élément** et **le renderer**

#### ! il est préférable d'utiliser le renderer pour modifier le DOM

## Écouter un événement dans la directive

```typescript
import { Directive, OnInit, ElementRef, Renderer2, HostListener } from '@angular/core';

@Directive({
  selector: '[appBetterHighlight]'
})
export class BetterHighlightDirective implements OnInit {

  constructor(private eltRef: ElementRef, private renderer: Renderer2) { }

  ngOnInit(): void {
    this.renderer.setStyle(this.eltRef.nativeElement, 'background-color', 'blue');
    this.renderer.setStyle(this.eltRef.nativeElement, 'color', 'white');
  }

  @HostListener('mouseenter') mickey() {
    console.log('coucou');
    this.renderer.setStyle(this.eltRef.nativeElement, 'background-color', 'cornflowerblue');
    this.renderer.setStyle(this.eltRef.nativeElement, 'color', 'yellow');
  }

  @HostListener('mouseleave') mini() {
    this.renderer.setStyle(this.eltRef.nativeElement, 'background-color', 'blue');
    this.renderer.setStyle(this.eltRef.nativeElement, 'color', 'white');
  }
}

```

On utilise le decorator `@HostListener`

il prend en paramètre l'événement du DOM pour lequel on exécute la fonction *décorée*

## Troisième méthode avec @HostBinding

On peut lier une propriété de l'élément du DOM à une variable javascript de la directive.

```typescript
import { Directive, HostBinding, HostListener } from '@angular/core';

@Directive({
  selector: '[appBindingHighlight]'
})
export class BindingHighlightDirective {

  @HostBinding('style.backgroundColor') bgcolor = 'deeppink';

  @HostBinding('style.color') color = 'darkviolet';

  @HostListener('mouseenter') in() {
    this.bgcolor = 'darkviolet';
    this.color = 'deeppink';
  }

  @HostListener('mouseleave') out() {
    this.bgcolor = 'deeppink';
    this.color = 'darkviolet';
  }

  constructor() { }
}
```

Le decorator `@HostBinding` prend en paramètre le nom de la propriété de l'élément HTML et la lie avec la variable déclarée après.

## directive paramétrée

En combinant le `@HostBinding` et le binding d'attribut on peut paramétrer sa directive.

```typescript
import { Directive, HostBinding, HostListener, Input, OnInit } from '@angular/core';

@Directive({
  selector: '[appBindingHighlight]'
})
export class BindingHighlightDirective implements OnInit {
    
   // on doit écraser les valeurs par défaut des paramètres
  ngOnInit(): void {
    this.bgcolor = this.customBgcolor;
    this.color = this.customColor;
  }

    // valeurs par défaut
  @Input() customColor = 'black';
  @Input() customBgcolor = 'yellow';

  @HostBinding('style.backgroundColor') bgcolor = this.customBgcolor;

  @HostBinding('style.color') color = this.customColor;

  @HostListener('mouseenter') in() {
    this.bgcolor = this.customColor;
    this.color = this.customBgcolor;
  }

  @HostListener('mouseleave') out() {
    this.bgcolor = this.customBgcolor;
    this.color = this.customColor;
  }

  constructor() { }

}

```

Dans le template :

```html
<span appBindingHighlight [customBgcolor]="'darkviolet'" [customColor]="'pink'">
    sit amet consectetur adipisicing elit. Minima aspernatur ea suscipit
</span>
```

Ne pas oublier les guillemets simples dans les guillemets doubles car on passe un string dans l'expression javascript contenue dans les guillemets doubles : `[customBgcolor]="'darkviolet'"`

On peut simplifier l'écriture dans ce cas de figure où on passe un string à l'attribut binder :

```html
<span appBindingHighlight customBgcolor="darkviolet" customColor="pink">
    sit amet consectetur adipisicing elit. Minima aspernatur ea suscipit
</span>
```

On retire les crochets et les guillemets simples, mais attention à ne pas confondre avec un attribut natif.

## Directive de type `[MonAttribut]="{...}"`

Pour obtenir des directive fonctionnant comme [ngClass] par exemple, il faut créer un attribut binder dont l'alias est le même nom que la directive :

```typescript
import { Directive, HostBinding, HostListener, Input, OnInit } from '@angular/core';

@Directive({
  selector: '[appBindingHighlight]'
})
export class BindingHighlightDirective implements OnInit {
  ngOnInit(): void {
    this.bgcolor = this.highlight.bgcolor;
    this.color = this.highlight.color;
  }

    // ici on crée un alias dans @Input('alias')
  @Input('appBindingHighlight') highlight: {bgcolor: string, color: string};

  @HostBinding('style.backgroundColor') bgcolor;

  @HostBinding('style.color') color;

  @HostListener('mouseenter') in() {
    this.bgcolor = this.highlight.color;
    this.color = this.highlight.bgcolor;
  }

  @HostListener('mouseleave') out() {
    this.bgcolor = this.highlight.bgcolor;
    this.color = this.highlight.color;
  }

  constructor() { }

}
```

#### `@Input('alias') monAttribut: type`

Dans le template on peut écrire :

```html
<span [appBindingHighlight]="{bgcolor:'darkviolet',color:'forestgreen'}">sit amet consectetur adipisicing elit. Minima aspernatur ea suscipit</span>
```

## exemple d'un dropdown

```typescript
import { Directive, HostListener, Input, Renderer2 } from "@angular/core";

@Directive({
    selector: '[appDropdown]',
})

export class DropdownDirective {

    isShow: boolean;

    @Input('appDropdown') dropmenu: HTMLElement;

    constructor(private renderer: Renderer2) {}
    
    @HostListener('click') toggleDropdown() {
        this.isShow?
            this.renderer.removeClass(this.dropmenu, 'show'):
            this.renderer.addClass(this.dropmenu, 'show');
        this.isShow = !this.isShow;
    }
}
```

#### ! On modifie le dom avec renderer.removeClass et rederer.addClass

Dans le template :

```html
<div class="dropdown" [appDropdown]="dropmenu">
            <button class="btn btn-outline-primary dropdown-toggle">
                Manage Recipe
            </button>
            <div class="dropdown-menu" #dropmenu>
                <a href="#" class="dropdown-item">To Shopping List</a>
                <a href="#" class="dropdown-item">Edit Recipe</a>
                <a href="#" class="dropdown-item">Delete Recipe</a>
            </div>
        </div>
```

La classe `show` s'appliquant sur un div interne, je le récupère avec `#dropmenu` et le passe en argument à ma directive.

## dropdown simplifié

Dans les css global on cible l'enfant :

```css
.show div{
    display: block;
}
```

Du coup plus besoin de la référence vers le div en question on a :

```typescript
import { Directive, HostListener, HostBinding, Input } from "@angular/core";

@Directive({
    selector: '[appDropdown]',
})

export class DropdownDirective {
    @HostBinding('class.show') isShow = false;
    
    @HostListener('click') toggleDropdown() {
        this.isShow = !this.isShow;
    }
}
```

