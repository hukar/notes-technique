## Autre syntaxe de *ngIF

`*` est un équivalent syntaxique du code suivant :

```html
<ng-template [ngIf]="evenNumberShow">
    <div>
        ...
    </div>
</ng-template>

// ou en *ngIf
<div *ngIf="evenNumberShow">
    ...
</div>
```

## Créer une directive structurale

```html
<span *appUnless="number%2!==0||evenNumberShow">{{number}}</span>
```

`*appUnless` fait l'inverse de `*ngIf` :

```typescript
import { Directive, Input, TemplateRef, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[appUnless]'
})
export class UnlessDirective {

  // setter de la propiété, il s'exécute lorsque la valeur de l'attribut change
  @Input('appUnless') set unless(condition: boolean) {
    if (!condition) {
      this.vcRef.createEmbeddedView(this.templateRef);
    } else {
      // pour vider la vue à cet endroit
      this.vcRef.clear();
    }
  }

  // accés au template de l'élément
  // template == what
  // view == where mark the place
  constructor(private templateRef: TemplateRef<any>, private vcRef: ViewContainerRef) { }

}
```

`@Input('appUnless')`  défini un alias sur le nom de la directive.

 