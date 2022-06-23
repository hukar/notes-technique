# 02 Angular Material Button

## Le bouton

On doit importer chaque module séparément dans `material.module.ts`  :

```typescript
import { NgModule } from '@angular/core';
import { MatButtonModule } from '@angular/material';  // <- on importe le bouton

@NgModule({
    imports: [MatButtonModule],  // transfert en import/export
    exports: [MatButtonModule]
})
export class MaterialModule {}
```

Maintenant dans un template on peut utiliser les composants `mat-button`

```html
<button mat-button>Basic</button>

<button mat-raised-button color="primary">Primary</button>

<button mat-stroked-button color="accent">Accent</button>

<button mat-flat-button color="warm">warm</button>

  <button mat-icon-button disabled>
    <mat-icon aria-label="Example icon-button with a heart icon">favorite</mat-icon>
  </button>


<a mat-raised-button routerLink=".">Link</a>
```

**Raised :** un petit rond au click

**stroked :** bouton filaire (outlined)

**flat :** sans ombre

**icon :** avec une icône, il faut MatIconModule

Il existe aussi **fab** et **mini-fab**, rond et petit rond.

