# 06 Angular Material Checkbox

import dans material.module:

```typescript
import { MatCheckboxModule } from '@angular/material'

@NgModule
imports: [MatCheckboxModule],
exports: [MatCheckboxModule]
```

template :

```html
<mat-checkbox 
      labelPosition="before"
      ngModel
      name="conditions"
      required
      color="primary"
>Accept terms and consitions</mat-checkbox>
```

