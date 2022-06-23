# 02 `template` et `directive`

## Linked `Template`

<img src="assets/first-step-template-url.png" alt="first-step-template-url" style="zoom:50%;" />

```typescript
@Component({
    selector: 'pm-product-list',
    templateUrl: './product-list.component.html',
})
```

<img src="assets/step-two.png" alt="step-two" style="zoom:33%;" />

On doit déclarer notre `component` dans notre `module`.

`app.module.ts`

```typescript
import { AppComponent } from './app.component';
import { ProductListComponent } from './product/product-list.component';

@NgModule({
    declarations: [AppComponent, ProductListComponent],
    imports: [BrowserModule],
    bootstrap: [AppComponent],
})
```



## Binding

### Interpolation

One Way Binding

Le code placé entre double accolade est appelé `template expression`.

<img src="assets/Screenshot 2021-02-04 at 15.47.06.png" alt="Screenshot 2021-02-04 at 15.47.06" style="zoom:50%;" />

Un `template expression` est converti par **Angular** en chaîne de caractère et remplacé dans le `html`.

