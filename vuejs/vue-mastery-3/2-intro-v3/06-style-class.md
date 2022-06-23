# 06 Lier le style et les classes

## `style` binding

```html
<div
     v-for="variant in variants"
     class="color-circle"
     :style="{ backgroundColor: variant.color }"
     @mouseover="changeSockColor(variant.color, $event)"
></div>
```

#### `:style="{camelCaseProperty: value}"`

les propriétés `css` sont écrites en `camelCase`.

On peut aussi mettre les propriété en `kebab-case` entre guillemets :

`:style="{ 'background-color': variant.color }"`

Si l'objet est trop gros, on peut le définir dans les `data` :

```js
data() {
  return {						
    circleStyle: {
      backgroundColor: 'pink',
      border: '1px solid blue',
    },
```



## `:disabled`

On peut assigner un booléen à l'attribut `disabled` :

```html
<button
        class="button"
        :disabled="!inStock"
        @click="addToCart"
>
```



##  `class` binding

On peut passer un objet avec comme clé le nom de la classe et comme valeur un booléen.

La classe est ajoutée si le booléen est à `true`.

```html
<button
        class="button"
        :class="{ disabledButton: !inStock }"
        :disabled="!inStock"
        @click="addToCart"
>
```

On peut utiliser `class` et `:class` en même temps.

### `in-line ternary operator`

#### `:class="[ condition ? si_vrai : si_faux ]"`

```html
<button
        class="button"
        :class="[inStock ? '' : 'disabledButton']"
        :disabled="!inStock"
        @click="addToCart"
>
```

On peut encore simplifier en :

```html
<button
        :class="['button', inStock ? '' : 'disabledButton']"
        :disabled="!inStock"
        @click="addToCart"
>
```

