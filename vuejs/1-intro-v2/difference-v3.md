# Différence entre `Vue 2` et `Vue 3`

## `vue.js devtools`

cocher `allow access to file URLs`

## Instancier l'`app`

```js
const app = new Vue({
  el: "#app",
  data: {
    product: "socks",
  },
});
```

`new Vue` à la place de `Vue.createApp`

`data` est un objet et non une fonction retournant un objet : `data() { return { ... }}`

`el:"#app` au lieu d'ajouter dans le `index.html` :

```html
<script>
  const mountedApp = app.mount("#app");
</script>
```

<img src="assets/Screenshot2020-09-23at09.56.00.png" alt="Screenshot 2020-09-23 at 09.56.00" style="zoom:50%;" />

## Reactivity system

Lorsqu'une valeur dans `data` change, l'interface est mis à jour.

## `v-bind`

```jsx
<img :src="image" alt="">
```

est en théorie équivalent à :

```jsx
<img :src={{image}} alt="">
```

mais l'écriture ci-dessus n'est pas permise !

### exemple de `binding`

<img src="assets/Screenshot2020-09-23at11.28.49.png" alt="Screenshot 2020-09-23 at 11.28.49" style="zoom: 33%;" />

## `event`

<img src="assets/Screenshot2020-09-23at12.03.43.png" alt="Screenshot 2020-09-23 at 12.03.43" style="zoom: 33%;" />

## Style binding

On peut utiliser la `kebab-case` entre guillemets si on préfère :

<img src="assets/Screenshot2020-09-23at14.13.10.png" alt="Screenshot 2020-09-23 at 14.13.10" style="zoom: 33%;" />

### Multi-binding

<img src="assets/Screenshot2020-09-23at14.14.33.png" alt="Screenshot 2020-09-23 at 14.14.33" style="zoom:33%;" />

On peut lier plusieurs `styleObject` en utilisant un tableau `:style:"[styleObject1, styleObject2]"`.

## `class` binding

<img src="assets/Screenshot2020-09-23at14.22.09.png" alt="Screenshot 2020-09-23 at 14.22.09" style="zoom:33%;" />

On peut aussi lier un tableau de classe :

<img src="assets/Screenshot2020-09-23at14.23.43.png" alt="Screenshot 2020-09-23 at 14.23.43" style="zoom:33%;" />

### Un mix :

```jsx
<button
    @click="addToCart"
    :disabled="!inStock"
    :class="[{disabledButton: !inStock}, {'border-active': borderActive}, bigFont, radiusMiddle]">
    Add To Cart</button>
```

### expression ternaire

```jsx
:class="[inStock ? '' : 'disabledButton']"
```

## Computed properties

Il y a une mise en cache des propriétés calculées.

Une propriété calculée va être réécrite lorsqu'un de ses éléments change.

## Component

<img src="assets/Screenshot2020-09-23at16.07.01.png" alt="Screenshot 2020-09-23 at 16.07.01" style="zoom:33%;" />

On a `Vue.component` au lieu de `app.component`.

On ne peut pas avoir de balise `html` cousine sans un parent englobant.

`data` est aussi une fonction : c'est pour que les données d'un composant aient leur propre `scope` lié à `data() { ... }`.

### props

Ce sont les données reçu du parent.

<img src="assets/Screenshot2020-09-23at16.11.48.png" alt="Screenshot 2020-09-23 at 16.11.48" style="zoom:33%;" />

Plutôt qu'un tableau de `props` on recommande un objet de `props` avec ses propres validations définies :

<img src="assets/Screenshot2020-09-23at16.13.34.png" alt="Screenshot 2020-09-23 at 16.13.34" style="zoom:33%;" />

## Two way data binding

<img src="assets/Screenshot2020-09-24at09.57.26.png" alt="Screenshot 2020-09-24 at 09.57.26" style="zoom:33%;" />
