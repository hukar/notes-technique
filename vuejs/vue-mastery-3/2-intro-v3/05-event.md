# 05 `event`

## `v-on:click `

Pour donner à Vue la gestion du `click` on utilise la directive `v-on:` devant l'événement moins le préfixe `on`:

```html
<button class="button" v-on:click="cart = cart + 1">
  Add to cart
</button>
```

## Relier une méthode

On écrit les méthodes dans l'objet options passé à `Vue.createApp` :

```js
const app = Vue.createApp({
    data() {
        return {
            product: 'Socks',
            cart: 0,
        }
    },
    methods: {
        addToCart() {
            this.cart += 1
        },
    },
})
```

```html
<button class="button" v-on:click="addToCart">
  Add to cart
</button>
```

### Raccourci de `v-on:click` : `@click`



## évènement `hover` : `@mouseover`

```html
<div
     v-for="variant in variants"
     :style="'background-color:' + variant.color"
     @mouseover="changeSockColor(variant.color)"
></div>
```

```js
methods: {
  // ...
  changeImageColor(color) {
    console.log('color', color)
    this.image = `./assets/images/socks_${color}.jpg`
  },
```



## ! on peut passer le nom d'une méthode ou une instruction (expression ?)  javascript valide

```html
<button class="button" v-on:click="addToCart">
```

et

```html
<button class="button" v-on:click="addToCart(product)">
```

et

```html
<button class="button" v-on:click="cart += 1">
```

sont tous les trois valide et équivalent.

Si on veut en plus récupérer l'objet natif `event` :

```html
<button class="button" v-on:click="addToCart(product, $event)">
```



## `.once` : invoque une seul fois

Avec l'option `.once` l'event handler est retiré après son utilisation :

```html
<button
        class="button button-red"
        @click.once="removeToCart"
>
  remove to cart
</button>
```

