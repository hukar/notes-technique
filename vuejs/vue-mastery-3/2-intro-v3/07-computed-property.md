# 07 Propriétés calculées

Une propriété calculée permet d'encapsuler la logique d'une propriété obtenu à partir d'autres propriétés.

## Calculé le titre

On ajoute à l'objet d'options la clé `computed` :

```js
const app = Vue.createApp({
  data() {
    return {
      key: value,
    };
  },
  methods: {
    myMethod() {},
    mySecondMethod() {},
  },
  computed: {
    title() {
      return this.key + "is the title";
    },
  },
});
```

```js
const app = Vue.createApp({
  data() {
    return {
      product: "Socks",
      brand: "Vue Mastery",
      // ...
    };
  },
  methods: {
    /* ... */
  },
  computed: {
    title() {
      return this.brand + " -- " + this.product;
    },
  },
});
```

<img src="assets/Screenshot2020-09-21at15.46.26.png" alt="Screenshot 2020-09-21 at 15.46.26" style="zoom:67%;" />

Le résultat d'une propriété calculée est mis en cache pour améliorer les performances.



## Exemple plus complexe

On va passer l'`index` d'une variation de chaussette pour calculer `image` et `inStock` :

```html
<div
     class="color-circle"
     v-for="(variant, index) in variants"
     :key="variant.id"
     @mouseover="updateVariant(index)"
     :style="{ backgroundColor: variant.color }"
     ></div>
```

`(elt, index) in elts` permet de récupérer l'index d'un élément de collection.

```js
data() {
  return {
    product: 'Socks',
    brand: 'Vue Mastery',
    curentVariant: 0,
    variants: [
      {
        id: 2234,
        color: 'green',
        image: './assets/images/socks_green.jpg',
        quantity: 0,
      },
      {
        id: 2235,
        color: 'blue',
        image: './assets/images/socks_blue.jpg',
        quantity: 50,
      },
    ],
  }
},
  computed: {
    title() {
      return `${this.brand} ${this.product} 🧦`
    },
      inStock() {
        return this.variants[this.curentVariant].quantity // false si 0 par défaut
      },
        image() {
          return this.variants[this.curentVariant].image
        },
  },
```

