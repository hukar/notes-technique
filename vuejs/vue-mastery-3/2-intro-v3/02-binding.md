# 02 Binding

## Lier les attributs : Attribute binding `v-bind`

<img src="assets/Screenshot2020-09-21at11.21.15.png" alt="Screenshot 2020-09-21 at 11.21.15" style="zoom:67%;" />

```html
<img v-bind:src="image" alt="" />
```

```html
<img :src="image" alt="" />
```

`v-bind` lie un attribut à une expression.

```html
<div class="product-image">
  <img :src="Math.random() > 0.5 ? image_blue : image_green" />
</div>
<p><a :href="url">google me</a></p>
```

On voie qu'on peut mettre un expression `js` dans le contenu d'un attribut `bindé`.



## Plusieurs cas d'utilisation

<img src="assets/use-case-binding-attribute.png" alt="use-case-binding-attribute" style="zoom:50%;" />

