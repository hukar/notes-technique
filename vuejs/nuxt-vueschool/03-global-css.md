# 03 Global `css`

Dans le fichier `nuxt.config.js`, il y a une propriété `css` qui prends un tableau.

Tous fichiers de `css` ajoutés à ce tableau deviennent des `ccs` globaux.

`nuxt.config.js`

```js
css: ["~/assets/style.css"],
```

`~` alias pour le `root directory`.

