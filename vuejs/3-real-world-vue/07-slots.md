# 07. `Slots`

Parfois on voudrait pouvoir personnaliser un composant à chaque usage.

Les `slots` permettent d'insérer un template à l'intérieur d'un composant :

<img src="assets/Screenshot2020-11-06at14.42.08.png" alt="Screenshot 2020-11-06 at 14.42.08" style="zoom: 25%;" />

Le `slot` a accès aux `data` du parent comme le montre l'exemple ci-dessous.

<img src="assets/Screenshot2020-11-06at14.45.30.png" alt="Screenshot 2020-11-06 at 14.45.30" style="zoom:25%;" />

On peut ajouter une valeur par défaut qui sera écrasée si le `template` la redéfinie :

```html
<template>
    <div>
        <button><slot>WORLD</slot></button>
    </div>
</template>
```

`WORLD` est la valeur par défaut.

Dans un composant parent :

```html
<p>
  <base-button></base-button>
  <base-button>HELLO</base-button>
</p>
```

<img src="assets/slot-for-base-button.png" alt="slot-for-base-button" style="zoom:50%;" />



## Modification de `BaseIcon` pour utiliser un `slot`

On voudrait que le texte après l'icone fasse parti du composant, on va utiliser un `slot` :

```html
<template>
    <div>
        <span class="icon-wrapper" v-html="svg">Icon</span>
        <slot></slot>
    </div>
</template>
```

Et dans le composant parent :

```html
<base-icon name="users">
  {{ event.attendees.length }} attending
</base-icon>
```





## `Named Slot`

ON peut avoir plusieurs `slots` dans un composant.

Pour que `Vue` puisse les différencier, il faut leur donner un nom.

<img src="assets/Screenshot2020-11-06at14.52.18.png" alt="Screenshot 2020-11-06 at 14.52.18" style="zoom:25%;" />

On est pas obligé de nommé le deuxième `slot` car il devient évident.

<img src="assets/Screenshot2020-11-06at14.54.31.png" alt="Screenshot 2020-11-06 at 14.54.31" style="zoom:25%;" />

Pour passer plusieurs éléments à un `slot`, on utilise le tag `<template>` :

<img src="assets/Screenshot2020-11-06at14.55.54.png" alt="Screenshot 2020-11-06 at 14.55.54" style="zoom:25%;" />

Après tout un composant est simplement un code de `template`.

Le `<template>` évite de surcharger le `DOM` avec des `<div>` ou des `span` inutiles.
