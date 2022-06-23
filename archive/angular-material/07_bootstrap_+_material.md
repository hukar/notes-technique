# 07 Bootstrap + Material Angular

1- créer deux fichier `src/_reset.scss` et `src/_variables`.

Dans  `src/_reset.scss` 

```scss
* {
  &:active,
  :focus {
    outline: none !important; // 1
  }
}

// label {
//   margin-bottom: 0; // 2
// }


// a:not(.mat-button):not(.mat-raised-button):not(.mat-fab):not(.mat-mini-fab):not([mat-list-item]) {
//   color: #3f51b5; // 3
// }

```

Supprime la ligne autour des boutons et input.

Le reste à voire !?

dans  `src/_variables`

```scss
$link-color: #3f51b5; // $mat-indigo
$link-hover-color: currentColor;
$link-hover-decoration: none;
```

On override la couleur des lien avec la couleur mat-primary ici indigo

Dans `src/styles.scss`

```scss
/* You can add global styles to this file, and also import other style files */
// @import "./node_modules/bootstrap/scss/bootstrap.scss";

// import Angular Material theme
// @import "~@angular/material/prebuilt-themes/indigo-pink.css";

// Imports functions, variables, and mixins that are needed by other Bootstrap files
// my override variable
@import "variables";

@import "./node_modules/bootstrap/scss/functions";
@import "./node_modules/bootstrap/scss/variables";
@import "./node_modules/bootstrap/scss/mixins";
// Import Reboot
@import "./node_modules/bootstrap/scss/reboot";
@import "./node_modules/bootstrap/scss/grid";
@import "./node_modules/bootstrap/scss/utilities"; // add css utilities

// my reset
@import "reset";

```

On importe les `_variables` en premier car celles de Bootstrap portent toutes le mot clé `!default` et sont donc overrider.

`_reset`  lui vient bien à la fin de la cascade classique (override classique).

