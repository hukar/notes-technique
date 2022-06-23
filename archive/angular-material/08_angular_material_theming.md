# 08 Angular Material theming

On crée `src/my-theme.scss`

```scss
@import '~@angular/material/theming';
// Plus imports for other components in your app.

// Include the common styles for Angular Material. We include this here so that you only
// have to load a single css file for Angular Material in your app.
// **Be sure that you only ever include this mixin once!**
@include mat-core();

// Define the default theme (same as the example above).
$spf-primary: mat-palette($mat-light-blue);
$spf-accent:  mat-palette($mat-pink, 800, A100, A400);
// The warn palette is optional (defaults to red).
$spf-warn:    mat-palette($mat-amber);

$spf-theme:   mat-light-theme($spf-primary, $spf-accent, $spf-warn);

// Include the default theme styles.
@include angular-material-theme($spf-theme);


// Define an alternate dark theme.
$dark-primary: mat-palette($mat-blue-grey);
$dark-accent:  mat-palette($mat-amber, A200, A100, A400);
$dark-warn:    mat-palette($mat-deep-orange);
$dark-theme:   mat-dark-theme($dark-primary, $dark-accent, $dark-warn);

// Include the alternative theme styles inside of a block with a CSS class. You can make this
// CSS class whatever you want. In this example, any component inside of an element with
// `.unicorn-dark-theme` will be affected by this alternate dark theme instead of the default theme.
.unicorn-dark-theme {
  @include angular-material-theme($dark-theme);
}
```

On le link dans `angular.json`

```json
"styles": [
              "src/styles.scss",
              "src/my-theme.scss"
            ],
```

