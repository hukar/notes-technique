# 03 Strucuture d'une application `Blazor`

## `App.razor`

C'est le routeur.

C'est à cet endroit qu'on pourra ajouter des éléments pour le logging et la sécurité.



## `index.html`

```html
<script src="_framework/blazor.webassembly.js"></script>
```

Ce script sert à charger les `dll` du projet `Blazor`.



## `_Imports.razor`

Permet de réunir en un seul fichier tous les imports utilisés dans les composants.