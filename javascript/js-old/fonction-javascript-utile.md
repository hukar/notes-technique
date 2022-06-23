# Les fonctions utiles
---
### removeAttribute()

une fonction pour enlever tout les attributs checked au reset du formulaire

```javascript
formFilter.onreset = function () {
  for (var i = 0;i < checkboxTab.length;i++) {
    var checkbox = checkboxTab[i]
    checkbox.removeAttribute('checked')
  }
}
```
récupérer un formulaire avec son attribut **name**

```html
<form name="formFilter" action="{{ linkSearch }}" method="post">
```
le javascript maintenant
```javascript
var formFilter = document.formFilter```
