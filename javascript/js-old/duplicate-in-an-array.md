# Supprimer des doublons dans un tableau

```js
const arr = ["x", "x", "a", "a", "b"];

arr.filter((x, i, arr) => arr.indexOf(x) === i); // [ 'x', 'a', 'b' ]
```

`indexOf` va chercher le premier indice de l'occurence, donc l'égalité sera fausse pour les suivantes.