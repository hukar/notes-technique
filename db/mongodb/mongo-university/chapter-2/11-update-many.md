# 11 `updateMany`

## Supprimer un champ `$unset`

```js
db.movieDetails.updateMany({ rated: null },
    { $unset: { rated: "" } }
)
```

On supprime le champ `rated` lorsque celui ci est null.

`rated: ""` la chaîne vide n'a pas d'incidence.

