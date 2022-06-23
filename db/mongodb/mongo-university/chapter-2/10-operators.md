# 10 Les opérateurs

## `$inc`

Ajouter une valeur à un champ numérique :

```bash
db.movieDetails.updateOne({title:"Wild Wild West"},{$inc: {"tomato.rewiews": 33, "tomato.userMeter": 33}})
```

## `$push` , `addToSet` et `$each`

```js
db.movieDetails.updateOne(
    { title: "A Million Ways to Die in the West" },
    {
        $push: {
            score: { $each: [56, 67, 68] }
        }
    }
)
```

Si on veut que les valeurs déjà dans le tableau soit ignorée : `$addToSet`

```js
db.movieDetails.updateOne(
    { title: "A Million Ways to Die in the West" },
    {
        $addToSet: {
            score: { $each: [56, 67, 69, 70] }
        }
    }
)
```

## `$pull` retirer un élement (ou plusieurs)

```js
db.movieDetails.update({},{
    $pull: {
        writers: {$in : ["Sergio Leone", "Sergio Donati"]},
        genres: "Western"}   
})
```

`$in: [value1, value2]` permet de spécifier plusieurs valeurs.

