# 12 `upsert`

Si on veut remplacer un document, on utilisera le comportement de `upsert`.

Il évite les doublons :

- si l'élément est trouvé, il est remplacé
- sinon il est créé

```js
db.movieDetails.updateOne(
    { title: "Turks in Space" },
    {
        $set: {
            title: "Turks in Space",
            year: 1030,
            animal: "lion"
        }
    },
    { upsert: true }

)
```

```bash
{ "acknowledged" : true, "matchedCount" : 1, "modifiedCount" : 1 }
```

Il a trouvé l'élément qui existait déjà, il l'a modifié.

```js
db.movieDetails.updateOne(
    { title: "Cochon in Space" },
    {
        $set: {
            title: "Cochon in Space",
            year: 1030,
            animal: "lion"
        }
    },
    { upsert: true }

)
```

```bash
{
	"acknowledged" : true,
	"matchedCount" : 0,
	"modifiedCount" : 0,
	"upsertedId" : ObjectId("5e42c2803fa389a010d4edcc")
}
```

Aucun trouvé, aucun modifié, **MongoDb** créé une `upsertId` et ajoute un nouvel élément dans la collection.

