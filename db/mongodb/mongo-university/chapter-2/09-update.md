# 09 Mise à jour d'un document

## Mise à jour du premier trouvé `updateOne`

```js
db.movieDetails.updateOne({
  title: "The Martian"
}, {
  $set: {
    poster: "http://ia.media-imdb.com/images/..."
  }
})
```

d'abord le filtre, puis le `setter`.

```bash
db.movieDetails.updateOne({title: "Wild Wild West"},{$set: {"year": 2045}})
```

```bash
{ "acknowledged" : true, "matchedCount" : 1, "modifiedCount" : 1 }
```

## Modifier un document imbriqué

```bash
db.movieDetails.updateOne({title: "Wild Wild West"},{$set: {
	awards:{
      fruit: "banana",
      dummies: 456,
      monkeys:13
      }
		}
	})
```

## `$SET`

Remplace ou ajoute le champ spécifié :

```bash
PRIMARY> db.movieDetails.updateOne({title: "Wild Wild West"},{$set: {tintin: "milou"}})
{ "acknowledged" : true, "matchedCount" : 1, "modifiedCount" : 1 }
```

Ici j'ajoute un champ `tintin` qui n'existait pas avant.

