# 13 `updateOne`

On peut utiliser le `SHELL` comme un interpréteur javascript.

```bash
PRIMARY> let filter = { title: "Star Wars: Episode III - Revenge of the Sith" };
PRIMARY> let doc = db.movieDetails.findOne(filter);
```

On créé un objet filtre et on charge dans une variable un document de `movieDetails`

```bash
PRIMARY> doc.type;
movie
PRIMARY> doc.type="masterpiece";
masterpiece
```

On sait voire et modifier les propriétés de ce document.

```bash
PRIMARY> doc.genres;
[ "Action", "Adventure", "Fantasy" ]
PRIMARY> doc.genres.push("Space Opera");
4
PRIMARY> doc.genres;
[ "Action", "Adventure", "Fantasy", "Space Opera" ]
```

On peut utiliser une fonction javascript `push`, elle retourne le nombre d'élément `4`.

```bash
PRIMARY> db.movieDetails.replaceOne(filter,doc);
{ "acknowledged" : true, "matchedCount" : 1, "modifiedCount" : 1 }
```

Enfin on remplace l'élément dans la base de données.

