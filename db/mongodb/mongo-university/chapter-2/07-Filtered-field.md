# 07 Filtrer sur les champs

## Dans `Compass`

![Screenshot 2020-02-10 at 14.45.58](assets/Screenshot 2020-02-10 at 14.45.58.png)

```json
{mpaaRating: "NOT RATED", year: 1995}
```

On peut voire qu'on récupère 61(sur 963534) documents, les filtres sont additif (`AND` logique).

## Dans le `SHELL`

On utilise `find({ property: value })`

```bash
db.movies.find({mpaaRating: 'Not Rated',viewerRating: {$gte: 3,$lt: 3.5}})
```

Pour un interval : `{$gte: 3,$lt: 3.5}`

`$gte` plus grand ou égal que ...

`$lt` plus petit que ...

## Documents imbriqués

Notation avec `.`

![Screenshot 2020-02-10 at 15.24.58](assets/Screenshot 2020-02-10 at 15.24.58.png)

```json
{"wind.direction.angle": 290}
```

Attention les guillemets sont obligatoires.

en ligne de commande :

```bash
PRIMARY> db.data.find({"wind.direction.angle": 290})
```

## Compter les enregistrements dans le `SHELL` : `count()`

```bash
PRIMARY> db.movieDetails.find({"awards.wins":2,"awards.nominations":2}).count()
12
```

## Array Field

Filtrer un tableau :

```bash
PRIMARY> db.movies.find({"cast": ["Charles Kayser","John Ott"]}).pretty()

{
	"_id" : ObjectId("58c59c6a99d4ee0af9e0c328"),
	"title" : "Blacksmith Scene",
	"year" : 1893,
	"imdbId" : "tt0000005",
	"mpaaRating" : "UNRATED",
	"genre" : "Short",
	"viewerRating" : 6.2,
	"viewerVotes" : 1189,
	"runtime" : 1,
	"director" : "William K.L. Dickson",
	"cast" : [
		"Charles Kayser",
		"John Ott"
	],
	"plot" : "Three men hammer on an anvil and pass a bottle of beer around."
}
```

En passant un tableau de valeurs, on ne **match** qu'avec un tableau identique.

Si on veut tous les films avec `"Mia Farrow"` , on fait :

```bash
PRIMARY> db.movies.find({"cast": "Mia Farrow"}).pretty()
```

Et là on obtient les films dont le tableau `cast` contient `"Mia Farrow"`.

### On peut spécifier à quelle place on veut que la recherche se situe :

![Screenshot 2020-02-10 at 15.49.46](assets/Screenshot 2020-02-10 at 15.49.46.png)

```json
{"cast.0": "Mia Farrow"}
```

On cherche `"Mia Farrow"` à l'indice 0 uniquement : 21 résultats.

![Screenshot 2020-02-10 at 15.49.59](assets/Screenshot 2020-02-10 at 15.49.59.png)

Quelque soit l'indice on a 58 résultats.