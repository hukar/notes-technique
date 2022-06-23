# 04 Charger des données

## Voire ses Base de données `show dbs`

```bash
PRIMARY> show dbs
admin  0.000GB
local  3.796GB
```

## Quitter le mongo shell `quit()` ou `exit`

```bash
PRIMARY> quit()
kar : ~ $ 
```

## Charger des données `load()`

Voici le script de données `loadMovieDetailsDataset.js`

```js
db = db.getSiblingDB("video");
db.movieDetails.drop();
db.movieDetails.insertMany([
    // .... toutes les données .......
]);
```

On lance le mongo shell dans le même répertoire que notre script de données :

```bahs
$ mongo "mongodb+srv://sandbox-zvyhz.mongodb.net/test"  --username m001-student --password m001-mongodb-basics
```

On charge le script :

```bash
PRIMARY> load("loadMovieDetailsDataset.js")
true
```

## Voire ce qu'on a chargé

```bash
PRIMARY> show dbs
admin  0.000GB
local  3.796GB
video  0.001GB

PRIMARY> use video
switched to db video

PRIMARY> show collections
movieDetails
```

```bash
PRIMARY> db.movieDetails.find().pretty()
{
	"_id" : ObjectId("5e3eccb90c3e01731150952c"),
	"title" : "Once Upon a Time in the West",
	"year" : 1968,
	"rated" : "PG-13",
	"runtime" : 175,
	"countries" : [
		"Italy",
		"USA",
		"Spain"
```

`find()` trouve tout les enregistrement

`pretty()` affiche les enregistrement bien indenté et présenté.

