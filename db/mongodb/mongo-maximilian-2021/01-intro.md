# 01 Introduction

## Installation

Voire doc officiel avec `brew`

## Commandes `mongo shell`


### `cls`
Nettoie l'écran.
```bash
> cls
```




### `show dbs`
Liste les base de données.
```bash
> show dbs
admin   0.000GB
config  0.000GB
local   0.000GB
```



### `show collections`

Liste les `collections` de la `database`.

```js
> use flights
switched to db flights
> show collections
flightData
passengers
```



### `use dbName`
Choisie une base de données pour les requêtes suivante, crée la base si elle n'existe pas.
```bash
> use shop
switched to db shop
```




### `inserOne(<document>)`
Ajoute un document au `container`: `products`.
```bash
> db.products.insertOne({name: "Bycicle", price: 567.99})
{
	"acknowledged" : true,
	"insertedId" : ObjectId("6034b7e877edb238d9ecc6eb")
}
```




### `find()`
Retrouve les documents du `container` `products`.
```bash
> db.products.find()
{ "_id" : ObjectId("6034b7e877edb238d9ecc6eb"), "name" : "Bycicle", "price" : 567.99 }
```




### `pretty()`
Indente le résultat de `find`.
```bash
> db.products.find().pretty()
{
	"_id" : ObjectId("6034b7e877edb238d9ecc6eb"),
	"name" : "Bycicle",
	"price" : 567.99
}
```



```bash
> db.products.insertOne({name: "knife", price: 78.99, brand: "Switzer"})
{
	"acknowledged" : true,
	"insertedId" : ObjectId("6034b82f77edb238d9ecc6ec")
}
> db.products.find().pretty()
{
	"_id" : ObjectId("6034b7e877edb238d9ecc6eb"),
	"name" : "Bycicle",
	"price" : 567.99
}
{
	"_id" : ObjectId("6034b82f77edb238d9ecc6ec"),
	"name" : "knife",
	"price" : 78.99,
	"brand" : "Switzer"
}
```

### Remarques

- On n'est pas obligé de mettre des guillemets aux noms des `key` (comme en `javascript`)
- Les guillemets sont ajoutés automatiquement aux documents (comme en `json`)
- Il n'y a pas de schéma.



## `Mongo DB` sous le capot

<img src="assets/mongo-db-under-scene.png" alt="mongo-db-under-scene" style="zoom:50%;" />

Pour plus d'efficacité `Mongo Db` peut stocker des données directemennt dans la mémoire.

`Mongo DB` utilise un `storage engine` : `WiredTiger`.

