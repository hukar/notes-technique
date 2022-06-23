# 03 Insérer des `documents`

Les méthodes d'insertion sont des méthodes de `collections`.

La différence entre ces méthodes est le nombre de `documents` admis et le type de valeur de retour.

## `db.<collection>.insert(<object> or <array of object>)`

### Un objet

```js
PRIMARY> db.first.insert({name:"toto",age: NumberInt(38)})
WriteResult({ "nInserted" : 1 })
```

Pour un objet inséré, retourne un objet de type `WriteResult` avec le nombre d'insertions `nInserted: 1`.

### Un tableau d'objets

```js
PRIMARY> db.first.insert([{name:"edouardo"},{species:"horse"},{color:"blue"}])
BulkWriteResult({
	"writeErrors" : [ ],
	"writeConcernErrors" : [ ],
	"nInserted" : 3,
	"nUpserted" : 0,
	"nMatched" : 0,
	"nModified" : 0,
	"nRemoved" : 0,
	"upserted" : [ ]
})
```

Ici on obtient en retour un objet de type `BulkWriteResult`, on y retrouve le nombre d'insertions `nInserted: 3`.

## `db.<collection>.insertOne(<object)`

```js
PRIMARY> db.first.insertOne({name:"Luna",specie:"Unicorn",color:"blue"})
{
	"acknowledged" : true,
	"insertedId" : ObjectId("5e7cb5156368bdfef443e4b3")
}
```

On a ici en retour l'attribut `_id` : `insertedId`.

## `db.<collection>.insertMany(<array of object>)`

```js
PRIMARY> db.first.insertMany([{one:1},{two: NumberInt(2)},{three: NumberLong(3)}])
{
	"acknowledged" : true,
	"insertedIds" : [
		ObjectId("5e7cb5a76368bdfef443e4b4"),
		ObjectId("5e7cb5a76368bdfef443e4b5"),
		ObjectId("5e7cb5a76368bdfef443e4b6")
	]
}
```

On reçoit ici un tableau d'attribut `_id` : `insertedIds`.

## `db.<collection>.find()`

Affiche tous les documents de la collection :

```js
PRIMARY> db.first.find()
{ "_id" : ObjectId("5e7cb3bb6368bdfef443e4af"), "name" : "toto", "age" : 38 }
{ "_id" : ObjectId("5e7cb3f66368bdfef443e4b0"), "name" : "edouardo" }
{ "_id" : ObjectId("5e7cb3f66368bdfef443e4b1"), "species" : "horse" }
{ "_id" : ObjectId("5e7cb3f66368bdfef443e4b2"), "color" : "blue" }
{ "_id" : ObjectId("5e7cb5156368bdfef443e4b3"), "name" : "Luna", "specie" : "Unicorn", "color" : "blue" }
{ "_id" : ObjectId("5e7cb5a76368bdfef443e4b4"), "one" : 1 }
{ "_id" : ObjectId("5e7cb5a76368bdfef443e4b5"), "two" : 2 }
{ "_id" : ObjectId("5e7cb5a76368bdfef443e4b6"), "three" : NumberLong(3) }
```

Que se passe-t-il si on insère deux enregistrement avec le même `_id` :

```js
PRIMARY> db.first.insertOne({"_id":"12345"})
{ "acknowledged" : true, "insertedId" : "12345" }

PRIMARY> db.first.insertOne({"_id":"12345"})
2020-03-26T15:08:41.394+0100 E  QUERY    [js] WriteError({ // ...
```

Une erreur est levée :

```js
"errmsg" : "E11000 duplicate key error collection: myDb.first index: _id_ dup key: { _id: \"12345\" }"
```

### `duplicate key error`

### avec un opérateur de query `$exists`

```js
PRIMARY> db.first.find({name:{$exists:true}})
{ "_id" : ObjectId("5e7cc0206368bdfef443e4ba"), "name" : "toto" }
```



## Supprimer un document

### `db.<collection>.remove(<query object>)`

```js
PRIMARY> db.first.remove({name:"Luna"})
WriteResult({ "nRemoved" : 1 })
```

Pour supprimer tous les documents, il suffit de passer un objet vide :

```js
PRIMARY> db.first.remove({})
WriteResult({ "nRemoved" : 17 })
```

```js
PRIMARY> db.first.find() // plus rien
```

