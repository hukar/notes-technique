# 02 Créer et Supprimer une `bdd`

Il n'y a pas de commande pour créer une nouvelle `bdd`.

## Changer de `bdd` active : `use <database name>`

D'abord on spécifie une `bdd` active avec `use`:

```js
use ma_nouvelle_bdd
```

Pour le moment la base de données n'a pas vraiment encore été créée, elle le sera à la création de la première `collection`.

## Créer une collection : `db.createCollection("<collection name>")`

```js
db.createCollection("ma_nouvelle_collection")
```

## Lister les collections : `show collections`

```js
show collections
```

## Supprimer une collection : 

### `db.getCollection("<collection name>").drop()`

### `db.collection_name.drop()`

Il vaut mieux la première (?).

```js
db.getCollection("ma_nouvelle_collection").drop()
```

## Supprimer une `bdd` : `db.dropDatabase()`

Supprime la `bdd` active.

```js
db.dropDatabase()
```



## Exemple

```js
PRIMARY> use titi
switched to db titi

PRIMARY> db.createCollection("grosMinet")
{
	"ok" : 1,
	"$clusterTime" : {
		"clusterTime" : Timestamp(1585217222, 1),
		"signature" : {
			"hash" : BinData(0,"WFEVgr48d5FFzj1vFtkT/7gk4Uc="),
			"keyId" : NumberLong("6806055642554433538")
		}
	},
	"operationTime" : Timestamp(1585217222, 1)
}

PRIMARY> show dbs
admin  0.000GB
local  1.474GB
titi   0.000GB

PRIMARY> show collections
grosMinet
```

```js
PRIMARY> db.getCollection("grosMinet").drop()
true
PRIMARY> show collections

PRIMARY> db.dropDatabase()
{
	"dropped" : "titi",
	"ok" : 1,
	"$clusterTime" : {
		"clusterTime" : Timestamp(1585217364, 1),
		"signature" : {
			"hash" : BinData(0,"eoYQQNcTtRGVRU7OJK0PXQBT1zk="),
			"keyId" : NumberLong("6806055642554433538")
		}
	},
	"operationTime" : Timestamp(1585217364, 1)
}

PRIMARY> db
titi
PRIMARY> show dbs
admin  0.000GB
local  1.474GB
```

## Remarque

```js
PRIMARY> use anotherDb
switched to db anotherDb
```

```js
PRIMARY> show collections
a
b
c
```

On peut utiliser du javascript `ES6` dans le `mongo shell`.

```js
PRIMARY> ["a","b","c"].forEach(val => db.getCollection(val).drop())
PRIMARY> show collections
```

Plus rien.

```js
PRIMARY> show dbs
admin    0.000GB
local    1.474GB
myDb     0.000GB
thirdDb  0.000GB
```

Supprimer toutes les collections d'une `bdd` la supprime par la même occasion.

