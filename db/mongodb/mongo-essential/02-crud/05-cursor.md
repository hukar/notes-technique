# 05 `cursor`

Le `cursor` est renvoyé par la méthode `find`.

## Trouver la taille de `batch size`

```js
let cursor = db.<collection>.find().batchSize(10)
```

```js
PRIMARY> cursor.next()
{ "_id" : ObjectId("5e7db2f0745c1d888f7043ee"), "index" : 1 }

PRIMARY> cursor.objsLeftInBatch()
9
```

#### `cursor.next()` renvoie un seul élément: le prochain

#### `cursor.objsLeftInBatch()` donne le nombre d'objets restant dans le lot `batch`

On déduit donc que le `batch size` est de `9 + 1 = 10`.

## Valeur par défaut

La valeur appliqué de `batch size` est automatiquement remise à `101` à chaque nouvelle connexion :

```js
PRIMARY> let cursor = db.cursor.find()
PRIMARY> cursor.objsLeftInBatch()
101
```

On voit ici cette valeur de `101`.

Cette valeur est remise à `101` à chaque nouvel appelle à `find`.

```js
PRIMARY> cursor = db.cursor.find()
{ "_id" : ObjectId("5e7db2f0745c1d888f7043ee"), "index" : 1 }
{ "_id" : ObjectId("5e7db2f0745c1d888f7043ef"), "index" : 2 }
// ...
{ "_id" : ObjectId("5e7db2f1745c1d888f704401"), "index" : 20 }
Type "it" for more
MongoDB Enterprise HukarCluster-shard-0:PRIMARY> cursor.objsLeftInBatch()
81
```

`81 + 20 = 101`.

## vérifier s'il reste encore un document : `hasNext`

```js
PRIMARY> cursor.next()
{ "_id" : ObjectId("5e7db2f1745c1d888f704402"), "index" : 21 }

PRIMARY> cursor.hasNext()
true
```

 

## aller à la fin

```js
PRIMARY> let cursor = db.cursor.find()

PRIMARY> while(cursor.hasNext()) cursor.next()
{ "_id" : ObjectId("5e7dd82de4bc260435241c31"), "index" : 150 }

PRIMARY> cursor.next()
2020-03-27T15:02:14.832+0100 E  QUERY    [js] uncaught exception: Error: error hasNext: false :
DBQuery.prototype.next@src/mongo/shell/query.js:299:15
@(shell):1:1
```

## `cursor.toArray`

Cette méthode met le contenu du `cursor` dans un tableau.

```js
> let cursor = db.cursor.find()
> cursor.toArray()
[
	{
		"_id" : ObjectId("5e8c982848ae0d686701a76f"),
		"index" : 1
	},
    // ...
    {
		"_id" : ObjectId("5e8d82d048ae0d686701a7d5"),
		"index" : 103
	}
]
```

On voit que les 103 `documents` sont dans le tableau.



## `cursor.forEach(<function>)`

Exécute un traitement sur chaque enregistrement.

```js
let cc = db.cursor.find()
cc.forEach(printjson)

{ "_id" : ObjectId("5e8c982848ae0d686701a76f"), "index" : 1 }
{ "_id" : ObjectId("5e8c982848ae0d686701a770"), "index" : 2 }
// ...
```

`printjson` affiche au format `json` :

```js
printjson
function(x) {
    print(tojson(x));
}
```

#### Exemple avec une `custom function`

```js
let cursor = db.cursor.find()

cursor.forEach(val => print(`the index is ${val.index}`))
```

```bash
the index is 1
the index is 2
the index is 3
// ...
```

## `cursor.count()`

Compte le nombre de `documents` dans le `cursor`.

```js
cursor.count()

103
```

On peut chainer sur `find`

```js
db.cursor.find().count()
```



## `cursor.limit(<number>)`

Limite le nombre de `documents` dans le `cursor`.

```js
db.cursor.find().limit(2)
```

```js
/* 1 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a76f"),
    "index" : 1
}

/* 2 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a770"),
    "index" : 2
}
```

## `cursor.skip(<number>)`

Permet de sauter un certain nombre de `documents` dans le `cursor`.

```js
db.cursor.find().skip(5)
```

```js
/* 1 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a774"),
    "index" : 6
}
// ...
/* 50 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a7a5"),
    "index" : 55
}
```

Dans `Robo 3T` le `shellBatchSize` est fixé à 50, on reçoit donc bien 50 `documents` mais il commence au sixième. Le cinq premier ont été sauté par la méthode `.skip(5)`.



## `cursor.sort({<fielname1>: 1, <fieldname2>: -1})`

Trie les `documents` du `cursor`.

`1` ascendant.

`-1` descendant.

Par défaut les  `documents` sont trié sur `_id` et de manière ascendant (du plus petit au plus grand).

```js
db.cursor.find().sort({"index": -1})
```

```js
/* 1 */
{
    "_id" : ObjectId("5e8d82d048ae0d686701a7d5"),
    "index" : 103.0
}

/* 2 */
{
    "_id" : ObjectId("5e8d82d048ae0d686701a7d4"),
    "index" : 102.0
}
// ...
```

## Combiner `skip`, `limit` et `sort`

```js
db.cursor.find().sort({"index": -1}).skip(3).limit(3)
```

```js
/* 1 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a7d2"),
    "index" : 100
}

/* 2 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a7d1"),
    "index" : 99
}

/* 3 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a7d0"),
    "index" : 98
}
```

```js
db.cursor.find().skip(3).limit(3).sort({"index": -1})
```

Donne le même résultat que plus haut (!) on aurait pu s'attendre à :

```js
/* 1 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a774"),
    "index" : 6
}


/* 2 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a773"),
    "index" : 5
}

/* 3 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a772"),
    "index" : 4
}
```

mais non!

Toutes ces méthodes sont exécutées sur le serveur, avant que le `cursor` ne soit renvoyé.

Peut importe l'ordre d'écriture on a toujours `sort` puis `skip` puis enfin `limit`.

#### `sort -> skip -> limit`

Quelque soit l'ordre où on chaîne ces méthodes.

### Chaîner avec `count`

```js
db.cursor.find().sort({"index": -1}).limit(3).skip(3).count()
```

```js
103
```

`count` renvoie toujours le nombre de `documents` retournés par `.find()`.

Il n'est pas impacté par les méthodes `sort`, `skip` et `limit`.

## Avec `forEach`

```js
db.cursor.find().limit(2).skip(65).forEach(o => print(`Document with _id ${o._id} has index ${o.index}`))
```

```js
Document with _id ObjectId("5e8c982848ae0d686701a7b0") has index 66
Document with _id ObjectId("5e8c982848ae0d686701a7b1") has index 67
```

`forEach` se met en bout de chaîne car il ne renvoie pas un `cursor`.