# 01 Mongo et Promise

## installation de `robo 3T`

```js
db.version()
```

Retourne la version du `Mongo Shell`.



## Base de connexion à `Mongo DB`

```js
const { MongoClient } = require('mongodb')

const uri = 'mongodb://localhost:27017'
const options = { useUnifiedTopology: true }

const client = new MongoClient(uri, options)

async function run() {
    try {
        await client.connect()
      
        // test the connection
        await client.db('admin').command({ ping: 1 })
     		console.log('connected successfully 🌈')
    } finally {
        await client.close()
    }
}

run().catch((err) => {
    console.log('connection failed 💥\n')
    console.dir(err)
})
```



## Les `logs` de `mongo db`

```bash
find / -name mongo.log
```

Si `Mongo` a été installé avec `brew` on trouve :

```bash
/usr/local/var/log/mongodb/
```

Pour ouvrir le dossier :

```bash
cd /usr/local/var/log/mongodb/
open . -a finder
```



On voie que lorsqu'on ouvre une connection avec `client.connect()`, c'est en fait deux connections qui sont réellement ouvertes : `connectionId: 8` `connectionId: 9`

```bash
{"t":{"$date":"2021-03-10T10:05:55.955+01:00"},"s":"I",  "c":"NETWORK",  "id":22943,   "ctx":"listener","msg":"Connection accepted","attr":{"remote":"127.0.0.1:50404","connectionId":8,"connectionCount":1}}
{"t":{"$date":"2021-03-10T10:05:55.960+01:00"},"s":"I",  "c":"NETWORK",  "id":51800,   "ctx":"conn8","msg":"client metadata","attr":{"remote":"127.0.0.1:50404","client":"conn8","doc":{"driver":{"name":"nodejs","version":"3.6.4"},"os":{"type":"Darwin","name":"darwin","architecture":"x64","version":"19.6.0"},"platform":"'Node.js v14.15.5, LE (unified)"}}}
{"t":{"$date":"2021-03-10T10:05:55.965+01:00"},"s":"I",  "c":"NETWORK",  "id":22943,   "ctx":"listener","msg":"Connection accepted","attr":{"remote":"127.0.0.1:50405","connectionId":9,"connectionCount":2}}
{"t":{"$date":"2021-03-10T10:05:55.966+01:00"},"s":"I",  "c":"NETWORK",  "id":51800,   "ctx":"conn9","msg":"client metadata","attr":{"remote":"127.0.0.1:50405","client":"conn9","doc":{"driver":{"name":"nodejs","version":"3.6.4"},"os":{"type":"Darwin","name":"darwin","architecture":"x64","version":"19.6.0"},"platform":"'Node.js v14.15.5, LE (unified)"}}}
{"t":{"$date":"2021-03-10T10:05:55.970+01:00"},"s":"I",  "c":"NETWORK",  "id":22944,   "ctx":"conn8","msg":"Connection ended","attr":{"remote":"127.0.0.1:50404","connectionId":8,"connectionCount":1}}
{"t":{"$date":"2021-03-10T10:05:55.971+01:00"},"s":"I",  "c":"NETWORK",  "id":22944,   "ctx":"conn9","msg":"Connection ended","attr":{"remote":"127.0.0.1:50405","connectionId":9,"connectionCount":0}}

```



## `insertOne(<document>)`

```js
// insert user
const usersCollection = client.db(dbName).collection('users')

const result = await usersCollection.insertOne({
  name: 'Kokusaï',
  age: 37,
})
console.dir(result.ops)
```
```bash
connected successfully 🌈
[
  {
    name: 'Kokusaï',
    age: 37,
    _id: ObjectID { _bsontype: 'ObjectID', id: [Buffer [Uint8Array]] }
  }
]
```

#### ! `result.ops` renvoie un tableau.

C'est une opération asynchrone, les erreurs sont gérées de manière globale par :

```js
run().catch((err) => {
    console.log('connection failed 💥\n')
    console.dir(err)
})
```



## `insertMany(<arrayOfDocument>)`

```js
const result = await usersCollection.insertMany([
  {
    name: 'Vitor',
    age: 56,
  },
  {
    name: 'Pola',
    age: 59,
  },
])
console.dir(result)
```

```bash
connected successfully 🌈
{
  result: { ok: 1, n: 2 },
  ops: [
    { name: 'Vitor', age: 56, _id: [ObjectID] },
    { name: 'Pola', age: 59, _id: [ObjectID] }
  ],
  insertedCount: 2,
  insertedIds: {
    '0': ObjectID { _bsontype: 'ObjectID', id: [Buffer [Uint8Array]] },
    '1': ObjectID { _bsontype: 'ObjectID', id: [Buffer [Uint8Array]] }
  }
}
```



## `ObjectId`

`Mongo` utilise des `GUID` Global Unique Identifier.

Longueur de `12 Bytes`.

- `4` timestamp
- `5` valeur aléatoire
- `3` incrément un compteur dont la valeur de départ est aléatoire.

Pas de collision d'`id` possible.
on peut créer ces `id` depuis notre code :

```js
const { ObjectID } = require('mongodb')

const testObjectId = [...Array(5)].forEach((elt) => console.log(ObjectID()))
```

```bash
6048a7820ea402073a4ad901
6048a7820ea402073a4ad902
6048a7820ea402073a4ad903
6048a7820ea402073a4ad904
6048a7820ea402073a4ad905
```

### Récupérer le `timestamp`

```js
const id = ObjectID()
const timestamp = id.getTimestamp()
console.log(id, ' : ', timestamp)
```

```bash
6048aad8ea31f007730d0d55  :  2021-03-10T11:17:44.000Z

6048aae4a95fb70777e11b20  :  2021-03-10T11:17:56.000Z
```

Toujours avec une heure de décalage : `Z` = temps universel `utc`.

### Convertir en date locale

```js
const date = Date(timestamp)
console.log(date)
```

#### ! ne fonctionne pas si on utilise `new` devant `Date` !?



## `findOne(<filter>)`

```js
const { MongoClient, ObjectID } = require('mongodb')

const uri = 'mongodb://localhost:27017'
const options = { useUnifiedTopology: true }

const client = new MongoClient(uri, options)
const dbName = 'task-manager'

async function run() {
    try {
        await client.connect()

        // test the connection
        await client.db('admin').command({ ping: 1 })
        console.log('connected successfully 🌈')

        const usersCollection = client.db(dbName).collection('users')

        const user = await usersCollection.findOne({ name: 'Karla' })
        console.log(user)
    } finally {
        await client.close()
    }
}

run().catch((err) => {
    console.log('connection failed 💥\n')
    console.dir(err)
})
```

```bash
connected successfully 🌈
{ _id: 6048d3fbe4065e09139de66a, name: 'Karla', age: 29 }
```

Si le filtre ne correspond à aucun document, la requête renvoie `null` mais ne provoque pas d'erreur :

#### ! si pas de document `Mongo` renvoie `null` mais ne génère pas d'erreur.

```js
const user = await usersCollection.findOne({ name: 'Karla', age: 11 })
console.log(user)
```

```bash
connected successfully 🌈
null
```

### Rechercher par l'`_id`

```js
const user = await usersCollection.findOne({ _id: '6048d3fbe4065e09139de66a' })
console.log(user)
```

```bash
connected successfully 🌈
null
```

L'`_id` pour `Mongo DB` doit être un objet pas une chaîne de caractère, on utilise `ObjectID` pour corriger le problème :

```js
const user = await usersCollection.findOne({ _id: new ObjectID('6048d3fbe4065e09139de66a') })
// ou bien sans le new
// const user = await usersCollection.findOne({ _id: ObjectID('6048d3fbe4065e09139de66a') })
console.log(user)
```

```bash
connected successfully 🌈
{ _id: 6048d3fbe4065e09139de66a, name: 'Karla', age: 29 }
```

#### ! si on veut rechercher par l'`_id` on doit fournir un `ObjectID`.

### retrouver le dernier document `option: sort`

```js
const options = {
  sort: {
  	_id: -1,
  },
}

const lastTask = await tasksCollection.findOne({}, options)
console.log(lastTask)
```

`-1` du plus récent au plus vieux.

## `find(<filter>)`

Pour récupérer plusieurs documents on utilise `find`.

`find` renvoie un `cursor`, un pointeur sur le résultat en base de données.

La méthode `toArray` de `cursor` renvoie les documents trouvés sous forme d'un tableau :

```js
const cursor = usersCollection.find({ age: { $gte: 40 } })
const arrayUsers = await cursor.toArray()
console.log(arrayUsers)
```

```bash
[
  { _id: 6048940cb0a29405bc870638, name: 'hukar', age: 45 },
  { _id: 'titi_koko', name: 'hukar', age: 45 },
  { _id: 60489a528d99f9064e4de2b6, name: 'Vitor', age: 56 },
  { _id: 60489a528d99f9064e4de2b7, name: 'Pola', age: 59 }
]
```



On peut limiter le nombre de documents renvoyé avec `limit`.

```js
const arrayUsers = await cursor.limit(3).toArray()
```

```bash
[
  { _id: 6048940cb0a29405bc870638, name: 'hukar', age: 45 },
  { _id: 'titi_koko', name: 'hukar', age: 45 },
  { _id: 60489a528d99f9064e4de2b6, name: 'Vitor', age: 56 }
]
```

Et on peut paginer avec `skip` :

```js
let nbTotalUser = 0
let cursor = usersCollection
	.find({ age: { $gte: 10 } })
	.limit(2)
	.skip(nbTotalUser)
let arrayUsers = await cursor.toArray()
console.log(arrayUsers)
nbTotalUser += 2

cursor = usersCollection
  .find({ age: { $gte: 10 } })
  .limit(2)
  .skip(nbTotalUser)
arrayUsers = await cursor.toArray()
console.log(arrayUsers)
nbTotalUser += 2

cursor = usersCollection
  .find({ age: { $gte: 10 } })
  .limit(2)
  .skip(nbTotalUser)
arrayUsers = await cursor.toArray()
console.log(arrayUsers)
```

```bash
[
  { _id: 6048940cb0a29405bc870638, name: 'hukar', age: 45 },
  { _id: 'titi_koko', name: 'hukar', age: 45 }
]
[
  { _id: 604895b5a92c4e05e40ce4df, name: 'Kokusaï', age: 37 },
  { _id: 60489a528d99f9064e4de2b6, name: 'Vitor', age: 56 }
]
[
  { _id: 60489a528d99f9064e4de2b7, name: 'Pola', age: 59 },
  { _id: 6048d3fbe4065e09139de66a, name: 'Karla', age: 29 }
]
```

### Connaitre le nombre de documents trouvés `cursor.count()`

```js
console.log(await cursor.count())
```

```bash
6
```



## `Promise` vocabulaire

```js
// 													fulfilled
// 												/
// Promise	-- pending --> 
// 												\
// 													rejected
```

Les `Promise` on des mécanismes pour ne pas pouvoir exécuter plusiseurs `resolve` ou `reject`.

```js
const doWorkPromise = new Promise((resolve, reject) => {
    setTimeout(() => {
        reject('aye aye aye')
    }, 2000)
})

async function run() {
    const result = await doWorkPromise
}

run().catch((error) => {
    console.log('ERROR:error:PROMISE')
    console.log(error)
})
```

Le meilleur des deux mondes : `await` plutôt que `.then()` par contre `.catch()` plutôt qu'un bloc `try & catch`.

`await` ne peut apparaître que dans une fonction `async`.

## Mettre à jour un document `updateOne` et `updateMany`

#### ! avec `updateOne`, on ne peut pas remplacer un `id`

```js
const users = client.db(dbName).collection('users')

const filter = { _id: 'titi_koko' }
const updateDoc = { $set: { _id: new ObjectID(), name: 'Roberta Ice' } }

const result = await users.updateOne(filter, updateDoc)
```

```bash
MongoError: Performing an update on the path '_id' would modify the immutable field '_id'
```

```js
const filter = { _id: 'titi_koko' }
const updateDoc = { $set: { name: 'Roberta Fire' } }

const result = await users.updateOne(filter, updateDoc)
console.log('modified :', result.modifiedCount, ' matched :', result.matchedCount)
```

```bash
modified : 1  matched : 1
```

On utilise l'opérateur `$set` pour modifier la valeur d'un champs.

### Opérateur `$inc`

Incrémente un champs.

```js
const filter = { _id: 'titi_koko' }
const updateDoc = { $inc: { age: 3 } }

const result = await users.updateOne(filter, updateDoc)
```

L'age passe de `45` valeur de départ à `48`.

On peut aussi décrémenter avec `$inc` :

```js
const filter = { _id: 'titi_koko' }
const updateDoc = { $inc: { age: -5 } }

const result = await users.updateOne(filter, updateDoc)
```



### `updateMany`

```js
const tasks = client.db(dbName).collection('tasks')

const filter = { completed: false }
const updateDoc = { $set: { completed: true } }

const result = await tasks.updateMany(filter, updateDoc)
console.log('modified :', result.modifiedCount, ' matched :', result.matchedCount)
```

```bash
modified : 2  matched : 2
```

Toutes mes tâches sont maintenant à `true`.



## `deleteOne` et `deleteMany`

```js
const users = client.db(dbName).collection('users')

const filter = { age: { $gt: 30 } }

const result = await users.deleteMany(filter)
console.log(result.deletedCount)
```

```bash
4
```



```js
const tasks = client.db(dbName).collection('tasks')

const filter = { description: 'Acheter des œufs 🥚🥚' }

const result = await tasks.deleteOne(filter)
```

