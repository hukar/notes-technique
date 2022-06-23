# AA Pluralsight - Jonathan Mills - MongoDB Node js

## Utiliser `admin` pour avoir des infos `listDatabases`

```js
const { MongoClient } = require('mongodb')

const uri = 'mongodb://localhost:27017'
const dbName = 'circulation'

async function main() {
    const client = new MongoClient(uri, { useUnifiedTopology: true })
    await client.connect()

    const admin = client.db(dbName).admin()

    console.log(await admin.serverStatus())
    console.log(await admin.listDatabases())
}

main()
```

`{ useUnifiedTopology: true }` option demandée par `Mongo DB` lorsqu'on lance le script.

`serverStatus` enormément d'infos sur le serveur `Mongo DB`.

`listDatabases` la liste des `databases` :

```js
{
  databases: [
    { name: 'admin', sizeOnDisk: 40960, empty: false },
    { name: 'cinema', sizeOnDisk: 81920, empty: false },
    { name: 'circulation', sizeOnDisk: 81920, empty: false },
    { name: 'config', sizeOnDisk: 110592, empty: false },
    { name: 'local', sizeOnDisk: 40960, empty: false }
  ],
  totalSize: 356352,
  ok: 1
}
```



## Créer et charger une collection de documents `insertMany`

Les documents sont dans `circulation.json` = tableau de documents `json`.

On crée une méthode mpour charger les données : `circulationRepo.js`

```js
const { MongoClient } = require('mongodb')

const uri = 'mongodb://localhost:27017'
const dbName = 'circulation'

function circulationRepo() {
    async function loadData(data) {
        try {
            const client = new MongoClient(uri, { useUnifiedTopology: true })

            await client.connect()
            const db = client.db(dbName)

            const results = await db.collection('newspaper').insertMany(data)

            return results
        } catch (error) {
            throw error
            console.log('Data not loaded')
        }
    }

    return { loadData }
}

module.exports = circulationRepo()
```

Avec `async` et `await` on utilise un `try-catch` pour gérer les erreurs, ici l'erreur est renvoyée au niveau suivant avec `throw`.

1. On crée un client : `new MongoClient(uri, options)`
2. On connect le client : `client.connect`
3. On récupère une collection : `client.db(dbName).collection(collectionName)`
4. On travaille sur la collection : `myCollection.insertMany(data)`
5. On ferme la connexion : `client.close`

### Dans `app.js`

```js
// ...

const circulationRepo = require('./repos/circulationRepo')
const data = require('./circulation.json')

// ...

try {
  const result = await circulationRepo.loadData(data)
  console.log(result)
} catch (error) {
  console.log('Something wrong happened!!')
}
```



## Effacer une database : `dropDatabase`

###  `await client.db(dbName).dropDatabase()`



## Fermer le client `client.close()`

Pour que le script `node js` s'arrête, il faut fermer le client à la fin :

```js
const client = new MongoClient()

await client.connect(uri, { useUnifiedTopology: true })

// ... Traitement des données

client.close()
```



## Gestion des erreurs

On utilise `assert` pour tester le code :

```js
const asser = require('assert')

// ...

assert.strictEqual(data.length, getData.length)
```

Une `assertion` va générer une erreur et la partie nettoyage du code ne sera pas exécuter (une exception arrête le code).

#### `assert.strictEqual(a,b)` et `assert.notStrictEqual(a,b)`

Pour résoudre ce problème on utilise `try-catch-finally` :

```js
// on définit le client à l'extérieur du bloc
const client = new MongoClient(uri, options)

// bloc try code à exécuter
try {
  // connection
  await client.connect()
  
  // operation et retour
  return client.db(dbName).collection(collName).find().toArray()
} catch(error) {
  // ici on gère les erreurs
  console.log(error)
  // si il existe un deuxième niveau on peut remonter l'erreur
  throw error
} finally {
  // On fait le ménage
  // client.db(dbName).dropDatabase()
  client.close()
}
```

#### ! `async function` retourn une `promise` implicite.

#### ! Pour exécuter des `Promise` en parallèle : `Promise.all([p1, p2, p3])`

### Méthode `get`

```js
async function get() {
  const client = new MongoClient(uri, { useUnifiedTopology: true })
  try {
    await client.connect()

    return await client.db(dbName).collection('newspaper').find().toArray()
  } catch (error) {
    throw error
  } finally {
    await client.close()
  }
}
```

`find` renvoie un `cursor`

`toArray` utilise le `cursor` pour créer un tableau avec toutes les données.



## `get` avec filtre

```js
// app.js
const filterData = await circulationRepo.get({ Newspaper: getData[4].Newspaper })
assert.notStrictEqual(filterData[0], getData[4])
```

```js
// circulationRepo.js
async function get(filter = {}) {
  // ...
  return await newspaper.find(filter).toArray()
```



## `get` avec `limit`

Pour récupérer un nombre de documents précis on utilise `cursor.limit(nbDocument)`

```js
// app.js
const limitData = await circulationRepo.get({}, 3)
assert.strictEqual(limitData.length, 3)
```

```js
// circulationRepo
async function get(filter = {}, limit = 10) {
  // ...
  await client.connect()

  let cursor = client.db(dbName).collection('newspaper').find(filter)

  cursor = cursor.limit(limit)
  return await cursor.toArray()
```

Le `cursor` ne contient rien tant qu'il n'est pas exaucé avec `toArray` ou `forEach`.



## Pagination avec `skip`

### `cursor.skip(nbDocument)`

```js
// app.js
let nbDoc = 0
const page1Data = await circulationRepo.get({}, 3, nbDoc)
nbDoc += page1Data.length
const page2Data = await circulationRepo.get({}, 3, nbDoc)
nbDoc += page1Data.length
const page3Data = await circulationRepo.get({}, 3, nbDoc)
nbDoc += page1Data.length
console.log(nbDoc, '\n', page1Data, page2Data, page3Data)
```

```js
// circulationRepo.js
async function get(filter = {}, limit = 10, skip = 0) {
  // ...
  await client.connect()

  let cursor = client.db(dbName).collection('newspaper').find(filter)

  cursor = cursor.limit(limit).skip(skip)
  return await cursor.toArray()
```



## `getById` avec `findOne`

```js
async function getById(id) {
  const client = new MongoClient(uri, { useUnifiedTopology: true })
  try {
    await client.connect()

    return await client.db(dbName).collection('newspaper').findOne({ _id: id })
  } catch (error) {
    console.log(`find by id ${id} failed`)
    throw error
  } finally {
    await client.close()
  }
}
```

Dans `app.js` on passe bien un `ObjectID` :

```js
const byId = await circulationRepo.getById(getData[4]._id)
```

Mais si pour des raisons pratique on récupère (par l'`url` par exemple) une `id` en chaîne de caractères, on aura une erreur :

```js
const id = getData[4]._id.toString()
console.log('id :', id)
const byId = await circulationRepo.getById(id)
assert.deepStrictEqual(byId, getData[4])
```

```bash
id : 604634a9417da68a35a61760
AssertionError [ERR_ASSERTION]: Expected values to be strictly deep-equal:
+ actual - expected

+ null
```

Il faut transformer la chaîne de caractère `id` en objet `ObjectID` dans `circulationRepo.js` :

```js
const { MongoClient, ObjectID } = require('mongodb')
// ...

async function getById(id) {
  // ...
  await client.connect()

  return await client
    .db(dbName)
    .collection('newspaper')
    .findOne({ _id: ObjectID(id) })
```

### `ObjectID(id)` transforme une chaîne en `ObjectID`.



## Ajouter un document `insertOne(item)`

`app.js`

```js
const paper = {
  Newspaper: 'Hukar Post',
  'Daily Circulation, 2004': 760034,
  'Daily Circulation, 2013': 474767,
  'Change in Daily Circulation, 2004-2013': -38,
  'Pulitzer Prize Winners and Finalists, 1990-2003': 52,
  'Pulitzer Prize Winners and Finalists, 2004-2014': 48,
  'Pulitzer Prize Winners and Finalists, 1990-2014': 100,
}
const addedItem = await circulationRepo.add(paper)
assert(addedItem._id)
const addedItemQuery = await circulationRepo.getById(addedItem._id)
assert.deepStrictEqual(addedItem, addedItemQuery)
```

`assert(something)` un alias de `assert.ok(something)` équivalent de `assert.equal(!!value, true)`

Regarde si une valeur est `thruthy` (vraisemblable !!).

méthode `add` :

```js
// circulationRepo.js
async function add(item) {
  const client = new MongoClient(uri, { useUnifiedTopology: true })
  try {
    await client.connect()
    const itemAdded = await client.db(dbName).collection('newspaper').insertOne(item)
    return itemAdded.ops[0]
  } catch (error) {
    console.log('added item failed')
    throw error
  } finally {
    client.close()
  }
}
```

Le document se trouve dans le tableau `ops` => `itemAdded.ops[0]`.



## Mettre à jour un document `replaceOne`

```js
async function update(id, newItem) {
  const client = new MongoClient(uri, { useUnifiedTopology: true })

  try {
    await client.connect()
    const updatedItem = await client
    .db(dbName)
    .collection('newspaper')
    .replaceOne({ _id: id }, newItem)
    return updatedItem.ops[0]
  } catch (error) {
    console.log(error)
  } finally {
    client.close()
  }
}
```

Il faut aussi renvoyer `updatedItem.ops[0]`

Tout a été remplacé sauf l'`_id`.

Dans `app.js`

```js
const newItem = {
  Newspaper: 'Super Hukar Post',
  'Daily Circulation, 2004': 678954,
  'Daily Circulation, 2013': 474767,
  'Change in Daily Circulation, 2004-2013': 100,
  'Pulitzer Prize Winners and Finalists, 1990-2003': 11,
  'Pulitzer Prize Winners and Finalists, 2004-2014': 0,
  'Pulitzer Prize Winners and Finalists, 1990-2014': 23,
}
const updatedItem = await circulationRepo.update(id, newItem)
assert.strictEqual(updatedItem.Newspaper, 'Super Hukar Post')
```





## Retirer un document `deleteOne(filter)`

```js
async function deleteItem(id) {
  const client = new MongoClient(uri, { useUnifiedTopology: true })

  try {
    await client.connect()

    const removed = await client.db(dbName).collection('newspaper').deleteOne({ _id: id })
    console.log('removed :', removed.deletedCount)
    return removed.deletedCount
  } catch (error) {
    console.log(error)
  } finally {
    await client.close()
  }
}
```

`removed.deletedCount` retourne le nombre de documents supprimés.































