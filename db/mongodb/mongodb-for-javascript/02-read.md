# 02 Driver et `read`

## Initialiser un client : `MongoClient`

```js
try {
    const client = await MongoClient.connect(
        process.env.MFLIX_DB_URI, 
        { useNewUrlParser: true,/* connectTimeoutMS: 200, retryWrites: true */})
}
```

### Récupérer les options

```js
const clientOptions = client.s.options
```



### Vérifier que `SSL` est activé

```js
expect(clientOptions.ssl).toBe(true)
```



## Récupérer la DB

```js
const mflixDB = client.db("mflix")
```

```js
const mflixCollections = await mflixDB.listCollections().toArray()
```



## Récupérer une collection

```js
const movies = mflixDB.collection("movies")
```

### Compter les documents

```js
const numMoves = await movies.countDocuments({})
expect(numMoves).toBe(46014)
```



## faire tourner les test : `jest`

```bash
npm test -t mongoclient

> server@1.0.0 test
> jest --passWithNoTests "mongoclient"
```

<img src="assets/test-jest-mongoclient-four-passed.png" alt="test-jest-mongoclient-four-passed" style="zoom:50%;" />



## `Promise`,  `Callback` et  `Async`

Si une fonction du `Driver` ne contient pas de `callback`, alors le `Driver` renvoie une `promise`.

### `callback`

```js
movies.findOne({ title: "one value"}, (err, doc) => {
    expect(err).toBeNull()
    expect(doc.title).toBe("one value")
    // ...
    done()  // for the testing framework
})
```





### `promise`

```js
movies.findOne({title: "one value"})
	.then(doc => {
        expect(doc.title).toBe("One value")
        expect(doc.cast).toContain("Salma Hayek")
        done()
	})
.catch(err => {
    expect(err).toBeNull()
    done()
})
```



### `async/await`

Les fonctions de `test` doivent être encadrées par une construction de ce type :

```js
test("nom du test", async () => {
    // ici mes tests
})
```

Toujours entourer un `await` avec un `try/catch block`.

```js
try {
    const { title, cast } = await movies.findOne({title: "Super Title"})
    expect(title).toBe("Super Title")
    expect(cast).toContain("Salma Hayek")
} catch(e) {
    expect(e).toBeNull ()
}
```





## Basic Reads

### `findOne`

```js
const result = await movies.findOne({ cast: "Salma Hayek" })

const { title, year, cast } = result
expect(title).toBe("Roadracers")
expect(year).toBe(1994)
expect(cast).toContain("David Arquette")
```

`cast` est un tableau, `{cast: "Salma Hayek"}` est compris comme **contient** `"Salma Hayek"`.

Si la requête ne trouve rien, elle renvoie `null`

```js
const nullResult = await movies.findOne({cast: "Toto Hakusho"})
expect(nullResult).toBeNull()
```



### Projection

```js
const result = await movies.findOne(
	{ cast: "Salma Hayek" },
    { projection: { title: 1, year: 1 } }
)

expect(Object.keys(result).length).toBe(3))  // avec _id, title, year
```

Une `projection` permet de ne récupérer que certain champs.

Par défaut `_id` est renvoyé aussi.

Pour ne pas avoir le champ `_id`, il faut l'exclure explicitement :

```js
const result = movies.findOne(
    { cast: "Salma Hayek" },
    {
        projection: {
            title: 1,
            year: 1,
            _id: 0,
        }
    }
)

expect(Object.keys(result).length).toBe(2) // title, year
```



### `find`

### `$all` => `ET`

`$all` est un opérateur qui va vérifier que toutes les valeurs sont présentes dans le tableau (`"Salma Hayek"` **ET** `"Jonnhy Deep"`).

```js
const results = await movies.find(
    {
		cast: { $all: ["Salma Hayek", "Johnny Depp"] }
    }
)
```

`results` est en fait un `cursor` .

Pour avoir le résultat **suivant** : `next`

```js
const { title, year, cast } = await results.next()
```

cela va retourner un et un seule document.

S'il n'y a plus de documents, `next` renvoie `null`.

On peut aussi récupérer le tableau de tous les documents correspondants on utilise `toArray` :

```js
const allResultsInArray = await results.toArray()
```



### `$in` => `OU`

En utilisant le `$in`, on veut les documents possédant une ou plusieurs des valeurs proposées :

Dans le `shell` :

```bash
db.movies.find({countries: {$in:["Russia", "Japan", "Mexico"]}},{title: 1}).count()
1468
```

Avec le `Driver JS` :

```js
try {
    const cursor = await movies.find({countries: {$in: countries}},{projection: {title: 1}})
} catch (e) {
    console.error(`Unable to issue find command, ${e}`)
    return []
}
```

