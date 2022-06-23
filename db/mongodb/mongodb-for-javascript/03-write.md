# 03 `write`:  User-Facing Backend 



## `limit`

On peut restreindre le nombre de document retournés avec `limit` :

```js
const limitedCursor = movies
	.find({directors: "Sam raimi"}, {_id: 0, title: 1, cast: 1})
	.limit(2)

expect((await limitedCursor.toArray()).length).toBeLessThanOrEqual(2)
```

On observe que la projection peut se faire sans le mot `projection`.

La même chose avec `aggregate` :

```js
const limitPipeline = [
    { $match: { directors: "Sam Raimi" } },
    { $project: { _id: 0, title: 1, cast: 1 } },
    { $limit: 2},
]

const limitedAggregation = await movies.aggregate(limitPipeline)

expect((await limitedAggregation.toArray()).length).toBeLessThanOrEqual(2)
```





## `sort`

```js
const sortedCursor = movies
	.find({ directors: "Sam raimi"}, {_id: 0, year: 1, title: 1, cast: 1 })
	.sort([["year", 1]])
```

`[["year", 1]]` : le `1` signifie ici `ascendant`.

Le trie s'effectue sur le serveur `Mongo DB`, le `cursor` renvoie les éléments triés.

Maintenant avec `aggregation framework` :

```js
const sortPipeline = [
    { $match: { directors: "Sam raimi" } },
    { $project: { -id: 0, year: 1, title: 1, cast: 1 } },
    { $sort: { year: 1 } },
]

const sortAggregation = await movies.aggregate(sortPipeline)
const movieArray = await sortAggregation.toArray()
```

Pour tester :

```js
for (let i = 0; i < movieArray.length - 1; i++) {
    let movie = movieArray[i]
    let nextMovie = movieArray[i + 1]
    
    expect(movie.year).toBeLessThanOrEqual(nextMovie.year)
}
```



## `skip`

`skipping` à du sens si les documents renvoyés sont triés, sinon on ne sait pas quels documents sont *sautés*.

```js
const skippedCursor = movies
	.find({ directors: "Sam Raimi" },{ _id: 0, title: 1, cast: 1 })
	.sort([["year", 1]])
	.skip(5)  // saute les cinq premiers documents trouvés
```

Avec `aggregation framework`

```js
const skippedPipeline = [
    { $match: { director: "Sam Raimi" } },
    { $project: { _id: 0, title: 1,  cast: 1 } },
    { $sort: { year: 1 } },
    { $skip: 5 },
]
```



Les méthodes `skip()`, `limit()`, `sort()` ont leurs équivalent dans l'`aggregation framework` : `$skip`, `$limit` et `$sort`.