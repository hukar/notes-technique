# 03 Les opérateurs logiques

## `$and` operator

Combiner plusieurs conditions, le résultat doit les vérifier toutes.

```js
{ $and : [{<condition1>}, {<condition2>}, ... ]}
```

exemple

```js
db.persons
// .find({eyeColor: "brown", age: {$gte: 25}}) => 253
.find({$and: [{eyeColor: "brown"}, {age: {$gte: 25}}]})
.count()

253
```

Mais cet opérateur est très utile lorsqu'on veut avoir plusieurs conditions sur le même champ, pour éviter les effets d'`overwrite`.

```js
db.persons
.find({age: {$gte: 25, $ne: 26, $ne: 27, $lt: 28}})
```

![Screenshot 2020-04-08 at 17.14.45](assets/Screenshot 2020-04-08 at 17.14.45.png)

On voit que le premier `$ne` est en fait écrasé par le second. Ici l'utilisation de `$and` résout le problème :

```js
db.persons
//.find({age: {$gte: 25, $ne: 26, $ne: 27, $lt: 28}}) => 101
.find({$and: [{age: {$gte: 25}},{age: {$ne: 26}},{age: {$ne: 27}}, {age: {$lt: 28}}]})
```

On obtient `50` documents, au lieu de `101` avec la première requête.

exemple de requêtes similaire avec `and` implicite : `,` ou explicite : `$and` :

```js
db.persons
//.find({$and: [{age: {$ne: 25}}, {age: {$gte: 25}}]}) => 692
.find({age: {$ne: 25, $gte: 25}})
.sort({age: 1})
.count()

692
```



## `$or` operator

```js
{ $or: [ {<condition1>}, {<condition2>}, ... ]}
```

exemple :

```js
db.persons.find({$or: [{gender: "male"}, {age: 25}]})
```

```js
db.persons
//.find({$or: [{age: 27}, {age: 25}]}) => 92
.find({age: {$in: [25, 27]}})
.count()

92
```

`{$or: [{age: 27}, {age: 25}]}` est équivalent à `{age: {$in: [25, 27]}}`.

Si on veut faire une condition `or` sur le même champs, l'opérateur `$in` propose une syntaxe plus concise.

### Une grande requête

```js
db.persons.find({$or: [{favoriteFruit: "banana"}, {age: {$lt: 24}}, {eyeColor: {$in: ["green","blue"]}}]}).count()

829
```

