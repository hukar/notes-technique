# 01 les `queries`

## Insérer des données dans `Robo 3T`

```js
db.persons.insert([ ... ])
```

Pour vérifier le nombre d'enregistrement :

```js
db.persons.find().count()

1000
```

## `query` d'égalité

```js
{<fieldname1>: <value1>, <fieldname2>: <value2>, ... }
```



La requête est un objet passé à `find`.

`{}` représente tout les `documents` :

```js
db.persons.find({}).count()

1000
```

Si on désire tous les documents dont l'age est égal à 20 :

```js
db.persons.find({"age": 20})
```

On obtient `46 documents`.

### Plusieurs égalités

```js
db.persons.find({"gender": "female", "eyeColor": "blue"}) // 170 documents
```

La `,` entre les égalités signifie `and`.

Les `"<fieldname>"` doubles guillemets autour des noms de propriété sont facultatives :

```js
db.persons.find({gender: "female", eyeColor: "green"}).count()

179
```

