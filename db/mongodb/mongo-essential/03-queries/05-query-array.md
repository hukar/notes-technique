# 05 Requêter un tableau `query array`

Rechercher une valeur dans un tableau

```js
{<fieldname>: <value>}
```



Rechercher une valeur à un index précis d'un tableau

```js
{"<fieldname>.<index>": <value>}  // ne pas oublier les doubles guillemets
```



exemple :

```js
db.persons
.find({tags: "duis"})
.count()

63
```

```js
db.persons
.find({"tags.0": "duis"})
.count()

22
```

## `$all` et `$size`

Un tableau contenant toute les valeurs spécifiées (il peut avoir plus de valeurs que celles spécifiées)

```js
{<fieldname>: {$all: [<value1>, <value2>, ... ]}}
```



Un tableau d'une certaine taille

```js
{<fieldname>: {$size: <number>}}
```



Exemple

```js
db.persons
.find({tags: {$all: ["duis","laborum"]}})
```

```js
// ...
 "tags" : [ 
        "esse", 
        "duis", 
        "laborum"
    ]
// ...
```

On voit ici que le tableau peut contenir d'autre valeur.

```js
db.persons
.find({tags: {$size: 2}})
.count()

230
```

## `query array` avec des `documents` imbriqués

```json
{
    friends: [
        {
            name: "Lora",
            age:NumberInt(23)
        },
        {
            name: "Bob",
            age:NumberInt(25)
        }
    ]
}
```

Exemples :

```js
{"friends.name": "Lora"}
```

Ou bien 

```js
{friends: {name: "Lora",age: 23}}
```

Mais ici l'ordre des propriétés compte, et `{age: 23, name: "Lora"}` ne trouverait pas de concordance.

