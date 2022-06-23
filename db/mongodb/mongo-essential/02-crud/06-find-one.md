# 06 `findOne`

`findOne` renvoie un `document ` :

```js
const doc = db.cursor.findOne()
doc._id
doc.index
```

```js
ObjectId("5e8c982848ae0d686701a76f")
1
```

## Avec une `query`

```js
db.cursor.findOne({"index": 86})
```

```js
/* 1 */
{
    "_id" : ObjectId("5e8c982848ae0d686701a7c4"),
    "index" : 86
}
```

