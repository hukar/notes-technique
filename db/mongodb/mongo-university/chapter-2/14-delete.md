# 14 `delete`

## `deleteOne`

```bash
PRIMARY> db.reviews.deleteOne({_id: ObjectId('5e42cab366c2d25d83545d63')})
{ "acknowledged" : true, "deletedCount" : 1 }
```

## `deleteMany`

```bash
PRIMARY> db.reviews.deleteMany({ rating: { $lte: 3.5 } })
{ "acknowledged" : true, "deletedCount" : 8 }
```

