db.reviews.deleteOne({ _id: ObjectId('5e42cab366c2d25d83545d63') })

db.reviews.deleteMany({ rating: { $lte: 3.5 } })
