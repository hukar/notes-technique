db.movieDetails.updateOne(
    { title: "A Million Ways to Die in the West" },
    {
        $addToSet: {
            score: { $each: [56, 67, 69, 70] }
        }
    }
)

db.movieDetails.update({}, {
    $pull: {
        writers: { $in: ["Sergio Leone", "Sergio Donati"] },
        genres: "Western"
    }
})

db.movieDetails.updateMany({ rated: null },
    { $unset: { rated: "" } }
)

db.movieDetails.updateOne(
    { title: "Cochon in Space" },
    {
        $set: {
            title: "Cochon in Space",
            year: 1030,
            animal: "lion"
        }
    },
    { upsert: true }

)
