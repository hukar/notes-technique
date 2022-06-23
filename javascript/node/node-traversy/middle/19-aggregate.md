# 19 Aggregate

Calcul de la moyenne des frais de scolarité (`tuition`) et création d'un champ pour ça.

## Modification de `models/Course.js`

```js
const mongoose = require("mongoose");

const CourseSchema = new mongoose.Schema({
   // ...
});

// Create a static method to get average of course tuitions
CourseSchema.statics.getAverageCost = async function(bootcampId) {
    console.log("calculating avg cost ...".blue);

    const arr = await this.aggregate([
        {
            $match: {bootcamp: bootcampId}
        },
        {
            $group: {
                _id: "$bootcamp",
                averageCost: { $avg: "$tuition" }
            }
        }
    ]);

    try {
        await this.model("Bootcamp").findByIdAndUpdate(bootcampId, {
            averageCost: arr[0].averageCost
        })
    } catch (error) {
        console.log(err);
    }
}

// Call getAverageCost after save
CourseSchema.post("save", function() {
    this.constructor.getAverageCost(this.bootcamp);
});

// Call getAverageCost before remove
CourseSchema.pre("remove", function() {
    this.constructor.getAverageCost(this.bootcamp);
});

module.exports = mongoose.model("Course", CourseSchema);
```

`this.aggregate` va renvoyer un table	au contenant un objet résultat de la requête d'agrégation.

Ensuite on utilise le `Model` de `Bootcamp` : `this.model("Bootcamp")` et on fait un `findByIdAndUpdate`.

Cette méthode statique est ensuite appelée après avoir sauvegarder un cours et avant d'en supprimer un :

`Schema.pre("remove", fn)`

`Schema.post("save", fn)`

## Une méthode static de `Model`

```js
// statics = on the models, else the method is called on the query
Course.myMethod() // = statics method
const courses = Course.findSomething()
courses.getFish() //not static method
```

Une méthode `static` est une méthode appelée sur le modèle et non sur la requête.

