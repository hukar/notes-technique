# 12 Course `Schema`

`models/Course.js`

```js
const mongoose = require("mongoose");

const CourseSchema = new mongoose.Schema({
    title: {
        type: String,
        trim: true,
        required: [true, "Please add a course title"],
    },
    description: {
        type: String,
        required: [true, "Please add a course description"],
    },
    weeks: {
        type: String,
        required: [true, "Please add a number of weeks"],
    },
    tuition: {
        type: Number,
        required: [true, "Please add a tuition cost"],
    },
    minimumSkill: {
        type: String,
        required: [true, "Please add a minimum skill"],
        enum: ["beginner", "intermediate", "advanced"],
    },
    scholarshipAvailable: {
        type: Boolean,
        default: false,
    },
    createdAt: {
        type: Date,
        default: Date.now,
    },
    bootcamp: {
        type: mongoose.Schema.ObjectId,
        ref: "Bootcamp",
        required: true,
    },
});

module.exports = mongoose.model("Course", CourseSchema);
```

Le lien vers le `bootcamp` est nouveau, il introduit un nouveau type :

#### `type: mongoose.Schema.ObjectId`

une nouvelle propriété :

#### `ref: "Bootcamp"`

Qui comme son nom l'indique est une référence vers `Bootcamp`.

On crée un `Schema` et on exporte un `Model ` : `mongoose.model(<name>, <Schema>)`.

