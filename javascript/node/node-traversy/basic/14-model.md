# 14 Model

## Créer un modèle pour un bootcamp

On crée un dossier `models` avec dedans `models/Bootcamp.js`.

Par convention on met la première lettre en capitale et le mot au singulier.

```js
const mongoose = require("mongoose");

const BootcampSchema = new mongoose.Schema({
    // name: String,
    name: {
        type: String,
        // required: true,
        required: [true, "Please add a name"],
        unique: true,
        trim: true,
        maxlength: [50, "Name can not be more than 50 characters"],
    },
    slug: String,
    description: {
        type: String,
        required: [true, "Please add a description"],
        maxlength: [500, "description can not be more than 500 characters"],
    },
    website: {
        type: String,
        match: [
            /https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)/,
            "Please use a valid URL with HTTP or HTTPS",
        ],
    },
    phone: {
        type: String,
        maxlength: [20, "Phone number can not be more than 20 characters"],
    },
    email: {
        type: String,
        match: [
            /^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i,
            "Please add a valid email",
        ],
    },
    address: {
        type: String,
        required: [true, "Please add an address"]
    },
    location: {
        // GeoJSON
        type: {
            type: String,
            enum: ["Point"],
            required: true
        },
        coordinates: {
            type: [Number],
            required: true,
            index: "2dsphere"
        },
        formattedAddress: String,
        street: String,
        city: String,
        state: String,
        country: String,
        zipcode: String,
    },
    careers: {
        type: [String],
        required: true,
        enum: [
            "Web Devlopment",
            "Mobile Devlopment",
            "UI/UX",
            "Data Science",
            "Business",
            "Other"
        ]
    },
    averageRating: {
        type: Number,
        min: [1, "Rating must be at least 1"],
        max: [10, "Rating must can not be more than 10"]
    },
    averageCost: Number,
    photo: {
        type: String,
        default: "no-photo.jpg"
    },
    housing: {
        type: Boolean,
        default: false
    },
    jobAssistance: {
        type: Boolean,
        default: false
    },
    jobGuarantee: {
        type: Boolean,
        default: false
    },
    acceptGi: {
        type: Boolean,
        default: false
    },
    createdAt: {
        type: Date,
        default: Date.now
    }
});

module.exports = mongoose.model("Bootcamp", BootcampSchema);
```

Pour un champ, on peut attribuer un type ou un objet d'options :

- `type` : `Boolean` `String` `Number` `[<type>]` `Date`
- `required` : `boolean` ou `[boolean, message]`
- `unique` : `boolean` Si la valeur doit être unique
- `trim` : `boolean`
- `maxlength` : `[number, message]`
- `enum` : `[<values>]` tableau de valeurs permises pour un champ
- `min` : `[number, message]`
- `max` : `[number, message]`
- `default` : `<default value>`

## dans `models/Course.js`

```js
bootcamp: {
        type: mongoose.Schema.ObjectId,
        ref: "Bootcamp",
        required: true,
    },
```

Pour référencé un document par son `ObjectId`.