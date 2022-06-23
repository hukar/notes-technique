# 05 Mongoose `middleware`

Les `middleware` Mongoose sont appelé `hook`.

## Ajouter `slugify`

```bash
npm i slugify
```

## Créer un `middleware`

Dans `models/Bootcamp.js` le `Schema` Mongoose, tout en bas :

```js
// ...
const slugify = require("slugify");

// ...
	createdAt: {
        type: Date,
        default: Date.now
    }
});

// Create bootcamp slug from the name
// this referre to the document
// arrow function not bind the good this
BootcampSchema.pre("save", function(next) {
    console.log("slugify ran", this.name);
    this.slug = slugify(this.name, { lower: true })
    next();
});

module.exports = mongoose.model("Bootcamp", BootcampSchema);
```

On utilise une fonction anonyme et pas une fonction fléchée pour que la valeur de `this` soit bien liée.

`slugify` dispose d'options, `lower` met tout en minuscule.

### Résultat

```json
{
    "success": true,
    "data": {
        // ...
        "name": "Hukar Bootcamp",
        // ...
        "slug": "hukar-bootcamp"
    }
}
```

