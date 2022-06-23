# 02 `Mongoose`

```js
const mongoose = require('mongoose')

const options = {
    useNewUrlParser: true,
    useUnifiedTopology: true,
}
mongoose.connect('mongodb://localhost:27017/test', options)

const Cat = mongoose.model('Cat', { name: String })

const kitty = new Cat({name: 'kitty'})
kitty.save().then(() => console.log('meow'))
```

C'est un `ODM` :  `O`bject `D`ocument `M`apper



## Startup

```bash
npm i mongoose
```

On va créer un repertoire `src`, puis `src/db`.

dedans on crée un fichier `mongoose.js`

```js
const mongoose = require('mongoose')

mongoose.connect('mongodb://localhost:27017/task-manager-api', {
    useNewUrlParser: true,
    useUnifiedTopology: true,
    useCreateIndex: true,
})
```

On passe la base de données directement à la connexion.

Pour une bonne indexation on ajoute l'option `useCreateIndex: true`.

### `mongoose.model(modelName, definition)`

```js
const User = mongoose.model('User', {
    name: {
        type: String
    },
    age: {
        type: Number
    }
})
```

On utilise une majuscule pour le nom du  `model`.

Chaque propriété peut recevoir un type et des validateurs.

```js
const me = new User({
    name: 'Hukar',
    age: 45,
})
```

On peut créer une instance à partir du modèle, cette instance possède des méthodes intéressantes :

