# 14 `populate`

On souhaite parfois intégrer les données d'un élément référencé.

Par exemple, chaque `course` aun `bootcamp`.

Il suffit d'ajouter la méthode `populate` sur le `Model`.

Il faut aussi importer le modèle concerné.

`controllers/courses.js`

```js
const Bootcamp = require("../models/Bootcamp");
// ...

exports.getCourses = asyncHandler(async (req, res, next) => {
  let query;

  if (req.params.bootcampId) {
    query = Course.find({ bootcamp: req.params.bootcampId });
  } else {
    query = Course.find().populate("bootcamp");
  }
  // ...
});
```

**Avant**

<img src="assets/Screenshot2020-05-13at15.21.45.png" alt="Screenshot 2020-05-13 at 15.21.45" style="zoom:25%;" />

**Après**

<img src="assets/Screenshot2020-05-13at15.23.46.png" alt="Screenshot 2020-05-13 at 15.23.46" style="zoom:25%;" />

On a maintenant tout le `bootcamp` et non plus seulement son `_id`.

## Seulement quelques champs

Si on veut seulement quelques champs :

`controllers/courses.js`

```js
query = Course.find().populate({
  path: "bootcamp",
  select: "name description",
});
```

<img src="assets/Screenshot2020-05-13at15.36.58.png" alt="Screenshot 2020-05-13 at 15.36.58" style="zoom:25%;" />
