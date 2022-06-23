# 13 `GET` courses

## 1 Créer un modèle

Voire précédemment

## 2 créer un contrôleur

`controllers/courses.js`

```js
const Course = require("../models/Course");
const asyncHandler = require("../middleware/async");

/**
 * @description get courses
 * @route GET /api/v1/courses
 * @route GET /api/v1/bootcamps/:bootcampId/courses
 * @access Public
 */
exports.getCourses = asyncHandler(async (req, res, next) => {
    let query;

    if (req.params.bootcampId) {
        query = Course.find({ bootcamp: req.params.bootcampId });
    } else {
        query = Course.find();
    }

    const courses = await query;

    res.status(200).json({
        success: true,
        count: courses.length,
        data: courses,
    });
});

```

Le contrôleur teste si un `Id` de bootcamp lui est fourni.

## 3 Créer les routes

#### `api/v1/courses`

`routes/courses.js`

```js
const express = require("express");

const { getCourses } = require("../controllers/courses");

const router = express.Router();
// const router = express.Router({ mergeParams: true });

router.route("/").get(getCourses);

module.exports = router;
```

#### `api/v1/bootcamps/5d713a66ec8f2b88b8f830b8/courses`

Il faut d'abord créer une redirection dans les routes de bootcamps.

`routes/bootcamps.js`

```js
// ...

// Include other resource routers
const courseRouter = require("./courses");

const router = express.Router();

// Re-route into other resource routers
router.use('/:bootcampId/courses', courseRouter);

// ...
```

On délègue la route `/:bootcampId/courses` au routeur de `courses` :

```js
router.use('/:bootcampId/courses', courseRouter);
```

Dans `routes/courses`, on autorise le passage de paramètres :

```js
const router = express.Router({ mergeParams: true });
```



## 4 L'ajouter à `server.js`

```js
// ...

// Routes file
const bootcamps = require("./routes/bootcamps");
const courses = require("./routes/courses");

// ...

// Mount router
app.use("/api/v1/bootcamps", bootcamps);
app.use("/api/v1/courses", courses);

// ...

```

