# 17 

## get one course

`controllers/courses.js`

```js
/**
 * @description get single course
 * @route GET /api/v1/courses/:id
 * @access Public
 */
exports.getCourse = asyncHandler(async (req, res, next) => {
    const course = await Course.findById(req.params.id).populate({
        path: "bootcamp",
        select: "name description",
    });

    if (!course) {
        return next(
            new ErrorResponse(`course with id ${req.params.id} not found`),
            404
        );
    }

    res.status(200).json({
        success: true,
        data: course,
    });
});
```

On lui associe son `bootcamp` grâce à `populate` et on restreint les champs souhaités avec `select`.

Il faut l'ajouter aux routes :

`routes/courses.js`

```js
router.route("/:id").get(getCourse);
```

## add one course

`controllers/courses.js`

```js
// ...
const Bootcamp = require("../models/Bootcamp");
// ...

/**
 * @description create course
 * @route POST /api/v1/bootcamps/:bootcampId/courses/
 * @access Private
 */
exports.addCourse = asyncHandler(async (req, res, next) => {
    // test the existence of the bootcamp
    const bootcamp = await Bootcamp.findById(req.params.bootcampId);

    if (!bootcamp) {
        return next(
            new ErrorResponse(
                `This bootcamp doesn't exist : ${req.params.bootcampId}`
            ),
            404
        );
    }
    const course = await Course.create({
        ...req.body,
        bootcamp: req.params.bootcampId,
    });

    res.status(201).json({
        success: true,
        data: course,
    });
});
```

Pour ajouter un cours il faut que le `bootcamp` existe.

On ajoute un cours à un `bootcamp` d'où l'url : `api/v1/bootcamps/:bootcampId/courses/`.

Si le `bootcamp` n'existe pas on envoie une erreur au gestionnaire d'erreur de `next`.

### Routage

On a déjà une redirection dans le routeur de `bootcamp`:

`routes/bootcamps.js`

```js
// Re-route into other resource routers
router.use('/:bootcampId/courses', courseRouter);
```

Du coup notre route se met facilement dans `routes/courses.js` :

```js
router.route("/").get(getCourses).post(addCourse);
```

