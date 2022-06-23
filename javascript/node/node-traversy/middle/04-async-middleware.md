# 04 async `middleware`

On va créer un `middleware` pour gérer les erreurs, on applique le principe `DRY` et on factorise les appelles à `try` and `catch`.

## `asyncHandler`

`middleware/async.js`

```js
const asyncHandler = (fn) => (req, res, next) =>
	Promise.resolve(fn(req, res, next)).catch(next);

module.exports = asyncHandler;
```

On prend un `middleware` et on renvoie un `middleware` dont les erreurs sont gérer par `catch(next)`.

## Refactor du controleur

```js
const Bootcamp = require("../models/Bootcamp");
const ErrorResponse = require("../utils/ErrorResponse");
const asyncHandler = require("../middleware/async");

/**
 * @description Get all bootcamps
 * @route       GET /api/v1/bootcamps
 * @access      Public
 */
const getBootcamps = asyncHandler(async (req, res, next) => {
    const bootcamps = await Bootcamp.find();

    res.status(200).json({
        success: true,
        count: bootcamps.length,
        data: bootcamps,
    });
});

/**
 * @description Get one bootcamp
 * @route       GET /api/v1/bootcamps/:id
 * @access      Public
 */
const getBootcamp = asyncHandler(async (req, res, next) => {
    const bootcamp = await Bootcamp.findById(req.params.id);
    if (!bootcamp) {
        return next(
            new ErrorResponse(
                `bootcamp with id : ${req.params.id} not found`,
                404
            )
        );
    }
    res.status(200).json({
        success: true,
        data: bootcamp,
    });
});

/**
 * @description Create bootcamp
 * @route       POST /api/v1/bootcamps
 * @access      Private
 */
const createBootcamp = asyncHandler(async (req, res, next) => {
    const bootcamp = await Bootcamp.create(req.body);
    // 201 status for create
    res.status(201).json({ success: true, data: bootcamp });
});

/**
 * @description Update one bootcamp
 * @route       PUT /api/v1/bootcamps/:id
 * @access      Private
 */
const updateBootcamp = asyncHandler(async (req, res, next) => {
    const bootcamp = await Bootcamp.findByIdAndUpdate(req.params.id, req.body, {
        new: true,
        runValidators: true,
    });

    if (!bootcamp) {
        return next(
            new ErrorResponse(
                `bootcamp with id : ${req.params.id} not found`,
                404
            )
        );
    }

    res.status(200).json({
        success: true,
        data: bootcamp,
    });
});

/**
 * @description Delete one bootcamp
 * @route       DELETE /api/v1/bootcamps/:id
 * @access      Private
 */
const deleteBootcamp = asyncHandler(async (req, res, next) => {
    const bootcamp = await Bootcamp.findByIdAndDelete(req.params.id);

    if (!bootcamp) {
        return next(
            new ErrorResponse(`bootcamp with id ${req.params.id} not found`),
            404
        );
    }
    res.status(200).json({
        success: true,
        data: {},
    });
});

module.exports = {
    getBootcamps,
    getBootcamp,
    createBootcamp,
    updateBootcamp,
    deleteBootcamp,
};
```

Tous les `try and catch` sont retirés et les `middleware` sont *wrappés* par `asyncHandler`.