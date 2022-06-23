# 18

### `/controllers/courses.js`

## update

```js
/**
 * @description update course
 * @route PUT /api/v1/courses/:id
 * @access Private
 */
exports.updateCourse = asyncHandler(async (req, res, next) => {
    
    const course = await Course.findByIdAndUpdate(req.params.id, req.body, {
        new: true,
        runValidators: true,
    });

    if(!course) {
        return next(new ErrorResponse(`course with id ${req.params.id} not found`), 404);
    }

    res.status(200).json({
        success: true,
        data: course,
    });
});
```

## delete

```js
/**
 * @description update course
 * @route DELETE /api/v1/courses/:id
 * @access Private
 */
exports.deleteCourse = asyncHandler(async (req, res, next) => {
    
    const course = await Course.findById(req.params.id);

    if(!course) {
        return next(new ErrorResponse(`course with id ${req.params.id} not found`), 404);
    }


    await course.remove();

    res.status(200).json({
        success: true,
        data: {},
    });
});
```

1. on teste l'existence du cours
2. on le supprime avec `.remove()`

