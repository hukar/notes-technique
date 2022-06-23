# 18 crud `delete`

## Ma façon

```js
async function deleteBootcamp(req, res, next) {
    try {
        const bootcamp = await Bootcamp.deleteOne(
            { _id: req.params.id },
            (error) => console.log("error: ", error)
        );
        res.status(200).json({
            success: true,
            data: bootcamp,
        });
    } catch (error) {
        res.status(400).json({ success: false });
    }
}
```

## Solution

### `Model.findByIdAndDelete(<id>)`

```js
async function deleteBootcamp(req, res, next) {
    try {
        const bootcamp = await Bootcamp.findByIdAndDelete(req.params.id);

        if (!bootcamp) {
            return res.status(400).json({ success: false });
        }
        res.status(200).json({
            success: true,
            data: {},
        });
    } catch (error) {
        res.status(400).json({ success: false });
    }
}
```

