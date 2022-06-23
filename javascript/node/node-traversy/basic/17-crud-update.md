# 17 crud `update`

## Ma façon

### avec `Model.findById` et `.save()`

```js
async function updateBootcamp(req, res, next) {
    try {
        const bootcamp = await Bootcamp.findById(
            req.params.id,
            (err, bootcamp) => {
                if (err) {
                    return console.log("error: ", err);
                }

                bootcamp.name = "updated name :)";
                bootcamp.save(() => console.log("bootcamp name updated"));
            }
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

### `Model.findOneAndUpdate(<id>,<fields_updated>,<options>)`

```js
async function updateBootcamp(req, res, next) {
    
    const bootcamp = await Bootcamp.findOneAndUpdate(req.params.id, req.body, {
        new: true,
        runValidators: true,
    });

    if (!bootcamp) {
        return res.status(400).json({ success: false });
    }

    res.status(200).json({
        success: true,
        data: bootcamp
    });
}
```

![Screenshot 2020-05-08 at 10.21.10](assets/Screenshot 2020-05-08 at 10.21.10.png)

Il faut aussi ajouter le `header` : `Content-Type: application/json` dans `Postman` ou sur le site.

`new: true` signifie que c'est le nouvel enregistrement (modifié) qui est renvoyé.

`runValidators: true` signifie que les changements passeront par les validateurs défini dans le `Schema`.

En fait `{name: "new name"}` est remplacé en interne par `{$set: {name: "new name"}}` pour éviter d'écraser accidentellement l'enregistrement.

