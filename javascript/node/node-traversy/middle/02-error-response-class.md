# 02 Création d'une classe `ErrorResponse`

Cette classe nous permet de renvoyer notre propre message d'erreur ainsi que le code du `status`.

Pour ce faire nous créons un dossier `utils` et notre classe dedans :

`utils/ErrorResponse.js`

```js
class ErrorResponse extends Error {
    constructor(message, statusCode) {
        super(message);
        this.statusCode = statusCode;
    }
}

module.exports = ErrorResponse;
```

Il faut légèrement modifier `middleware/error.js` :

```js
const colors = require("colors");

function errorHandler(err, req, res, next) {
    // log to console for dev
    console.log(err.stack.red);

    res.status(err.statusCode || 500).json({
        success: false,
        error: err.message || "server error",
    });
}

module.exports = errorHandler;

```

On peut utiliser ici le `err.statusCode`

## Utilisation dans le controleur

`controllers/bootcamps.js`

```js
async function getBootcamp(req, res, next) {
    try {
        // const bootcamp = await Bootcamp.find({ _id: req.params.id });
        const bootcamp = await Bootcamp.findById(req.params.id);
        if (!bootcamp) {
            return next(new ErrorResponse(`bootcamp with id : ${req.params.id} not found`, 404));
        }
        res.status(200).json({
            success: true,
            data: bootcamp,
        });
    } catch (error) {
        next(new ErrorResponse(`id : ${req.params.id} is not a valid formatted id`, 400));
    }
}
```

`404` not found

`400` bad request

![Screenshot 2020-05-08 at 15.26.41](assets/Screenshot 2020-05-08 at 15.26.41.png)

![Screenshot 2020-05-08 at 15.26.26](assets/Screenshot 2020-05-08 at 15.26.26.png)

Dans un cas l'`id` est valide mais n'existe pas et dans l'autre l'`id` est de format invalide.