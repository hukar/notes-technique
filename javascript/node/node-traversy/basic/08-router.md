# 08 Le routeur d'`Express`

## Mise en place

On crée le dossier `routes` et dedans le fichier `bootcamps.js` où il y aura toutes les routes de la ressource `bootcamps`

## `bootcamps.js`

```js
const express = require("express");

const router = express.Router();
```
On instancie un `router`

```js
router.get("/", (req, res) => {
    res.status(200).json({ success: true, msg: "show all bootcamps" });
});

router.get("/:id", (req, res) => {
    res.status(200).json({
        success: true,
        msg: `show single bootcamp ${req.params.id}`
    });
});

router.post("/", (req, res) => {
    res.status(200).json({ success: true, msg: "create new bootcamp" });
});

router.put("/:id", (req, res) => {
    res.status(200).json({
        success: true,
        msg: `update bootcamp ${req.params.id}`
    });
});

router.delete("/:id", (req, res) => {
    res.status(200).json({
        success: true,
        msg: `delete bootcamp ${req.params.id}`
    });
});
```
les routes partent de `api/v1/bootcamps` leurs écriture est donc simplifiée

```js
module.exports = router;
```
Il faut à la fin exporter ses routes.

## Simplification de `server.js`

```js
const express = require("express");
const dotenv = require("dotenv");

// Route files
const bootcamps = require("./routes/bootcamps");

// Load env vars
dotenv.config({ path: "./config/config.env" });

const app = express();

// Mount routers
app.use("/api/v1/bootcamps", bootcamps);

const PORT = process.env.PORT || 8080;

app.listen(
    PORT,
    console.log(
        `Server running in ${process.env.NODE_ENV} mode on port ${PORT}`
    )
);
```

On importe ses routes : 

### `const bootcamps = require("./routes/bootcamps");`

On monte le routeur :

###`app.use("/api/v1/bootcamps", bootcamps);` 

