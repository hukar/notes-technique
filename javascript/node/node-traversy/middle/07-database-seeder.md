# 07 database Seeder

`seeder.js`

```js
const fs = require("fs");
const mongoose = require("mongoose");
const colors = require("colors");
const dotenv = require("dotenv");
const path = require("path");

// Load env vars
dotenv.config({ path: "./config/config.env" });

// Load models
const Bootcamp = require("./models/Bootcamp");

// Connect to DB
mongoose.connect(process.env.MONGO_URI, {
  useNewUrlParser: true,
  useCreateIndex: true,
  useUnifiedTopology: true,
  useFindAndModify: false,
});

// Read JSON files
const bootcamps = JSON.parse(
  fs.readFileSync(path.join(__dirname, "_data", "bootcamps.json"), "utf8")
);

// solution plus simple
// const bootcamps = require("./_data/bootcamps.json");

// Import data into DB
const importData = async () => {
  try {
    await Bootcamp.create(bootcamps);

    console.log("data imported ...".green.inverse);
    process.exit();
  } catch (error) {
    console.error(error);
  }
};

// Delete data on DB
const deleteData = async () => {
  try {
    await Bootcamp.deleteMany();

    console.log("data deleted ...".red.inverse);
    process.exit();
  } catch (error) {
    console.error(error);
  }
};

switch (process.argv[2]) {
  case "import":
    importData();
    break;
  case "delete":
    deleteData();
    break;
  default:
    console.log("bad argument".red.bold);
}
```

Pour récupérer les objets d'un fichier `json` on peut tout simplement faire:

```js
const bootcamps = require("./_data/bootcamps.json");
```

**rappel** : `process.argv[2]` est le paramètre fourni au script.

utilisation de `async / await` pour gérer les appelles à la DB.

<img src="assets/Screenshot2020-05-11at11.22.49.png" alt="Screenshot 2020-05-11 at 11.22.49" style="zoom:25%;" />

#### ! si on n'utilise pas `process.exit()` le processus ne se termine pas.
