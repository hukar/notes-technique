# 12 `Mongoose`

```bash
npm i mongoose
```

## Configuration

On crée un deuxième fichier dans le répertoire `config` :

`config/db.js`

```js
const mongoose = require("mongoose");

const connectDb = async () => {
    const conn = await mongoose.connect(process.env.MONGO_URI, {
        useNewUrlParser: true,
        useCreateIndex: true,
        useUnifiedTopology: true,
        useFindAndModify: false
    });

    console.log(`MongoDb connected: ${conn.connection.host}`);
}

module.exports = connectDb;
```

Les différent `useX...` sont des recommandation de `MongoDB`.

Dans `config/config.env` :

```env
NODE_ENV=devlopment
PORT=5000

MONGO_URI=mongodb+srv://hukarTraversy:hukarTraversy@hukarcluster-w1mpz.gcp.mongodb.net/devcamper?retryWrites=true&w=majority
```

## Utilisation dans `server.js`

`server.js`

```js
// ...
const morgan = require("morgan");
const connectDb = require("./config/db.js");

// Load variable
dotenv.config({ path: "./config/config.env" });

// connect to database
connectDb();
```

## Gestion globale des erreurs

Dans `server.js` :

```js
const server = app.listen(
    PORT,
    console.log(
        `Server running in ${process.env.NODE_ENV} mode on port ${PORT}`
    )
);

process.on("unhandledRejection", (err, promise) => {
    console.log(`error: ${err.message}`);
    server.close(() => process.exit(1));
});
```

Cela évite d'utiliser un `try and catch` et cela permet d'avoir le message d'erreur clair.

On a ça :

```bash
Server running in devlopment mode on port 5000
error: Could not connect to any servers in your MongoDB Atlas cluster. Make sure your current IP address is on your Atlas cluster's IP whitelist: https://docs.atlas.mongodb.com/security-whitelist/.
```

au lien de ça :

```bash
Server running in devlopment mode on port 5000
(node:96151) UnhandledPromiseRejectionWarning: MongooseServerSelectionError: Could not connect to any servers in your MongoDB Atlas cluster. Make sure your current IP address is on your Atlas cluster's IP whitelist: https://docs.atlas.mongodb.com/security-whitelist/.
    at new MongooseServerSelectionError (/Users/kar/Documents/programmation/node/node-api-traversy/devcamper_api/node_modules/mongoose/lib/error/serverSelection.js:24:11)
    at NativeConnection.Connection.openUri (/Users/kar/Documents/programmation/node/node-api-traversy/devcamper_api/node_modules/mongoose/lib/connection.js:823:32)
    at Mongoose.connect (/Users/kar/Documents/programmation/node/node-api-traversy/devcamper_api/node_modules/mongoose/lib/index.js:333:15)
    at connectDb (/Users/kar/Documents/programmation/node/node-api-traversy/devcamper_api/config/db.js:4:33)
    at Object.<anonymous> (/Users/kar/Documents/programmation/node/node-api-traversy/devcamper_api/server.js:10:1)
    at Module._compile (internal/modules/cjs/loader.js:1156:30)
    at Object.Module._extensions..js (internal/modules/cjs/loader.js:1176:10)
    at Module.load (internal/modules/cjs/loader.js:1000:32)
    at Function.Module._load (internal/modules/cjs/loader.js:899:14)
    at Function.executeUserEntryPoint [as runMain] (internal/modules/run_main.js:74:12)
    at internal/main/run_main_module.js:18:47
(node:96151) UnhandledPromiseRejectionWarning: Unhandled promise rejection. This error originated either by throwing inside of an async function without a catch block, or by rejecting a promise which was not handled with .catch(). To terminate the node process on unhandled promise rejection, use the CLI flag `--unhandled-rejections=strict` (see https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode). (rejection id: 1)
(node:96151) [DEP0018] DeprecationWarning: Unhandled promise rejections are deprecated. In the future, promise rejections that are not handled will terminate the Node.js process with a non-zero exit code.
```

