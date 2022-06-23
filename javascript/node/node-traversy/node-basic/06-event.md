## 06 déclencher un événement `event`

```js
const EventEmitter = require("events");

// create class
class MyEmitter extends EventEmitter {}

// Init Object
const myEmitter = new MyEmitter();

// Event Listener
myEmitter.on("titi", () => console.log("event fired !"));

// Init Event
myEmitter.emit("titi");
```

### écouter `myEmitter.on(eventName, function)`

### Déclencher `myEmitter.emit(eventName`)

## système de `log` :

```js
const EventEmitter = require("events");
const uuid = require("uuid");
const path = require("path");
const fs = require("fs");

class Logger extends EventEmitter {
    loggDir = path.join(__dirname, "logg");

    loggFile = path.join(__dirname, "logg", "logg.txt");
    log(msg) {
        if (!fs.existsSync(this.loggDir)) {
            fs.mkdir(this.loggDir, {}, err => {
                if (err) throw err;
            });
        }

        if (fs.existsSync(this.loggFile)) {
            fs.appendFile(
                this.loggFile,
                `id: ${uuid.v4()} => message: ${msg}\n`,
                err => {
                    if (err) throw err;
                }
            );
        } else {
            fs.writeFile(
                this.loggFile,
                `id: ${uuid.v4()} => message: ${msg}\n`,
                err => {
                    if (err) throw err;
                }
            );
        }

        // Call event
        this.emit("message", { id: uuid.v4(), msg });
    }
}

module.exports = Logger;
```

## exemple avec `async` et `await`

```json
const EventEmitter = require("events");

const events = new EventEmitter();

events.on("boom", () => console.log("boom happened"));

const boomFactory = () => {
    return new Promise((resolve, reject) => {
        setTimeout(resolve, 1500, () => events.emit("boom"));
    });
};

const severalExplosion = async () => {
    const boom_1 = await boomFactory();
    boom_1();
    const boom_2 = await boomFactory();
    boom_2();
    const boom_3 = await boomFactory();
    boom_3();
    const boom_4 = await boomFactory();
    boom_4();
};

severalExplosion();
```

toutes les 1,5 s `"boom happened"` apparaît dans la console.