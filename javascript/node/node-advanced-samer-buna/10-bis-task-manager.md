# 10 bis un `task manager`

Ce `task manager` utilise deux fichier `.js`, un `client` ey un `server`.

`Client.js`

```js
const EventEmitter = require("events");
const readline = require("readline");

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});
```
`readline` permet de créer une interface sur l'entrée / sortie du terminal, on l'utilise en greffant un écouteur (`listener`) dessus : `rl.on("line", doSomething)`

```js
const client = new EventEmitter();
```
le client est simplement une instance de `EventEmitter`
```js
const server = require("./server")(client);
```
On peut ainsi passer le client au constructeur du serveur (évite d'utiliser `require` dans  `server.js`)
```js
server.on("response", resp => {
    // efface le contenu du terminal : ANSI escape code
    process.stdout.write("\u001B[2J\u001B[0;0f");
    process.stdout.write(resp);
    process.stdout.write("\n> ");
});

let command, args;

rl.on("line", input => {
    [command, ...args] = input.split(" ");
    client.emit("command", command, args);
});
```





`server.js`

```js
const EventEmitter = require("events");

class Server extends EventEmitter {
    constructor(client) {
        super();
        this.task = {};
        this.taskId = 1;
        // pour différé l'envoie lorsque l'écouteur est prêt nextTick
        process.nextTick(() =>
            this.emit("response", "type a command (help list commands)")
        );
        client.on("command", (command, args) => {
            console.log(`command : ${command}`);
            // help, add, ls, delete
            switch (command) {
                case "help":
                case "add":
                case "ls":
                case "delete":
                    this[command](args);
                    break;
                default:
                    this.emit("response", "unknows command ...");
            }
        });
    }

    help() {
        this.emit(
            "response",
            `command available :
        add
        ls
        delete`
        );
    }
    add(args) {
        this.task[this.taskId] = args.join(" ");
        this.emit("response", `task ${this.taskId} as added`);
        this.taskId++;
    }
    ls() {
        let tasks = "";
        for (let taskId in this.task) {
            tasks += `${taskId} : ${this.task[taskId]}\n`;
        }
        this.emit("response", tasks);
    }
    delete(args) {
        delete this.task[args[0]];
        this.emit("response", `task ${args[0]} was deleted`);
    }
}
// ici on reçoit la référence du client
module.exports = client => new Server(client);
```

