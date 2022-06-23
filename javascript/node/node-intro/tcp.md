# `tcp`

`server.js`

```js
const net = require("net");
const server = net.createServer(onConnection);

const PORT = 8000;

let count = 0;
const users = {};

server.listen(PORT, () => console.log(`server listen on port :${PORT}`));

function onConnection(conn) {
    // conn est un socket de connexion
    // console.log(conn);

    const message = `Welcome on our < severCHAT >, the number of connected person is ${count}\nWhat is your name ?\n`;

    // empêche un bug d'égalité string / buffer
    conn.setEncoding("utf8");

    let name;
    count++;
    conn.write(message);

    // c'est mieux de nommer les fonction, en cas d'erreur on a le nom de la fonction
    conn.on("data", function onData(data) {
        if (!name) {
            // le slice retire un retour chariot à la fin (-1) dû à la touche entrée
            if (users[data.slice(0, -1)]) {
                conn.write(`${data.slice(0, -1)} is already taken, choose an other name\n`);
            } else {
                name = data.slice(0, -1);
                users[name] = conn;
                // broadcast
                shoutOut(`${name} has joined this room \n`);
            }
        } else {
            // broadcast
            shoutOut(`${name} : ${data}`);
        }
    });

    conn.on("close", function onClose() {
        count--;
        delete users[name];
        // broadcast : diffusion
        shoutOut(`${name} left!`);
    });
}

function shoutOut(message) {
    Object.keys(users).forEach((user) => {
        const cnx = users[user];
        cnx.write(message);
    });
}

// utilisation de netcat nc pour le test (et pas curl).
// nc localhost 8000
```



`client.js`

```js
const net = require("net");
const port = 8000;
const handler = {
    [Symbol.toStringTag]: "Handler",
    retries: 0,
    maxRetries: 10,
    interval: 2000,
    quit: false,
    inputHandler(data) {
        if (data.toString().trim() === "QUIT") {
            this.quit = true;
            console.log("you're quitting the chat room");
            this.conn.end();
            // on ferme l'écriture dans le terminal
            process.stdin.pause();
        } else {
            this.conn.write(data);
        }
    },
    connect() {
        // remettre à zéro le compteur de reconnexion
        this.retries = 0;
        // retourne un net.socket
        // [host] par défaut localhost
        // net.socket hérite de event emitter
        this.conn = net.createConnection(port, () => {
            console.log("is connecting ...");
        });

        this.conn.on("close", this.close.bind(this));
        this.conn.on("error", this.error.bind(this));

        process.stdin.resume();
        process.stdin.on("data", this.inputHandler.bind(this));
        this.conn.pipe(process.stdout, { end: false });
    },
    close() {
        // this.conn.emit("error");
        console.log("connection closed ... trying to reconnect");
        // corrige la duplication d'affichage liée à plusieurs "listener"
        process.stdin.removeAllListeners("data");
        // ferme l'entrée du terminal
        process.stdin.pause()
        this.reconnect();
    },
    error(err) {
        console.log(`error: ${err}`);
        // this.reconnect();
    },
    reconnect() {
        if( this.retries >= this.maxRetries) {
            throw new Error("the connection is so bad!");
        }
        setTimeout(() => {
            console.log("trying to connect");
            console.log(this.toString());
            this.retries++;
            this.connect();
        }, this.interval);
        
    },
};

handler.connect();
```



## remarque

Si on n'attrape pas l'événement `"data"`, on n'intercepte pas les événement `"close"` et `"end"` ??

`client2.js` tirer de la documentation officielle.

```js
const net = require('net');
const client = net.createConnection(8000, () => {
  // 'connect' listener.
  console.log('connected to server!');
  client.write('world!\r\n');
});
// client.on('data', (data) => {
//   console.log(data.toString());
// //   client.end();
// });
client.on('close', () => {
    console.log('connection closed');
});
client.on('end', () => {
  console.log('disconnected from server');
});
```

Je déconnecte l'événement `"data"`.

![Screenshot 2020-04-21 at 10.01.12](assets/Screenshot 2020-04-21 at 10.01.12.png)

On voit que les événement `"close"` et `"end"` ne sont pas déclenchés.

Maintenant on décommente l'écouteur de `"data"` :

```js
const net = require('net');
const client = net.createConnection(8000, () => {
  // 'connect' listener.
  console.log('connected to server!');
  client.write('world!\r\n');
});
client.on('data', (data) => {
  console.log(data.toString());
//   client.end();
});
client.on('close', () => {
    console.log('connection closed');
});
client.on('end', () => {
  console.log('disconnected from server');
});
```

![Screenshot 2020-04-21 at 10.34.56](assets/Screenshot 2020-04-21 at 10.34.56.png)

Nos événements `kend"` et `"close"` on bien été déclenchés.