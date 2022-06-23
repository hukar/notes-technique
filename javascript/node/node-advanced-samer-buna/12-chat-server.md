# 12 chat server (amélioration)

```js
process.stdout.write("\u001B[2J\u001B[0;0f");

const server = require("net").createServer();

const sockets = {};
let counter = 0;

function timestamp() {
    const now = new Date();

    return `${now.getHours()} : ${now.getMinutes()}`;
}

// l'objet socket implémente un stream en duplex (read et write)

server.on("connection", socket => {
    socket.id = counter++;

    console.log("client connected");
    socket.write("please type your name !\n");

    socket.on("data", data => {
        if (!sockets[socket.id]) {
            socket.name = data;
            socket.write(`Welcome ${socket.name}\n`);
            sockets[socket.id] = socket;
            return;
        }
        Object.entries(sockets).forEach(([key, sckt]) => {
            if (socket.id === Number(key)) {
                return;
            }
            sckt.write(`${socket.name} ${timestamp()}:`);
            sckt.write(data);
        });
    });

    socket.on("end", () => {
        delete sockets[socket.name];
        console.log("client is disconnected");
    });

    socket.setEncoding("utf8");
});

server.listen(4546, console.log("server bound"));

//pour tester on peu utiliser telnet ou netcat nc
```

![Screenshot 2020-03-23 at 17.09.07](assets/Screenshot 2020-03-23 at 17.09.07.png)