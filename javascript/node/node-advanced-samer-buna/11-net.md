# 11 `net`

## Création d'un simple serveur

`Net.js`

```js
process.stdout.write("\u001B[2J\u001B[0;0f");

const server = require("net").createServer();

// l'objet socket implémente un stream en duplex (read et write)

server.on("connection", socket => {
    console.log("a client is connected");
    socket.write("hello new client !\n");
});

server.listen(4546, console.log("server bound"));
```

Pour tester on peut utiliser `telnet` ou `netcat`, ici on utilise `netcat`.

```bash
nc localhost 4546
hello new client !
```

Dans le terminal de `Node.js`

```
server bound
a client is connected
```

## Lire le `socket` : `socket.on("data", cb)`

L'objet `socket` implémente un stream en duplex, il peut à la fois lire et écrire (`read` et `write`).

Le `socket` est en lecture sur l'événement `data` :

```js
server.on("connection", socket => {
    console.log("a client is connected");
    socket.write("hello new client !\n");
    
    socket.on("data", data => {
        console.log(data);
    });
});
```

Il reçoit un `Buffer`

`serveur Node.js`

```
server bound
a client is connected
<Buffer 48 65 79 20 79 6f 75 21 0a>
```

`client netcat`

```bash
nc localhost 4546
hello new client !
Hey you!
```

Si je renvois les données reçu, j'obtiens un `string` car la méthode `.write` encode en `utf8` par défaut.

```js
server.on("connection", socket => {
    console.log("a client is connected");
    socket.write("hello new client !\n");
    socket.on("data", data => {
        console.log(data);
        socket.write("data is : ");
        socket.write(data);
    });
});
```

`Client`

```bash
nc localhost 4546
hello new client !
hello server !!
data is : hello server !! # on voit qu'on récupère un string
```

## `socket.setEncoding`

On peut régler l'encoding pour le socket avec `socket.setEncoding` :

```js
server.on("connection", socket => {
    console.log("a client is connected");
    socket.write("hello new client !\n");
    socket.on("data", data => {
        console.log(data);
        socket.write("data is : ");
        // socket.write(data, "utf8");
        socket.write(data);
    });

    socket.setEncoding("utf8");
});
```

Ici le `buffer` est remplacé par une chaîne de caractère en `utf8`.

![Screenshot 2020-03-23 at 15.29.02](assets/Screenshot 2020-03-23 at 15.29.02.png)

## Fin de communication `socket.on("end", cb)`

```js
server.on("connection", socket => {
    console.log("a client is connected");
    socket.write("hello new client !\n");
    socket.on("data", data => {
        console.log(data);
        socket.write("data is : ");
        // socket.write(data, "utf8");
        socket.write(data);
    });

    socket.on("end", () => {
        console.log("client is disconnected");
    });

    socket.setEncoding("utf8");
});
```

![Screenshot 2020-03-23 at 15.34.16](assets/Screenshot 2020-03-23 at 15.34.16.png)

## Communiquer avec plusieurs `socket`

À chaque fois que quelqu'un se connecte cela crée un nouvel objet `socket` qui est propre à sa connexion.

```js
let counter = 0;

server.on("connection", socket => {
    socket.id = counter++;
    socket.write("hello new client !\n");
    socket.on("data", data => {
        socket.write(`${socket.id} :`);
        socket.write(data);
    });

    socket.on("end", () => {
        console.log("client is disconnected");
    });

    socket.setEncoding("utf8");
});
```

![Screenshot 2020-03-23 at 15.44.24](assets/Screenshot 2020-03-23 at 15.44.24.png)

### On peut relier les `socket`

```js
let counter = 0;
const sockets = {};

server.on("connection", socket => {
    socket.id = counter++;
    sockets[socket.id] = socket;
    socket.write("hello new client !\n");
    socket.on("data", data => {
        Object.entries(sockets).forEach(([, sckt]) => {
            sckt.write(`${socket.id} :`);
            sckt.write(data);
        });
    });

    socket.on("end", () => {
        console.log("client is disconnected");
    });

    socket.setEncoding("utf8");
});
```

On crée un ensemble de `socket`

```js
const sockets = {};
```

On itère sur chaque `socket` afin de lui passer le message :

```js
Object.entries(sockets).forEach(([, sckt]) => {
    // ...
});
```

`Object.entries` renvoie un tableau de couple `[key, value]`, où `key` est un string représentant le nom de la propriété et `value`, sa valeur : `[[k1, v1], [k2, v2], [k3, v3], ... ]` pour un object `{k1: v1, k2: v2, k3: v3, ... }`.

### Si un client disconnect !

```bash
client is disconnected
events.js:187
      throw er; // Unhandled 'error' event
                                ^
Error: This socket has been ended by the other party
```

Crash, car on essaye d'écrire sur un `socket` qui n'existe plus.

### Solution

Il faut retirer le `socket` lorsqu'il se déconnecte `.on("end", cb)`.

```js
socket.on("end", () => {
    delete sockets[socket.id];
    console.log("client is disconnected");
});
```

![Screenshot 2020-03-23 at 16.06.52](assets/Screenshot 2020-03-23 at 16.06.52.png)

Pas de crash.