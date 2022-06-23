# 14 `udp`

`udp.js`

```js
const dgram = require("dgram");
PORT = 3333;
HOST = "127.0.0.1";
```
Infos communes

```js
//server
const server = dgram.createSocket("udp4");

server.on("listenning", () => {
    console.log("UDP server is listenning ...")
});

server.on("message", (message, rinfo) => {
    console.log(`${rinfo.address} : ${rinfo.port} - ${message}`);
});

server.bind(PORT, HOST);
```

On branche le serveur `UDP` sur deux événements `listenning` et `message`
```js
//client

const client = dgram.createSocket("udp4");

client.send("udp rocks !!", PORT, HOST, (err) => {
    if(err) {
        console.log(err);
    }

    console.log("message sent");
    client.close();
});
```

```bash
node udp.js
message sent
127.0.0.1 : 63116 - udp rocks !!
```

À chaque envoie de message, le client utilise un port différent :

```js
setInterval(() => {
    const client = dgram.createSocket("udp4");
    
    client.send("udp rocks !!", PORT, HOST, (err) => {
        if(err) {
            console.log(err);
        }
    
        console.log("message sent");
        client.close();
    });
}, 1000);
```

On voit qu'à chaque nouveau client on a un nouveau `port`.

```bash
node udp.js
message sent
127.0.0.1 : 64448 - udp rocks !!
message sent
127.0.0.1 : 56352 - udp rocks !!
message sent
127.0.0.1 : 53863 - udp rocks !!
```

## On peut utiliser un `Buffer`

```js
const client = dgram.createSocket("udp4");
const buff = Buffer.from("udp rocks !!");
const start = 0;
const end = buff.length;

client.send(buff, start, end, PORT, HOST, err => {
    if (err) {
        console.log(err);
    }

    console.log("message sent");
    client.close();
});
```

Il y a deux arguments en plus, le début et la fin souhaité du `buffer`.

