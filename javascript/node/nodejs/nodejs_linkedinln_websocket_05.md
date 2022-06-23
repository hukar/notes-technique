# Socket avec ws

```sh
npm install ws --save
```

app.js

```js
var webSocketServer = require('ws').Server
var wss = new webSocketServer({'port': 2900})

wss.on('connection', (ws) => {
    console.log('CONNECTED !!')
    
    // visible dans les outils de développement de google chrome
    ws.send('yo koko!!')
})
```

index.html

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Socket Socket</title>
    <link rel="stylesheet" href="kiki.css">

</head>
<body>
    <h1>hello gogo!</h1>
    <form action="javascript:void(0)">
        <label for="message"></label>
        <input type="text" name="message" required autofocus>
    </form>
 	<!-- on relie le script ici -->
    <script src="ws-client.js"></script>
</body>
</html>
```

ws-client.js

```js
// on se branche sur le protocole websocket
var ws = new WebSocket('ws://localhost:2900')

ws.onopen = function () {
    setTitle('connect to CHAT')
}

ws.onclose = function () {
    setTitle('DISCONNECTED')
}

document.form[0].onsubmit = function () {
    var input = document.getElementById('message')
    ws.send(input.value)
    input.value = ''
}

function setTitle (title) {
    document.querySelector('h1').innerHTML = title
}

function printMessage (message) {
    var p = document.createElement('p')
    p.innerText = message
    document.querySelector('div.messages').appendChild(p)
}
```