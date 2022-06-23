# HTTP
## récupérer une page web

`https.request(options, (response) => { ... })`

```js
var fs = require('fs')
var https = require('https')

var options = {
    hostname: 'fr.wikippedia.org',
    port: 443,
    path: '/wiki/Node.js',
    method: 'GET'
}
var req = https.request(options, (response) => {
    var responseBody = ''
    console.log('start')
    console.log(response.statusCode)
    // console.log(response.headers)

    response.setEncoding('utf-8')

    response.on('data', (chunk) => {
        console.log(chunk)
        responseBody += chunk
    })

    response.on('end', () => {
    // écriture d'un fichier en asynchrone
        fs.writeFile('wiki.html', responseBody.trim(), () => {
            console.log('fichier enregistré')
        })
    })
    
})
// gestion des erreurs
req.on('error', (error) => {
    console.log(error)
})

req.end()
```

## serveur http

```js
var fs = require('fs')
var http = require('http')

var server = http.createServer((req, res) => {
    res.writeHeader('200', {'content-type': 'text/plain'})
    res.end('je suis un serveur HTTP')
})

server.listen(3000)

console.log('server on http://localhost:3000')
```

### Servir du HTML

```js
var fs = require('fs')
var http = require('http')

var server = http.createServer((req, res) => {

	//content-type en text/html
    res.writeHeader('200', {'content-type': 'text/html'})

    var response = `<!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>Server HTTP</title>
    </head>
    <body>
        <h1>Hello world</h1>
    	
    	// on peut afficher les variables de la requête par exemple
        <p>${req.url}</p>
        <p>${req.method}</p>
        <p>${req}</p>
    
    </body>
    </html>`
    res.end(response)
})

server.listen(3000)

console.log('server on http://localhost:3000')
```

## serveur de fichier statique

```js
var fs = require('fs')
var http = require('http')
var path = require('path')

var server = http.createServer((req, res) => {
   console.log(`${req.method} : request for ${req.url}`)

   if (req.url === '/') {
       fs.readFile('./public/index.html', (error, html) => {
           res.writeHead(200, {'content-type': 'text/html'})
           res.end(html)
       })

   } else if (req.url.match(/.css$/)) {
		// avec un stream
        // var cssPath = path.join(__dirname,'graphisme',req.url)
        // var fileStream = fs.createReadStream(cssPath,'utf-8')

        // res.writeHead(200, {'content-type': 'text/css'})
        // fileStream.pipe(res)

        fs.readFile('./graphisme/pilou.css', (error, css) => {
            res.writeHead(200, {'content-type': 'text/css'})
            res.end(css)
        })
   } else {
       res.writeHead(404, {'content-type': 'text/plain'})
       res.end('404 not found')
   }
   
})

server.listen(3000)
```

Les fichiers ne dévoilent pas leur vrai location sur le serveur :
en sécurité cela s'appelle un chateau de sable.

## éteindre un processus node 

```sh
ps // pour listre les processus
kill 1434  // le nombre étant l'identifiant PID du processus node
```

## servir du json

`JSON.stringify(data)` pour transformer du json en texte

`req.url.match(/une-url-particuliere$/)` pour récupérer une url

`tab.filter((item => { return item.attr === quelquechose })` pour filtrer les élément d'un tableau égale à un critère

```js
var fs = require('fs')
var http = require('http')

var data = require('./data/data.json')
var resGlobal

var server = http.createServer((req, res) => {
    resGlobal = res
    
    // mini routeur
    if (req.url === '/') {
        res.writeHeader(200, {'content-type': 'text/json'})
        res.end(JSON.stringify(data))
    } else if (req.url.match(/is-active$/)) {
        isActive()
    } else if (req.url.match(/is-inactive$/)) {
        isInActive()
    } else {
        res.end('Not found')
    }
})

function isActive() {
    var isActive = data.filter((item) => {
        return item.isActive === true
    })
    resGlobal.end(JSON.stringify(isActive))
}

function isInActive() {
    var isInActive = data.filter((item) => {
        return item.isActive === false
    })
    resGlobal.end(JSON.stringify(isInActive))
}

server.listen(3000)

console.log('server on http://localhost:3000')
```

# recevoir des données en POST

```js
var fs = require('fs')
var http = require('http')

var resGlobal

var server = http.createServer((req, res) => {
    resGlobal = res

    if (req.method === 'GET') {
        if (req.url.match(/.css$/)) {
            fs.readFile('./graphisme/pilou.css', (error, css) => {
                res.writeHead('201', {'content-type': 'text/css'})
                res.end(css)
            })
        } else {
            fs.readFile('./public/index.html',(error, html) => {
                res.writeHead('200', {'content-type': 'text/html'})
                res.end(html)
            })
        }
        
    } else if(req.method = 'post') {
        var body
        
        // récupération des données
        req.on('data', (chunk) => {
            body = chunk   
        })
        req.on('end', () => {
        	// affichage des données
            res.writeHead('200', {'content-type': 'text/html'})
            res.end(body)
        })
    }

})

server.listen(3000)

console.log('server on http://localhost:3000')
```