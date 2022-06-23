# NPM

## fichier package.json

On initialise un projet avec `npm init` npm va alors créer un fichier package.json adapté à son projet.

Le package.json va être un point d'entrée global.

package.json

```js
{
  "name": "nodejs-linkedin",
  "version": "1.0.0",
  "description": "Une formation à nodejs",
  "main": "app.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "Karim Meshoub",
  "license": "ISC"
}
```

## installer Express avec npm

```sh
npm install express --save
```

`--save` enregistre express comme une dépendance dans le fichier package.json

```js
{
  "name": "nodejs-linkedin",
  "version": "1.0.0",
  "description": "Une formation à nodejs",
  "main": "app.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "Karim Meshoub",
  "license": "ISC",
  "dependencies": {
    "express": "^4.15.4"
  }
}
```

Le dossier node_modules est créer avec express dedans

# Express

## le routage

```js
var express = require('express')
var fs = require('fs')
var app = express();

// répertoire statique dans le quel les fichiers peuvent être lus
// ex: index.html, style.ccs
app.use(express.static('./public'))


// cacher le chemin vers les css
app.get('/kiki.css', (req, res) => {
    fs.readFile('./graphisme/pilou.css', (error, css) => {
        res.end(css)
    })
})
app.get('/hello', function (req, res) {
  res.send('Hello World!')
})

// définir le routage en post
app.post('/', function (req, res) {
    res.send('<strong>formulaire envoyé</strong>')
  })

app.get('/pipi', function (req, res) {
    res.send('Hello pipi!')
  })

app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
```

## récupération de données - middleware

Insatallation de **body-parser** 

```sh
npm install body-parser --save
```

```js
var express = require('express')
var bodyParser = require('body-parser')
var fs = require('fs')
var app = express();

app.use(bodyParser.urlencoded())
app.use(express.static('./public'))

app.get('/kiki.css', (req, res) => {
    fs.readFile('./graphisme/pilou.css', (error, css) => {
        res.end(css)
    })
})
app.get('/hello', function (req, res) {
	// ici on récupére les données de la requête dans l'URL
	// n'utilise pas body-parser
    console.log(req.query)
  res.send('Hello World!')
})

app.post('/', function (req, res) {

	// ici on récupère les données en parsant le body
	// grâce à body-parser
    var data = req.body
    res.send(`
        <strong>formulaire envoyé : </strong><br>
        <p>nom : ${data.userName}</p>
        <p>password : ${data.password}</p>
        <p>date : ${data.date}</p>
        `)
  })

app.get('/pipi', function (req, res) {
    res.send('Hello pipi!')
  })

app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
```