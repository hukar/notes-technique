# initialiser un projet

`npm init` dans le dossier du projet 
On répond à des questions 
cela génère le fichier package.json

```js
{
  "name": "expressmovie",
  "version": "1.0.0",
  "description": "movie app fans",
  "main": "app.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "keywords": [
    "movie",
    "cinema",
    "fan"
  ],
  "author": "Karim Meshoub",
  "license": "ISC"
}
```

>mise à jour de npm

>```sh
>sudo npm i -g npm
>```

## installer express


```sh
npm install express --save
```

`--save` permet l'ajout d'une entrée dans le fichier **package.json**

```js
{
  "name": "expressmovie",
  
  			...
  			
  "license": "ISC",
  "dependencies": {
    "express": "^4.15.4"
  }
}
```

var est tombé en désuétude :  
si la valeur ne change pas `const maConstante = ...`  
sinon `let maVar = ...`

# première application

fichier app.js

```js
const express = require('express')
const app = express()

const PORT = 3000

app.get('/',(req, res) => {
// affiche le message sur le navigateur à l'adresse localhost:3000
    res.send('hello <b>kiki</b>')
})

app.listen(PORT, () => {
// affiche le message dans le terminal
    console.log(`on écoute le port ${PORT}`)
})
```

On lance l'application sur le terminal

```sh
node app.js
```

On envoie du text/html, donc on peut utiliser le `<b>` de l'exemple.

# nodemon

Pour ne pas avoir à relancer node à chaque changelent du code

```sh
sudo npm install -g nodemon
```

-g obligatoire pour un outil en ligne de commande (globally)

Désinstaller un module global

```sh
sudo npm uninstall - g nodemon
```

installer la dépendance dans package.json uniquement en développement:

```sh
npm install nodemon --save-dev
```

package.json

```js
{
  "name": "expressmovie",
  
 			...
 			
  "license": "ISC",
  "dependencies": {
    "express": "^4.15.4"
  },
  "devDependencies": {
    "nodemon": "^1.12.1"
  }
}
```
On lance l'application maintenant non plus avec `node app.js`
 mais avec `nodemon app.js`

# npm start

Pour lancer une application on ne connait pas forcement le nom du fichier serveur node, on met donc un script de lancement dans le fichier package.json

```js
{
  "name": "expressmovie",
  "version": "1.0.0",
  "description": "movie app fans",
  "main": "app.js",
  // on ajoute ici une entrée "start"
  "scripts": {
    "start": "node app.js",
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  
  				...
  
}

```

On peut maintenant taper

```sh
npm run start
# il existe un alias
npm start
```

Idem si en développement on veut lancer nodemon :  

On modifie **package.json**

```js
script: {
	"start": "node app.js",
	"test": "echo \"error\" && exit 1,
	"dev": "nodemon app.js"
}
```

Et pour l'exécuter `npm run dev` 

# routage

routage simple :

```js
app.get('/movies', (req, res) => {
    res.send('ici bientôt des films')
})
```

routage avec paramètre :

```js
app.get('/movies/:id', (req, res) => {
	// on récupère le paramètre
    const id = req.params.id
    res.send(`le film d'ID : ${id}`)

})
```

## attention à l'ordre des routes

```js
app.get('/movies/add', (req, res) => {

    res.send(`<form method="get">
                titre :<input type="text" name="title"/><br />
                réalisateur : <input type="text" name="director"/><br />
                <p>
                    <button type="reset">effacer</button>
                    <button>soumettre</button>
                </p>
            </form>`
        )
})

app.get('/movies/:id', (req, res) => {
    const id = req.params.id
    res.send(`le film d'ID : ${id}`)
})
```
Si on fait l'inverse **'add'** sera concidéré comme le paramètre id et on n'arrivera jamais à `get('/movies/ass', ...)`