# système de fichier
## Contenu du répertoire courant `./`
### Traitement Synchrone

```js
var fs = require('fs')

var files = fs.readdirSync('./')

console.log(files)
console.log('read')
```

```sh
[ 'nodejs_linkedinln_2.md',
  'pipi.js',
  'questionHandler.js',
  'rognon',
  'server.js',
  'spawned.js' ]
read
```

### Traitement Asynchrone

```js
var fs = require('fs')

fs.readdir('./', (error, files) => {
    console.log(files)
})

console.log('read')
```
Le *read* est écrit avant en asynchrone

```sh
read
[ 'nodejs_linkedinln_2.md',
  'pipi.js',
  'questionHandler.js',
  'rognon',
  'server.js',
  'spawned.js' ]
```

# Statistique des fichiers

```js
var fs = require('fs')

fs.readdir('./', (error, files) => {
    files.forEach((fileName) => {
        var stat = fs.statSync(fileName)
        
        // si le fichier n'est pas un répertoire
        if (stat.isFile()) {
            console.log(fileName)
            console.log(stat)
        } else {
            console.log(fileName)
            console.log('pas de stat!!')
        }
    })
})
```

```sh
questionHandler.js
Stats {
  dev: 16777220,
  mode: 33188,
  nlink: 1,
  uid: 501,
  gid: 20,
  rdev: 0,
  blksize: 4096,
  ino: 32817233,
  size: 1073,
  blocks: 8,
  atimeMs: 1504079383000,
  mtimeMs: 1503992483000,
  ctimeMs: 1503992483000,
  birthtimeMs: 1503990684000,
  atime: 2017-08-30T07:49:43.000Z,
  mtime: 2017-08-29T07:41:23.000Z,
  ctime: 2017-08-29T07:41:23.000Z,
  birthtime: 2017-08-29T07:11:24.000Z }
rognon
pas de stat!!
```

# Afficher le contenu des fichiers du répertoire courant

`fs.readFile(fileName, 'utf-8', (error, content) => { ... })`

```js
var fs = require('fs')

fs.readdir('./', (error, files) => {
    files.forEach((fileName) => {
        fs.readFile(fileName, 'utf-8', (error, content) => {
            console.log('--------------------------------')
            console.log(fileName)
            console.log('---------')
            console.log(content)
            console.log('--------------------------------')
        })
      
    })
})
```
```sh
--------------------------------
rognon
---------
undefined
--------------------------------
--------------------------------
server.js
---------
var fs = require('fs')

fs.readdir('./', (error, files) => {
    files.forEach((fileName) => {
        fs.readFile(fileName, 'utf-8', (error, content) => {
            console.log('--------------------------------')
            console.log(fileName)
            console.log('---------')
            console.log(content)
            console.log('--------------------------------')
        })

    })
})


--------------------------------
```

# Créer un fichier 

`fs.writeFile('monText.txt', content, (error, file) => { ... })`

```js
var fs = require('fs')

var content = 'koko'

// utiliser trim pour avoir un fichier propre
fs.writeFile('text.txt', content.trim(), (error, file) => {
    console.log('file created')
})
```

## ajouter du texte à un fichier

`appendFile`
Si le fichier n'existe pas, il sera créé

```js
var fs = require('fs')

var content = '\nkiki'

fs.appendFile('text.txt', content, (error, file) => {
    console.log('file created')
})
```

# créer un répertoire

`fs.mkdir('lib', (error) => { ... })`

```js
var fs = require('fs')

fs.mkdir('lilou', (error) => {
    if (error) {
        console.log(error)
    } else {
        console.log('directory created')
    }
})
```

Pour supprimer un répertoire en shell :

```sh
rm -d lilou
```

## vérifier d'abord l'existence d'un répertoire avant d'essayer de le créer

`fs.existsSync('nomDeDossier')`

```js
var fs = require('fs')


if (fs.existsSync('lilou')) {
    console.log('hello lilou')
} else {
    console.log('pas de lilou')
    fs.mkdir('lilou', (error) => {
        if (error) {
            console.log(error)
        } else {
            console.log('directory created')
        }
    })
}
```

# renommer et supprimer des fichiers
## renommer
Plutôt faire du synchrone

`fs.renameSync('old.txt','new.txt')`

```js
var fs = require('fs')

// vérifier si le fichier existe
if (fs.existsSync('text.txt')) {
	// vérifier que le nouveau nom n'écrase pas un fichier déjà existant
    if (fs.existsSync('coco.txt')) {
        console.log('le fichier existe déjà : conflis')
    } else {
        fs.renameSync('text.txt','coco.txt')
    }
    
} else {
    
    console.log('LE FICHIER N\'EXISTE PAS')
}
```

## supprimer

`fs.unlinkSync('monFichier.txt')`

```js
var fs = require('fs')

if (fs.existsSync('titi.txt')) {
    fs.unlinkSync('titi.txt')
}

// ou bien avec try & catch
var fs = require('fs')

try {
    fs.unlinkSync('titi.txt')
} catch(error) {
    console.log(`erreur de suppression de fichier : ${error.path}`)
}
```
```sh
erreur de suppression de fichier : titi.txt
```

## renommage et déplacement en même temps

```js
fs.renameSync('lilou/bibi','./bibou')
```
```sh
lilou-|
      bibi
      
transformé en :
      
lilou
bibou
```
## supprimer un dossier

utilisation de la récursivité

removeDirectory.js

```js
var fs = require('fs')
function RemoveDirectory () {   

    this.setDirectory = (dir) => {
        this.dir = dir  
    }

    //fs.rmdirSync()
    this.remove = () => {
        var stat

        (function walkedDirectory (path) {

            fs.readdirSync(path).forEach((fileName) => {
                stat = fs.statSync(`${path}/${fileName}`)
    
                if (!stat.isFile()) {
                		// on traite le dossier
                    walkedDirectory(`${path}/${fileName}`)
                } else {
                		// on efface le fichier
                    fs.unlinkSync(`${path}/${fileName}`)
                }
            })
            // on efface le dossier traité
            fs.rmdirSync(path)
        })(this.dir)
    }
}

module.exports = new RemoveDirectory()
```

server.js

```js
var rd = require('./removeDirectory')

rd.setDirectory('lilou')

rd.remove()
```

`fs.unlinkSync(file)` efface un fichier
`fs.rmdirSync(dir)` efface un dossier vide

# lecture en stream des très gros fichiers

`fs.createReadStream('fichier.txt')`

```js
var fs = require('fs')

var stream = fs.createReadStream('long-text.txt', 'utf-8')

stream.once('data', () => {
    console.log('start')
})

stream.on('data', (chunk) => {
	// chunk est un morceau du fichier
    process.stdout.write(`chunk : ${chunk.length}\n`)
})

stream.on('end', () => {
    console.log('finish')
})

```

## un stream en écriture

```js
var fs = require('fs'),
     readline = require('readline'),
     rl = readline.createInterface(process.stdin, process.stdout),
     stream,
     pseudo

function addToStream () {
    var question = 'nom du chat ? '

    if (stream) {
        question = `${pseudo}`
    }

    rl.question(question, (data) => {
        if (!stream) {
            stream = fs.createWriteStream(`${data}.txt`)
            pseudo = data
        } else {
        	// écrit au fur et à mesure dans le fichier ${data}.txt
            stream.write(`${pseudo} : ${data}\n`)
        }

        addToStream()
    })
}

addToStream()
```