# Objet global

Node fonctionne en module.

Il ya un objet global : `global`

```js
// définie pour le module
var kiki = 'kiki'

//définie de manière global
global.msg = 'pipipipi'

console.log(global.msg)
console.log(msg)

> pipipipi
> 'pipipipi'
```

Connaitre le répertoire et le fichier courant :

```js
console.log(__dirname)
console.log(__filename)

> /Users/hukar/Dropbox/nodejs-linkedin
> /Users/hukar/Dropbox/nodejs-linkedin/server.js
```

# arguments de la ligne de commande

`process.argv` voyons ce qu'il contient déjà :
```js
console.log(process.argv)

> [ '/usr/local/bin/node',
  '/Users/hukar/Dropbox/nodejs-linkedin/server.js']
```

on récupère les argument dans process.argv

```js
// foction pour tester l'existence d'une clé
var collect = function (key) {

    var index = process.argv.indexOf(key) 
    return index === -1 ? null : process.argv[index + 1]

}

var name = collect('--name')
var message = collect('--message')
if (name && message) {
	// on peut sortir du script ici
   return console.log(`nom : ${name} message : ${message}`)
}

console.log("erreur pas assez d'arguments")
```

# les entrées / sorties

On va les trouver dans process : `process.stdout` et `process.stdin`

```js
var questions = [
    'comment tu t\'appelles-tu ?',
    'Que fais-tu dans la vie ?',
    'Quel est ton langage de programmation préféré ?'
]

var answers = []

// écrire dans le terminal 
function ask (index) {
    process.stdout.write(`\n\n${questions[index]}\n`)
}

// on câble l'événement 'data' 
// => taper un texte et pousser la touche entrée
process.stdin.on('data', function (data) {
        console.log(data)
        // data : <Buffer 6d 69 63 68 65 6c 0a>
        // trim() supprime les sauts de ligne de la console
        answers.push(data.toString().trim())
        
        if (questions.length === answers.length) {

            // on sort du process
            process.exit()
        } 
        ask(answers.length) 
 })  

ask(0)

// événement directement sur process
process.on('exit', function () {
    console.log(answers)
})
```

# travail avec un timer angry script

```js
var questions = [
    'comment tu t\'appelles-tu ?',
    'Que fais-tu dans la vie ?',
    'Quel est ton langage de programmation préféré ?'
]

var reactions = [
    'tu es là !?',
    'Allo ?',
    'bon ...'
]

var answers = []

// on sauve l'index courant dans une variable
var actualIndex

function createTimeout () {
    setTimeout(function () {
        askWithReaction()
    },3000)
}

function ask (index) {
    process.stdout.write(`${questions[index]}`)
    actualIndex = index
    createTimeout()
}

function askWithReaction () {
	// efface la ligne
    process.stdout.clearLine()
    // remet le curseur au début
    process.stdout.cursorTo(0)
    process.stdout.write(`${reactions[actualIndex]} ${questions[actualIndex]}`)
}


// on câble l'événement 'data'
process.stdin.on('data', function (data) {
        // trim() supprime les sauts de ligne de la console
        answers.push(data.toString().trim())
        
        if (questions.length === answers.length) {

            // on sort du process
            process.exit()
        } 
        ask(answers.length) 
 })  

ask(0)

// événement directement sur process
process.on('exit', function () {
    console.log(answers)
})


```

# les modules de nodejs

`require()` va nous permettre d'utiliser les modules

obtenir le nom du fichier courant :

```js
var path = require('path')
console.log(__filename)
console.log(path.basename(__filename))

> /Users/hukar/Dropbox/nodejs-linkedin/server.js
> server.js
```

## Readline

```js
var readline = require('readline')
var rl = readline.createInterface(process.stdin, process.stdout)

var questions = [
    'comment tu t\'appelles-tu ? ',
    'Que fais-tu dans la vie ? ',
    'Quel est ton langage de programmation préféré ? '
]

var person = {}

var attributes = ['name','job','lg']

var actualIndex = 0

function getQuestion () {
    return questions[actualIndex]
    if (actualIndex < (questions.length -1)) {
        actualIndex ++
    }
}
function ask () {
    rl.question(getQuestion(), function (answer) {
        person[attributes[actualIndex]] = answer
        actualIndex++
        
        if (actualIndex === attributes.length) {
            rl.close()
        }

        ask()
    })
}


// gestionnaire d'événement
rl.on('close', function () {
    console.log(person)
    process.exit()
})

ask()
```

# Création de son module

`module.exports`

questionHandler.js

```js
var readline = require('readline')
var rl = readline.createInterface(process.stdin, process.stdout)

function QuestionHandler () {
    this.actualIndex = 0
    this.questions
    this.answers = []
    this.attributes

    this.setQuestions = function (questions) {
        this.questions = questions
        return this
    }

    this.setAttributes = function (attributes) {
        this.attributes = attributes
        return this
    }
    
    this.getQuestion = function () {
        return this.questions[this.actualIndex]
    }

    this.ask = function () {
    
    	// pour conserver le this de 	
        var that = this

        rl.question(this.getQuestion(), function (answer) {
            that.answers[that.attributes[that.actualIndex]] = answer
            that. actualIndex++
            
            if (that.actualIndex === that.questions.length) {
                console.log(that.answers)
                rl.close()
            }
    
            that.ask()
        })
    }
}

rl.on('close', function () {
    process.exit()
})

module.exports = new QuestionHandler()
```

server.js

```js
var questionHandler = require('./questionHandler')

var questions = [
    'comment tu t\'appelles-tu ? ',
    'Que fais-tu dans la vie ? ',
    'Quel est ton langage de programmation préféré ? '
]

var attributes = ['name','job','lg']

questionHandler
    .setQuestions(questions)
    .setAttributes(attributes)
    .ask()
```

# child process processus enfant

Pour executer des commandes Shell

`exec`

```js
var exec = require('child_process').exec

exec('touch pipi.js;echo "bonjour lulu" > pipi.js;mkdir rognon;ls -l', function (error, stdout) {
    if (error) {
        throw error
    }

    console.log('listing finished')
    console.log(stdout)
})
```

```sh
listing finished
listing finished
total 24
-rw-r--r--  1 hukar  staff    13 30 aoû 10:01 pipi.js
-rw-r--r--@ 1 hukar  staff  1073 29 aoû 09:41 questionHandler.js
drwxr-xr-x  2 hukar  staff    68 30 aoû 10:01 rognon
-rw-r--r--@ 1 hukar  staff   246 30 aoû 10:01 server.js
```

# stopper un processus
`process.exit()`

`process.stdin.on('data', ...)` événement entrer quelque chose dans la console (appuyer sur **entrer** en fait!)

`() => {}` écriture ES6 d'une fonction anonyme

```js
setInterval(() => {
    process.stdout.write('.')
},1000)

process.stdin.on('data', () => {
    console.log('Stopped by parent')
    process.exit()
})
```

# communiquer entre processus

`spawn`

server.js

```js
var spawn = require('child_process').spawn

var child = spawn('node',['spawned.js'])

child.stdout.on('data', (data) => {
    process.stdout.write(data)
})

process.stdin.on('data', (data) => {
    child.stdin.write(data)
	
	// on éteint le processus parent seulement 
	// quand le processus enfant à fini toutes ses tâches
    child.on('exit', () => {
        process.exit()
    })
})
```

spawned.js

```js
setInterval(() => {
    process.stdout.write('.')
},1000)

process.stdin.on('data', () => {
    console.log('Stopped by parent')
    process.exit()
})
```