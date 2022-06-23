# 00 Intro à `Nodejs`

`Nodejs` est `single thread`

Il supporte la programmation concurrente via les événements et les fonctions de rappel.

`Node` est très performant pour tout ce qui est entrée/sortie (i/O)

Par contre, les traitement qui requiert une grosse charge `cpu` sont à éviter.

## `NPM`

`Node` `Package` `Manager`

Les packages sont stockés dans le dossier `node_module`

Toutes les dépendances sont listées dans le fichier `package.json`

Des script `NPM` peuvent être créé pour exécuter certaine tâches, comme faire tourner un serveur

```bash
npm init # génère un fichier package.json
npm install express # installe un paquet localement
npm install -g nodemon # installe un paquet globalement ter
```

## Installation de module

```bash
npm init # plusieurs questions à renseigner

npm install uuid
```

c'est un générateur d'Id Universel.

`package.json`

```json
{
  "name": "crash-course",
  "version": "1.0.0",
  "description": "a crash course about node",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "Hukar",
  "license": "ISC",
  "dependencies": {
    "uuid": "^3.4.0"
  }
}
```

Si on veut installer un module seulement pour le développement :

```bash
npm install --save-dev nodemon # -D idem
```

`package.json`

```json
// ...
 "author": "Hukar",
  "license": "ISC",
  "dependencies": {
    "uuid": "^3.4.0"
  },
  "devDependencies": {
    "nodemon": "^2.0.2"
  }
}
```

`nodemon` recharge automatiquement le script dans `node` à chaque sauvegarde.

## Reconstruire son application

Pour *"transporter"* son application on peut supprimer le dossier `node_modules` qui est souvent énorme, avec `package.json` et la ligne :

```bash
npm init
```

le projet sera reconstruit.

## Lancer un script

Pour lancer le fichier `index.js` :

```bash
node index
# ou bien
node index.js
```

## Module

Pour partager le code, on peut créer des modules :

`Person.js`

```js
const Person = {
    name: "John Doe",
    age: 37
};

module.exports = Person;
```

### `module.exports = Person;`

Attention c'est une assignation `=`

`index.js`

```js
const Person = require("./Person");

console.log(Person.name);
```

### `const Person = require("./Person");`

## Classe javascript

Rappel :

```js
class Person {
    constructor(name, age) {
        this.name = name;  // attention au mot clé this
        this.age = age;
    }

    sayHello() {
        return `Hello my name is ${this.name} and I am ${this.age}`;
    }  // écriture de méthode
}

module.exports = Person;
```

### `constructor(name, age) { ... }`

`index.js`

```js
const Pers = require("./Person"); // on est pas obligé de garder le même nom

const john = new Pers("John Mitchel", 45); // création d'un objet : new

console.log(john.sayHello());
```







