# Note app

## Packages `npm` intéressants

### `validator`

Permet de valider tout un tas de chose :

```js
const validator = require("validator");

validator.isEmail("karim@hibou.kikikiki"); //? true
validator.isURL("http://www.titi.com/karim/hiboukikikiki"); //? true
```

### `chalk`

Permet de colorer la sortie de `console.log`

```js
const chalk = require("chalk");

console.log(chalk.red("hello", chalk.green("world!")));
console.log(chalk.inverse("hey don't do that"));
console.log(
  chalk.keyword("pink").inverse("you know"),
  chalk.keyword("pink").inverse.bold("me!!!!")
);
```

<img src="assets/Screenshot2020-04-23at17.00.35.png" alt="Screenshot 2020-04-23 at 17.00.35" style="zoom:25%;" />

`process.argv` : arguments vector

```js
console.log(process.argv);
```

## `yargs`

```js
const yargs = require("yargs");

console.log(process.argv);
console.log(yargs.argv);
```

```bash
node app add --title="fist note"
[
  '/usr/local/bin/node',
  '/Users/kar/Documents/programmation/node/node-andrew-mead/notes-app/app',
  'add',
  '--title=fist note'
]
{ _: [ 'add' ], title: 'fist note', '$0': 'app' }
```

Par défaut `yargs` propose `--help` et `--version`

```bash
node app --version
1.0.0

node app --help
Options:
  --help     Show help                                                 [boolean]
  --version  Show version number
```

## Régler la version

### `yargs.version(<new version>)`

```js
//customize yargs version
yargs.version("1.1.6");
```

```bash
./app.js --version
1.1.6
```

## Créer une commande

### `yargs.command({ <options> })`

```js
// Create command add
yargs.command({
  command: "add",
  describe: "Add a new note",
  handler: function () {
    console.log("adding a new note");
  },
});
```

```bash
./app.js add
adding a new note
```

### Ajouter des options

```js
// Create command add
yargs.command({
  command: "add",
  describe: "Add a new note",
  builder: {
    title: {
      describe: "Note title",
    },
  },
  handler: function (argv) {
    console.log("adding a new note", argv);
  },
});
```

```bash
./app.js add --title="first note"
adding a new note { _: [ 'add' ], title: 'first note', '$0': 'app.js' }
```

Si je ne donne pas d'option, cela fonctionne quand même.

### option obligatoire `demandOption`

On veut rendre l'option `title` obligatoire `required` :

```js
// Create command add
yargs.command({
  command: "add",
  describe: "Add a new note",
  builder: {
    title: {
      describe: "Note title",
      demandOption: true, // option requiried by default false
    },
  },
  handler: function (argv) {
    console.log("adding a new note", argv);
  },
});
```

```bash
./app.js add
app.js add

Add a new note

Options:
  --help     Show help                                                 [boolean]
  --version  Show version number                                       [boolean]
  --title    Note title                                               [required]

Missing required argument: title
```

### Type de l'option

Si une option est renseignée toute seule, elle est considérée de type booléen :

```bash
./app.js add --title
adding a new note { _: [ 'add' ], title: true, '$0': 'app.js' }
```

On veut forcer le type de l'option :

```js
// Create command add
yargs.command({
  command: "add",
  describe: "Add a new note",
  builder: {
    title: {
      describe: "Note title",
      demandOption: true, // option requiried by default false
      type: "string",
    },
  },
  handler: function (argv) {
    console.log("adding a new note", argv);
  },
});
```

```bash
./app.js add --title
adding a new note { _: [ 'add' ], title: '', '$0': 'app.js' }
```

`title` est un bien un `string` vide.

Pour l'instant on laisse une ligne à la fin du script :

```js
console.log(yargs.argv);
```

Si on retire cette ligne bizarrement le script ne fonctionne plus.

en fait `yargs.argv` enclenche le _parsage_ de la ligne de commande.

Comme on a pas besoin d'un `console.log` en plus on peut le remplacer par :

```js
yargs.argv;
```

ou plus élégant, de demander tout simplement à `yargs` de _parser_ :

```js
yargs.parse();
```

> ## aparté json
>
> ```js
> const book = {
>   title: "Alice",
>   author: "Lewis",
> };
>
> const bookJson = JSON.stringify(book, null, 4);
>
> console.log(bookJson);
> ```
>
> `4` étant la taille de l'indentation, `null` l'emplacement de la fonction de remplacement.
>
> ```bash
> {
>     "title": "Alice",
>     "author": "Lewis"
> }
> ```
>
> À la sortie on obtient une jolie indentation et des doubles guillemets sur le nom des propriétés.
>
> On peut l'écrire dans un fichier :
>
> ```js
> const fs = require("fs");
>
> // ...
>
> fs.writeFileSync("./books.json", bookJson);
> ```
>
> ### De même pour lire :
>
> ```js
> const dataBuffer = fs.readFileSync("./books.json");
>
> const books = JSON.parse(dataBuffer);
> ```
>
> `fs.readFileSync` renvoie un `buffer` de données :
>
> ```bash
> <Buffer 5b 0a 20 20 20 20 7b 0a 20 20 20 20 20 20 20 20 22 74 69 74 6c 65 22 3a 20 22 41 6c 69 63 65 22 2c 0a 20 20 20 20 20 20 20 20 22 61 75 74 68 6f 72 22 ... 84 more bytes>
> ```

## Programmation défensive : récupérer les notes de `notes.json`

### avec `existSync(<path>):boolean`

```js
function _loadNotes() {
  let notes;
  if (fs.existsSync("notes.json")) {
    const dataBuffer = fs.readFileSync("notes.json");
    notes = JSON.parse(dataBuffer);
  } else {
    notes = [];
  }

  return notes;
}
```

`existsSync` vérifie l'existence d'un fichier, ici `notes.json`.

### avec `try / catch`

```js
function _loadNotes() {
  try {
    const dataBuffer = fs.readFileSync("notes.json");
    return SON.parse(dataBuffer);
  } catch (e) {
    return [];
  }
}
```
