# 07 File System

Il faut être capable de manipuler les données binaires pour pouvoir lire un fichier.

Le module `fs` utilise un `Buffer` pour pouvoir manipuler les données binaires.

## `fs.readFileSync`

```js
const fs = require("fs");
const path = require("path");

// by default utf8
const greet = fs.readFileSync(path.join(__dirname, "greet.txt"), "utf8"); 

console.log(greet);
console.log("normal flux");
```

```bash
hello greet !! 😻 🐾 🐾 🐾
normal flux
```

Lit un fichier de manière synchrone (bloquante).

## `fs.readFile`

```js
const fs = require("fs");
const path = require("path");

const greet2 = fs.readFile(
    path.join(__dirname, "greet2.txt"),
    "utf8",
    (err, file) => {
        if (err) console.log(`[[error]] : ${err}`);
        console.log(`file can be ${file}`);
    }
);

console.log("normal flux");
```

```bash
normal flux
file can be hohoho GREET 2 is coming ✨✨ 🔥
```

Cette fois `readFile` est asynchrone `normal flux` est affiché avant le fichier.

L'erreur est passée en premier paramètre, c'est un pattern de `Node.js` :

### `error-first callback`

`null` s'il n'y a pas d'erreur, ou un objet décrivant l'erreur.

C'est un standard `Node.js` qui nous permet de savoir où est placée l'erreur.

Si on ne donne pas l'encodage `utf8`, on obtient un `Buffer` :

```js
const greet2 = fs.readFile(path.join(__dirname, "greet2.txt"), (err, file) => {
    if (err) console.log(`[[error]] : ${err}`);
    console.log(file);
});
```

```
<Buffer 68 6f 68 6f 68 6f 20 47 52 45 45 54 20 32 20 69 73 20 63 6f 6d 69 6e 67 20 e2 9c a8 e2 9c a8 20 f0 9f 94 a5>
```

Si plusieurs fichier sont ouverts, il seront placés dans le `heap` de `V8`, il peut y avoir une surcharge de la mémoire.

Un mécanisme doit être mis en place, c'est là qu'interviennent les `streams`.

## Création asynchrone d'un dossier et définition de ses droits

```js
const http = require("http");
const path = require("path");
const fs = require("fs");
const url = require("url");

// url dossier publique
const pubPath = path.join(__dirname,"pub");

const mkPub = async () => {
    if(!fs.existsSync(pubPath)) {
        await fs.mkdir(pubPath, () => {
            console.log(`${path.basename(pubPath)} was created`);
        });
        await fs.chmod(pubPath, 0O444, () => {
            console.log(`${path.basename(pubPath)} has is permission setted to 444`);
        });
    } else {
        console.log(`${path.basename(pubPath)} already exist`);
    }
    
    return "operation finished";
}

const server = http.createServer(
    (req, res) => {
        // count the number of request
        server.count++;
        console.log("count :", server.count);
        
		// créer le repertoire pub en lecture seule s'il nexiste pas
        mkPub().then(console.log); // display "operation finished"

        res.end("hello coco");
    }
);
server.count = 0;

const PORT = process.env.NODE_PORT || 6666;
server.listen(PORT, console.log(`server running on [${PORT}]`));
```

`fs.existsSync(pubPath)` regarde de manière synchrone si un fichier ou un dossier existe.

`async` défini une fonction asynchrone qui renvoie une `promise`, 

sa valeur de retour est passer comme data de la `promise`.

```js
const mkPub = async () => {
    // ...
    
    return "operation finished";
}

mkPub().then(console.log); // display "operation finished"
```



`path.basename(pubPath)` pour `dir/pub/titi.txt` renvoie `titi.txt`.

`await` permet d'attendre qu'un traitement asynchrone soit terminé.

`fs.chmod(path, octal,callback)` fixe les permissions d'un dossier/fichier sur base d'un numéro en octal.

`0O444` 444 en octal.

`server.count` variable static de `server` pour compter le nombre de requête reçues.

`process.env.NODE_PORT` permet d'accéder aux variables d'environnement.

Ici `NODE_PORT` est défini dans `.bash_profile`

```bash
export NODE_PORT=4546
```

## Supprimer des fichiers `unlink` et `unlinkSync`

```js
fs.unlink("./output.txt", err => {
    if(err) {
        return console.log("erreur :", err);
    }
    console.log(`delete ${"./output.txt"}`);    
});

// version synchrone
try {
  fs.unlinkSync('/tmp/hello');
  console.log('successfully deleted /tmp/hello');
} catch (err) {
  // handle the error
}
```



## Lire un répertoire `fs.readdir`

`.readdir` renvoie une erreur ou un tableau de noms de fichier.

`filtered.js`

```js
const fs = require("fs");
const path = require("path");

module.exports = function displayFilteredFiles(pathDir, ext, callback) {
  fs.readdir(pathDir, (err, files) => {
    if (err) {
      return callback(err);
    }

    const filteredFiles = files.filter(
      file => path.extname(file) === "." + ext
    );

    callback(null, filteredFiles);
  });
}
```

`path.extname` renvoie l'extension avec `.` Devant : `.jpg` par exemple.

On respecte le pattern `error first`

```js
function bar(callback) {
    function foo(err, data) {
        if(err) {
            return callback(err);
        }
        
        callback(null, data);
    }
}
```

#### `fs.readdir(path, callback)`

`program.js`

```js
const filtered = require("./filtered");

const pathDir = process.argv[2] || "./loggy";
const extension = process.argv[3] || "png";

filtered(pathDir, extension, (err, filteredFiles) => {
  if (err) {
    return console.log("my error:", err);
  }
  filteredFiles.forEach(file => {
    process.stdout.write(file + "\n");
  });
});
```

```bash
$ node program6.js logg jpg
my error: [Error: ENOENT: no such file or directory, scandir 'logg'] {
  errno: -2,
  code: 'ENOENT',
  syscall: 'scandir',
  path: 'logg'
}
```

L'eereur est gérée proprement.

```bash
$ node program6.js loggy jpg
loulou.jpg
nafnaf.jpg
```

