# 03 L'objet `global`, `process` et `buffer`

## L'objet `global`

`index.js`

```js
require("./utils");

console.log(answer);
```
`utils.js`

```js
const answer = 42;
```

```bash
$ node index.js
/Users/kms/Documents/programmation/node/node-advanced-buna/index.js:3
console.log(answer);
            ^

ReferenceError: answer is not defined
```

En utilisant l'objet `global` :

```js
global.answer = 42;
```

```bash
$ node index.js
42
```

Dans le `repl` l'objet `global` contient toutes les librairies de `Node.js`

![Screenshot 2020-03-10 at 16.30.31](assets/Screenshot 2020-03-10 at 16.30.31.png)

Dans un script normal seulement deux librairies sont par défaut dans l'objet `global` :

-  `buffer`
- `process`

Les autres librairies doivent être importées avec `require`.

### `process`

Donne des informations sur le système où tourne `Node`.

### `process.version`

```bash
$ node -p "process.versions"
{
  node: '12.13.0',
  v8: '7.7.299.13-node.12',
  uv: '1.32.0',
  zlib: '1.2.11',
  brotli: '1.0.7',
  ares: '1.15.0',
  modules: '72',
  nghttp2: '1.39.2',
  napi: '5',
  llhttp: '1.1.4',
  http_parser: '2.8.0',
  openssl: '1.1.1d',
  cldr: '35.1',
  icu: '64.2',
  tz: '2019a',
  unicode: '12.1'
}
```

### `process.env`

```bash
$ node -p "process.env"
{
  SHELL: '/usr/local/bin/bash',
  XPC_FLAGS: '0x0',
  TERM_PROGRAM_VERSION: '433',
  CONDA_EXE: '/Users/kms/opt/anaconda3/bin/conda',
  _CE_M: '',
  SSH_AUTH_SOCK: '/private/tmp/com.apple.launchd.ajgamaxWuV/Listeners',
  TERM_SESSION_ID: 'E717C795-FEB3-44D9-97DA-466A4E12BAEC',
  PWD: '/Users/kms',
  LOGNAME: 'kms',
  HOME: '/Users/kms',
  TMPDIR: '/var/folders/l5/lpyzr4mx4yl105m2y9zwwk7h0000gn/T/',
  CLICOLOR: '1',
  NODE_PORT: '4546',
  TERM: 'xterm-256color',
  _CE_CONDA: '',
  USER: 'kms',
  CONDA_SHLVL: '0',
  DISPLAY: '/private/tmp/com.apple.launchd.3W0kOfcE8C/org.macosforge.xquartz:0',
  SHLVL: '1',
  XPC_SERVICE_NAME: '0',
  CONDA_PYTHON_EXE: '/Users/kms/opt/anaconda3/bin/python',
  LC_CTYPE: 'UTF-8',
  PS1: '\\u: \\W $ ',
  PATH: '/Users/kms/opt/anaconda3/condabin:/Library/Frameworks/Python.framework/Versions/3.8/bin:/Users/kms/Library/sonar-scanner-4.0.0.1744-macosx/bin:/Users/kms/Library/Python/3.8/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/usr/local/share/dotnet:/opt/X11/bin:~/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands:/Applications/Xamarin Workbooks.app/Contents/SharedSupport/path-bin',
  OLDPWD: '/Users/kms/Documents/programmation/mongo',
  TERM_PROGRAM: 'Apple_Terminal',
  _: '/usr/local/bin/node',
  __CF_USER_TEXT_ENCODING: '0x1F5:0x0:0x0'
}
```

On obtient la même chose en tapant `env` dans le terminal :

```bash
$ env
SHELL=/usr/local/bin/bash
XPC_FLAGS=0x0
TERM_PROGRAM_VERSION=433
CONDA_EXE=/Users/kms/opt/anaconda3/bin/conda
_CE_M=
SSH_AUTH_SOCK=/private/tmp/com.apple.launchd.P0JsLs1vLc/Listeners
TERM_SESSION_ID=E717C795-FEB3-44D9-97DA-466A4E12BAEC
PWD=/Users/kms
LOGNAME=kms
LaunchInstanceID=1188FB7E-3D8D-4CB7-810E-FF2FAE6CA0AA
HOME=/Users/kms
SECURITYSESSIONID=186a6
TMPDIR=/var/folders/l5/lpyzr4mx4yl105m2y9zwwk7h0000gn/T/
CLICOLOR=1
NODE_PORT=4546
TERM=xterm-256color
_CE_CONDA=
# ...
```



On ne peut pas modifier la valeur des attributs de `process.env` :

```bash
 $ node -p "process.env.NODE_PORT= 5555"
5555

$ echo $NODE_PORT
4546
```

### Principe du fichier de configuration

On ne lit pas directement dans son programme les valeurs de `process.env`, mais on le fait dans le fichier de configuration.

`config.js`

```js
module.exports = {
    port: process.env.NODE_PORT || 6666
};
```

`index.js`

```js
const config = require("./config");

console.log(config.port);
```

```bash
$ node index.js 
4546
```

### `process.release.lts`

```bash
$ node -p "process.release.lts"
Erbium
```

Le nom de la `release`.

Si `Node.js` n'est pas dans une Long Time Support Version, `process.release.lts` renvoie `undefined`.

### `process.stdin` et `process.stdout`

```js
process.stdin.on("readable", () => {
    let chunk;
    while ((chunk = process.stdin.read()) !== null) {
        process.stdout.write(`data : ${chunk}`);
    }
});

process.stdin.on("end", () => {
    process.stdou.write("end");
});
```

Ce sont des `streams` , il existe aussi `process.stderr`.

## `process` est une instance de `event emitter` 

L'événement `exit` :

Cette événement est déclenché lorsque l'`event loop` n'a plus rien à faire, ou que manuellement on on appelle `exit`.

L'événement `uncaughtException` :

Quand une exception n'est pas attrapé.



```js
process.on("exit", code => {
    // exécute encore une action synchrone
    // avant que le processus de node se termine
});

process.on("uncaughtException", err => {
    // quelque chose n'a pas été géré
    // Faire le nettoyage et de toute façon sortir (exit)

    console.error(err);
});

// on occupe l'event loop
process.stdin.resume();

// Généré un TypeError exception
console.dog();
```

Si on ne fait rien de plus le processus `node` continu dans un état indéfini.

Il faut aussi arrêter le processus :

```js
const fs = require("fs");

process.on("exit", code => {
    // exécute encore une action synchrone
    // avant que le processus de node se termine
    fs.writeFileSync(`./log_${Date.now()}`, code, { encoding: "utf8" });
});

process.on("uncaughtException", err => {
    // quelque chose n'a pas été géré
    // Faire le nettoyage et de toute façon sortir (exit)

    console.error(err);

    //On force la sortie
    process.exit(678);
});

// on occupe l'event loop
process.stdin.resume();

// Généré un TypeError exception
console.dog();

```

Un fichier (en synchrone) est généré à chaque sortie avec le code envoyé par `.exit()`.

## La classe `Buffer`

```bash
$ node
Welcome to Node.js v12.13.0.
Type ".help" for more information.
> Buffer
[Function: Buffer] {
  poolSize: 8192,
  from: [Function: from],
  of: [Function: of],
  alloc: [Function: alloc],
  allocUnsafe: [Function: allocUnsafe],
  allocUnsafeSlow: [Function: allocUnsafeSlow],
  isBuffer: [Function: isBuffer],
  compare: [Function: compare],
  isEncoding: [Function: isEncoding],
  concat: [Function: concat],
  byteLength: [Function: byteLength],
  [Symbol(kIsEncodingSymbol)]: [Function: isEncoding]
}
```

Est utilisé pour travailler avec des `streams` de données binaires.

C'est un morceau de mémoire allouée à l'extérieur du `heap` de `v8`.

La taille est déterminée à la création et ne peut plus être changée après.

Un `Buffer` contient des données binaires, pour être lu, on doit préciser l'encodage.

C'est un tableau d'entier (entre 0 et 255) de 1 byte.

<table><tr><td>0</td><td>0</td><td>1</td><td>1</td><td>0</td><td>1</td><td>0</td><td>1</td></tr></table>

## Création de `Buffer`

### `Buffer.alloc(size)`

Crée un `buffer` de la taille spécifiée en bytes.

Le rempli avec des zéros.

```bash
> Buffer.alloc(8)
<Buffer 00 00 00 00 00 00 00 00>
```

### `Buffer.allocUnsafe(size)`

Ne rempli pas le `buffer` avec des zéros.

```bash
> Buffer.allocUnsafe(8)
<Buffer 10 7a 94 02 01 00 00 00>
```

### `.fill([number])`

Remplie le `buffer` avec zéro par défaut ou la valeur spécifiée.

```bash
> Buffer.allocUnsafe(8).fill()
<Buffer 00 00 00 00 00 00 00 00>
> Buffer.allocUnsafe(8).fill(255)
<Buffer ff ff ff ff ff ff ff ff>
```

#### ! `.allocUnsafe` garde ce qui été écris dans la mémoire

`.toString` par défaut `utf8`.

```bash
> Buffer.allocUnsafe(200).toString()
'32LE\u0001\u0000\u0000\u0000(�\u0002\u0003\u0001\u0000\u0000\u0000XC\u0006\u0003\u0001\u0000\u0000\u0000\f\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u000e*&�\u0001\u0001\u0000\u0000�f\u0000\u0000�\u0001\u0000\u0000hC\u0006\u0003\u0001\u0000\u0000\u0000�f\u0000\u0000+\u0000\u0000\u0000@C\u0006\u0003\u0001\u0000\u0000\u0000�C\u0006\u0003\u0001\u0000\u0000\u0000\tg\u0000\u00005\t\u0000\u0000`y\u0006\u0003\u0001\u0000\u0000\u0000�C\u0006\u0003\u0001\u0000\u0000\u0000\u0007g\u0000\u0000\u0018\b\u0000\u0000�C\u0006\u0003\u0001\u0000\u0000\u0000�C\u0006\u0003\u0001\u0000\u0000\u0000�f\u0000\u0000\t\u0000\u0000\u0000�C\u0006\u0003\u0001\u0000\u0000\u0000\u0019g\u0000\u00005\t\u0000\u0000�<\u0006\u0003\u0001\u0000\u0000\u0000`D\u0006\u0003\u0001\u0000\u0000\u0000readUInt16LE�\u0001\u0000\u00000�\u0002\u0003\u0001\u0000\u0000\u0000\bD\u0006\u0003\u0001\u0000\u0000\u0000'
```

On trouve ce texte `readUInt16LE` par exemple.

### `Buffer.from(something)`

```js
const string = "touché";
const buffer = Buffer.from("touché");

console.log(string, string.length);
console.log(buffer, buffer.length);
```

```bash
touché 6
<Buffer 74 6f 75 63 68 c3 a9> 7
```

Comme le caractère accentué `é` est codé sur deux bytes, le buffer qui compte en byte est d'une longueur de 7.

On peut faire des manipulations sur les `buffer` comme sur les tableaux mais avec un résultat différent.

## Opération sur les `buffers` : `.slice`

> rappel javascript
>
> ```js
> const arr = [1,2,3,4];
> 
> const newArr = arr.slice(-3, -1); //? [2,3] (premier inclus, deuxième exclus)
> 
> newArr[1] = 0;
> newArr; //? [2,0]
> arr; //? [1,2,3,4]
> ```

Par contre avec les `buffers` :

```js
const fs = require("fs");

const conversionMap = {
    "88": "65",
    "89": "66",
    "90": "67"
};

fs.readFile(__filename, (err, buffer) => {
    const tail = buffer.slice(-4, -1);

    for (let i = 0; i < tail.length; i++) {
        tail[i] = conversionMap[tail[i]];
    }

    console.log(buffer.toString());
});

// TAG: XYZ

```

```bash
$ node buffer_slice.js 
const fs = require("fs");
# ...

// TAG: ABC

$ 
```

Le `slice ` référence la même partie de la mémoire, le `buffer` originel est donc modifié.

## `StringDecoder`

Le module `string_decoder` fournit une `API` pour le décodage des objets `buffer` en chaînes de caractères de manière à préserver les caractères codés multi-octets `UTF-8` et `UTF-16`.

`toString` n'attend pas la suite des octets dans le stream pour le décodage.

```js
const { StringDecoder } = require("string_decoder");
const decoder = new StringDecoder("utf8");

process.stdin.on("readable", () => {
    let chunk;

    while ((chunk = process.stdin.read()) !== null) {
        const buffer = Buffer.from([chunk]);

        console.log("with .toString", buffer.toString());
        console.log("with StringDecoder", decoder.write(buffer));
    }
});
```

```bash
0xE2
with .toString �
with StringDecoder 
0x82
with .toString �
with StringDecoder 
0xAC
with .toString �
with StringDecoder €
```

On voit que `toString` essaye à chaque fois de décoder sans succès le binaire reçu, tandis que `StringDecoder` attend d'avoir tout les bits et décode avec justesse le caractère `€` codé sur trois byte.

#### ! on doit toujours préférer `StringDecoder`.

