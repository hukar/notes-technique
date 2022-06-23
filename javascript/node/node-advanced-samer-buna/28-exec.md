# 28 `exec`

Lance un `shell`.

```js
const { exec } = require("child_process");

exec("find . -type f | wc -l", (err, stdout, stderr) => {
    console.log(`Number of lines : ${stdout}`);
});
```

On peut donc utiliser les opérateur du `shell` comme `|`.

Cette méthode `buffer` la totalité du résultat et le passe à une `callback`.

Si le résultat est courte `exec` ne pose pas de problème, par contre si le résultat est conséquent, il vaut mieux utiliser `spawn` qui le traîtera en `stream`.

##  Même exemple avec `spawn` : Les options

On peut faire hériter la sortie standard à `spawn` : `stdio: 'inherit'`.

```js
const { spawn } = require("child_process");

const find = spawn("find", [".", "-type", "f"], {
    stdio: 'inherit'
});
```

```bash
node other-spawn.js 
./exec.js
./other-spawn.js
./test.js
./find-result.txt
./spawn-pipe.js
./coco.txt
./find-wc.js
./spawn.js
```

### Le meilleur des deux mondes (`shell` et pas `buffuré`)

`shell: true`

```js
const { spawn } = require("child_process");

const find = spawn("find . -type f | wc", {
    stdio: "inherit",
    shell: true
});
```

```bash
node other-spawn.js 
       8       8     106
```

Si besoin on peut raccourcir à :

```js
const { spawn } = require("child_process");

spawn("find . -type f | wc", {
    stdio: "inherit",
    shell: true
});

```

```bash
node other-spawn.js 
       8       8     106
```

### Changer de Répertoire

```js
const { spawn } = require("child_process");

spawn("find . -type f | wc -l", {
    stdio: "inherit",
    shell: true,
    cwd: "/Users/kar/Documents/programmation/mongo/mongo-001"
});
```

Option `cwd` : current working directory.

### Définir l'environnement

Par défaut, l'environnement est celui du `shell` :

```js
const { spawn } = require("child_process");

spawn("echo $PATH", {
    stdio: "inherit",
    shell: true
});
```

```bash
node other-spawn.js 
/Library/Frameworks/Python.framework/Versions/3.8/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/share/dotnet:~/.dotnet/tools:/Library/Frameworks/Mono.framework/Versions/Current/Commands:/Applications/Xamarin Workbooks.app/Contents/SharedSupport/path-bin
```

On peut isoler l'environnement du `shell` :

```js
const { spawn } = require("child_process");

spawn("echo $HOME", {
    stdio: "inherit",
    shell: true,
    env: {}
});
```

```bash
node other-spawn.js 

```

Ou bien créer de nouvelles variables uniquement pour le script :

```js
const { spawn } = require("child_process");

spawn("echo $PERSO love $HOBBY", {
    stdio: "inherit",
    shell: true,
    env: {
        PERSO: "titi",
        HOBBY: "tennis"
    }
});
```

```bash
node other-spawn.js 
titi love tennis
```

### `detached`

permet de *détacher* un processus.

`timer.js`

```js
setTimeout(() => {}, 8000);
```

`detach.js`

```js
const { spawn } = require("child_process");

const child = spawn("node",["timer.js"],{
    // detached: true,
    stdio: "ignore"
});

// child.unref();
```

Si on n'utilise pas `detached`, les deux processus restent liés :

#### ! utilisation de `Glances` pour monitorer les processus (`enter` pour introduire un filtre).

![Screenshot 2020-04-06 at 11.01.08](assets/Screenshot 2020-04-06 at 11.01.08.png)

Quand `timer.js` se termine, `detach.js ` se termine aussi.

`detach.js` reste actif jusqu'à la fin de `timer.js`.

#### avec `detached`

```js
const { spawn } = require("child_process");

const child = spawn("node",["timer.js"],{
    detached: true,
    stdio: "ignore"
});

child.unref();
```

`child.unref()` enlève le lien vers `timer.js`, si on ne le met pas on revient dans la situation sans `detached`.

![Screenshot 2020-04-06 at 11.04.47](assets/Screenshot 2020-04-06 at 11.04.47.png)

On voit que `timer.js` tourne tout seul, `detach.js` s'est directement arrêté à la fin de sa pile d'exécution.

