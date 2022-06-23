#1 - npm init
générer un package.json avec la commande :

```sh
npm init
```

#2 - installer lite-server
Dans le repértoire du projet taper :


```sh
npm install lite-server --save-dev
```

Créer un fichier `index.html` qui sera lu par le serveur et un fichier `app.js` :



```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Typescript</title>
    <script src="app.js"></script> <!-- ici le script -->
</head>
<body>
    
</body>
</html>
```





#3 - Configurer package.json

Ajouter la ligne suivante :

```json
{
  "name": "decorator-2",
  "version": "1.0.0",
  "description": "",
  "main": "app.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1",
    "start": "lite-server" // <- ici
  },
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "lite-server": "^2.3.0"  // <- ici on voit la dépendance
  }
}
```

#4 - lancer le serveur

```sh
npm start
```
Sur le navigateur `localhost:3000`
#5 - Générer tsconfig.json

commande :

```sh
tsc --init
```

```json
{
    "compilerOptions": {
        "module": "commonjs",
        "target": "es2016",
        "jsx": "preserve",
        "experimentalDecorators": true // <- obligatoire pour utiliser
        								// les decorator
    },
    "exclude": [
        "node_modules",
        "**/node_modules/*"
    ]
}
```
#6 - Compiler à la volée

```sh
tsc -w *.ts
```
----

----



## TSLint

Pour utiliser tslint, il faut taper :

```sh
tslint --init
```

`tslint.json` sera créer à la racine du projet

## auto-compilation

Pour compiler automatiquement :

```sh
tsc -w hello
tsc -w *.ts
```

`hello` est un fichier `hello.ts` 

`*.ts` représente n'importe quel fichier `.ts`

## exécuter du code dans vscode

extension Code Runner `ctrl` + `alt` + `n`

# Configuration de tsconfig.json

empécher la transcompilation si erreur :

```json
"noEmitOnError": true,
```

Source map pour le debbugage :

```json
"sourceMap": true,
```

> **!Attention : en mode watch le transcompileur génère le js même si il y a des erreurs**

Paramètre déclaré et non utilisé :

```json
"noUnusedParameters": true,    
```

### 

# Compiler typescript en CLI

Il nous faut `ts-node` qui compile TypeScript dans le repl de node.

Il faut aussi `nodemon` qui surveille le dossier courant et execute un script au changement.

Le script parfait :

```sh
nodemon --exec ts-node app.ts
```

On peut bien sure exécuter ts-node seul pour une seul fois :

```sh
ts-node app.ts
```

Les exports et imports sont gérer.



## ! tsconfig n'est pris en compte que si on ne designe pas une cible à tsc

```sh
tsc
```

ne fonctionne pas si pas de `tsconfig.json`

Par contre avec une tsconfig (`tsc --init`) :

```sh
tsc *.ts
```

n'utilise pas `tsconfig.json`

utiliser à la place :

```sh
tsc
```

