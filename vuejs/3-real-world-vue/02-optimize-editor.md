# 02 Optimiser son éditeur

## Extension VSCode

### `Vetur` de Pine Yu

- syntax highlighting

- snippets `ctrl + space`
  - vue -> squelette de composant

### `eslint` et `prettier`

Pour éviter les conflits il y a une dépendance importante dans `package.json` :

```json
"@vue/eslint-config-prettier": "^6.0.0",
```

Dans `.eslintrc.js` :

```js
extends: ["plugin:vue/essential","plugin:prettier/recommended", "@vue/prettier"],
```

Un autre (quelle différence ?? => le javascript est _linté_ prendre plutôt celui plus bas)

```js
extends: [
  "plugin:vue/recommended",  // vue/recommended sont des règles plus strict que /essential
  "@vue/prettier", // ADD
  "eslint:recommended" // ici le javacsript est linté
],
```

Si besoin pour éviter les conflits :

```js
"prettier/vue"; // de eslint-config-prettier mais déjà injecté grace à @vue/prettier

```

Créer un fichier de configuration pour `prettier`

`.prettierrc.js`

```js
module.exports = {
  trailingComma: "es5", // mettre une virgule à la fin des attributs d'un objets
  tabWidth: 4,
  semi: false,
  singleQuote: true,
  htmlWhitespaceSensitivity: "ignore", // pour éviter des chose bizarre dans le html
};
```

### Problème d'affichage des `tags` avec `Prettier`

On a parfois un `html` qui ressemble à ça :

```html
<a href="https://vuejs.org" target="_blank" rel="noopener"
   >Core Docs</a
  >
```

alors que l'on voudrait ça :

```html
<a href="https://vuejs.org" target="_blank" rel="noopener">
  Core Docs
</a>
```

Il faut juste régler `HTML Whitespace Sensitivity` à `ignore` :

<img src="assets/prettier-whitespace-sensitivity.png" alt="prettier-whitespace-sensitivity" style="zoom:50%;" />





## User Settings

Pour éviter les conflits avec `eslint` on met `vetur validation template` à `false`.

```json
"vetur.validation.template": false
```

### Auto Fix On Save

```json
"editor.codeActionsOnSave": {
        // "source.fixAll.eslint": true
    	"source.fixAll": true  // fonctionne pour moi
    }
```

Décocher `Editor detect indentation` si nécessaire :

<img src="assets/Screenshot2020-11-05at14.26.43.png" alt="Screenshot 2020-11-05 at 14.26.43" style="zoom:33%;" />

## Additional Snippets

Sarah Drasner

<img src="assets/Screenshot2020-09-24at16.21.01.png" alt="Screenshot 2020-09-24 at 16.21.01" style="zoom:50%;" />

`vdata`

```js
data() {
    return {
        key: value
    }
},
```

`von` : un `event handler`

```html
<div @click="handler(arg, event)"></div>
```

`vimport-lib`

```js
import { libName } from 'libName';
```



## Bonus

`live share` pour coder à distance ensemble.

<img src="assets/live-share.png" alt="live-share" style="zoom:50%;" />



## Bien avoir `LF` comme fin de ligne

<img src="assets/LF.png" alt="LF" style="zoom:50%;" />



## Par `Sarah Drasner` : `Vue VS Code Extension Pack`

<img src="assets/extensions.png" alt="extensions" style="zoom:50%;" />



## Exemple de `settings.json` pour `VSCode`

<img src="assets/settings.png" alt="settings" style="zoom:50%;" />

`Window.zoomLevel` niveau de zoom de la fenêtre, intéressant de le faire par projet.

`"editor.defaultFormater":"esbenp.prettier-vscode` permet la précédence de `Prettier` comme formateur.

### Ne pas *commiter* les `settings` dans `git` !

