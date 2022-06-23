# Parsing `url`

## `url.parse(<url:string>)`

```js
const url = require("url");

const apiUrl ="https://justin.me/one/1?three=seven";

const urlParsed = url.parse(apiUrl);

console.log(urlParsed);
```

```bash
Url {
  protocol: 'https:',
  slashes: true,
  auth: null,
  host: 'justin.me',
  port: null,
  hostname: 'justin.me',
  hash: null,
  search: '?three=seven',
  query: 'three=seven',
  pathname: '/one/1',
  path: '/one/1?three=seven',
  href: 'https://justin.me/one/1?three=seven'
}
```

Pour le teste :

```js
const apiUrl ="je vais à la montagne";
const urlParsed = url.parse(apiUrl);

console.log(urlParsed);
```

```bash
Url {
  protocol: null,
  slashes: null,
  auth: null,
  host: null,
  port: null,
  hostname: null,
  hash: null,
  search: null,
  query: null,
  pathname: 'je%20vais%20à%20la%20montagne',
  path: 'je%20vais%20à%20la%20montagne',
  href: 'je%20vais%20à%20la%20montagne'
}
```

Il n'y a pas d'erreur, mais la pluspart des résultats attendu sont à `null`.

## `url.parse(<url>).path`

```js
const apiUrl ="https://justin.me"; // === https://justin.me/

const urlParsed = url.parse(apiUrl);

console.log(urlParsed.path);
```

```bash
/
```

Même si l'`url` ne spécifie pas le slash de fin `/`, `.path` le renvoie en tant que `root`.

## Différence entre `.path` et `.pathname`

```js
const apiUrl ="https://justin.me/one/1";
const urlParsed = url.parse(apiUrl);

console.log(urlParsed.path);
console.log(urlParsed.pathname);
```

```bash
/one/1
/one/1
```

On pourrait penser que c'est la même chose mais :

```js
const apiUrl ="https://justin.me/one/1?name=titi&age=24";
const urlParsed = url.parse(apiUrl);

console.log(urlParsed.path);
console.log(urlParsed.pathname);
```

```bash
/one/1?name=titi&age=24
/one/1
```

dans le `path` on retrouve la `querystring` et pas dans le `pathname`.

## `querystring`

```js
const url = require("url");
const querystring = require("querystring");

const apiUrl ="https://justin.me/one/1?name=titi&age=24";
const urlParsed = querystring.parse(apiUrl);

console.log(urlParsed);
```

```bash
[Object: null prototype] {
  'https://justin.me/one/1?name': 'titi',
  age: '24'
}
```

Ce n'est pas ce qu'on attendait.

Il faut utiliser `url` et `querystring` ensemble pour obtenir le résultat escompté.

```js
const apiUrl ="https://justin.me/one/1?name=titi&age=24";
const urlParsed = url.parse(apiUrl);
const queryParsed = querystring.parse(urlParsed.query);

console.log(queryParsed);
```

### `querystring.parse(urlParsed.query)`

```bash
[Object: null prototype] { name: 'titi', age: '24' }
```

### Un exemple plus complexe

```js
const apiUrl ="https://justin.me/one/1?name=titi&color=924&color=567";
const urlParsed = url.parse(apiUrl);

const { query } = urlParsed;  // assignation par décomposition
const queryParsed = querystring.parse(query);

console.log(queryParsed);
```

```bash
[Object: null prototype] { name: 'titi', color: [ '924', '567' ] }
```

On obtient automatiquement un tableau si un nom de propriété est reçu avec plusieurs valeurs.

### ! les nombres sont convertis en chaîne de caractère

