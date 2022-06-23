# Les cookies

## Exemple de cookies

![cookie](assets/cookie.png)



UN cookie a un poids max de `4kb`

Les cookies font partie du protocole `HTTP`

Ce protocole est `Stateless` il ne conserve pas de données.

Les `cookies` servent à créer (maintenir) un `state` entre le client et e serveur.

- 4kb
- Clé/valeur paire
- Ce sont seulement des chaines de caractères
- Envoyés au serveur avec chaque requêtes `HTTP`

#### ! Les `cookies` sont envoyés avec chaque requête `HTTP`

Ils servent à maintenir un état entre le serveur et le client

## Le cookie

Il a 5 propriétés

- key=value; (obligatoire)
- expires=date; (optionel: si il n'est pas définie le cookie expire quand le browser est fermé)
- path=path; (optionel: défini un dossier ou un chemin)
- domain=domain; (optionel: définie les domaines autorisé a utiliser ce cookie, sinon le document courant est considéré comme un domaine)
- Secure; (flag optional, oblige le browser a utiliser uniquement https pour ce cookie)

```
cookie_name=cookie_value;expires=Sun, 16 Jul 3567 06:23:41 GMT;path=/;secure;
```

Si un nouveau `cookie` avec le même nom est envoyé, il écrase l'ancien.

### Création d'un `cookie`

```js
const keyValue = "consentCookie=true";

const now = new Date();
now.setTime(now.getTime() + 3600 * 1000 * 24);
newDate = now.toUTCString();

const cookie = `${keyValue};expires=${newDate}`;

document.cookie = cookie;
```

Une fonction pour créer un cookie d'une journée :

```js
function setCookie(key, value) {
    const now = new Date();

    // in millisecond
    now.setTime(now.getTime() + 3600 * 1000 * 24);

    document.cookie = `${key}=${value};expires=${now.toUTCString()}`;
}
```



### Retrouver un `cookie`

Fonction maison :

```js
const cookies =
    "login=666; consentCookie=true; cookie_0=value_0; cookie_1=value_1";

const list = cookies.split("; ").map(cookie => cookie.split("="));
```

étapes :

```js
let l = cookies.split("; "); 
//? [ 'login=666','consentCookie=true','cookie_0=value_0','cookie_1=value_1' ]

l.map(cookie => cookie.split("="));
//? [ [ 'login', '666' ],[ 'consentCookie', 'true' ],[ 'cookie_0', 'value_0' ],[ 'cookie_1', 'value_1' ] ]
```

#### La fonction en elle-même :

```js
function retrieveCookie(cookies, cookieName) {
    const listCookies = cookies.split("; ").map(cookie => cookie.split("="));

    const output = listCookies.filter(x => x[0] == cookieName); //?
    return output.length === 0 ? undefined : output[0][1]; //?
}


retrieveCookie(cookies, "logn"); //? undefined
retrieveCookie(cookies, "login"); //? '666'
```

Étapes :

```js
const l = list.filter(cookie => cookie[0] == "login");
//? [ [ 'login', '666' ] ]

const l = list.filter(cookie => cookie[0] == "logn");
//? []

l.length === 0 ? undefined : l[0][1]; //? undefined
```

#### Simplification pour le navigateur

Les `cookies` sont toujours donnés par `document.cookie` on peut donc simplifier la fonction :

```js
function retrieveCookie(cookieKey) {
    const listCookies = document.cookie.split("; ").map(cookie => cookie.split("="));

    const output = listCookies.filter(x => x[0] == cookieKey);
  	return output.length === 0 ? undefined : output[0][1]
}
```

#### Utilisation

```js
const cookie_value =  retrieveCookie(cookie_name);
// string retrieveCookie(string)
```

#### 