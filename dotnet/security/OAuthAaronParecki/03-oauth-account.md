# 03 Créer un compte `OAuth`

Exercice de https://oauth.school/exercise/introduction/



## créer un compte `auth0`

<img src="assets/create-account-auth0.png" alt="create-account-auth0" style="zoom:50%;" />

Je me suis inscris avec mon compte `Google`.

<img src="assets/create-oauth-2.png" alt="create-oauth-2" style="zoom:50%;" />



## Créer une `API`

<img src="assets/create-one-api.png" alt="create-one-api" style="zoom:50%;" />

<img src="assets/create-my-api-huker-api.png" alt="create-my-api-huker-api" style="zoom:50%;" />



## Définir mon `Default Audience`

C'est mon `endpoint`.

<img src="assets/default-setting-path-to-set.png" alt="default-setting-path-to-set" style="zoom:50%;" />

En descendant plus bas :

<img src="assets/set-default-audience.png" alt="set-default-audience" style="zoom:50%;" />

Je copie-colle l'`URL` de mon `API`. Puis `Save` bien sûr.



## `Oauth server Issuer URI`

Il faut créer une application (n'importe laquelle).

<img src="assets/create-application-for-issuer-uri.png" alt="create-application-for-issuer-uri" style="zoom:50%;" />

Puis aller dans `Settings` puis `advanced settings` :

<img src="assets/openid-configuratuion-url-showing.png" alt="openid-configuratuion-url-showing" style="zoom:50%;" />

On copie et introduis l'`url` de `OpenId Configuration` dans le navigateur:

<img src="assets/issuer-display-in-browser.png" alt="issuer-display-in-browser" style="zoom:50%;" />

La première ligne est le `issuer` (l'`emetteur`):

```
"issuer": "https://hukar.eu.auth0.com/"
```



























