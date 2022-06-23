# 01 Les fondations

## API Application Programming Interface

## REST Representational  State Transfert

Utilisation de `HTTP` et des `URL` pour invoquer des fonctionnalités.

### Les six conditions

1. Architecture **Client - Server**
2. **Uniform Interface**
	- C'est une manière standardisée de transmettre les informations.
	- Les ressources sont identifiées par une `URL`.
	- Utilisation des méthodes `HTTP` : manipulation des ressources en utilisant les représentations.
	- Self-Descriptive messages: indiquer le format des ressources `Accept` et `Content-Type` sont utilisés pour ça.
	- `HATEOAS` Hypermedia As The Engine Of Application State : la réponse contient des liens pour manipuler cette ressource.
3. Stateless Protocols : chaque requête possède assez d'information pour qu'elle soit résolu par le serveur de manière satisfaisante.
4. **Cache** la ressource est renvoyé par le cache au lieu du serveur.
5. **Layered System** les évolutions côté serveur n'impactent pas le client.
6. **Code On-Demand** (optional) le service peut envoyer du code exécutable par le client (javascript). 

## `HTTP` Anatomy

### `http request`

Request line

header

body [optional]

### Request line

```
HTTP-METHOD URI HTTP-PROTOCOL
```

```
GET /api/authors HTTP/1.1
```

### Header

Contient les *headers* de la requête.

Les *headers* (en-têtes) sont les méta-données de la requête.

Ce sont des couples clé-valeur (key-value).

```
Host: en.wikipedia.org
Cache-Control: no-cache
```

### Body

```
POST /api/authors HTTP/1.1
Host: myWebAPI.com
Content-Type: application/json
Cache-Control: no-cache

{
	"name": "titi",
	"age": 567
}
```

Il y a une ligne blanche entre la partie `header` et le `body`.

### `HTTP Response`

- Status line
- Header
- Body [optional]

```
HTTP/1.1 200 OK
Date: Thu, 03 jan 2020 22:45:09 GMT
Server: gws
Accept-Range: bytes
Content-Length: 68894
Content-Type: text/html;charset=UTF-8

<!doctype html><html> ...
```

## `HTTP` methods

`GET` requête de données.

`HEAD` même chose que `GET` mais seulement pour récupérer le `Header`.

`POST` envoyer des informations au serveur à travers le `Body` de la requête.

`PUT` met à jour une donnée ou la crée si elle n'existe pas.

`DELETE` supprimer une ressource.

`PATCH` mettre à jour seulement une partie d'une ressource.



## `HTTP` Status

Il y a cinq catégories de status :

| Code | Catégorie      |
| ---- | -------------- |
| 1xx  | Informatif     |
| 2xx  | Succès         |
| 3xx  | Redirection    |
| 4xx  | Erreur Client  |
| 5xx  | Erreur serveur |

## Exemple

`100` Continue

`200` OK

`201` Created

`204` No Content

`400` Bad Request

`401` Unauthorized (il faut se logger)

`403` Forbidden

`404` Not Found

`405` Method Not Allowed (par exemple `DELETE` sur une ressource)

`500` Internal Server Error (retournée par exemple si notre backend n'arrive pas a se connecter à la BDD)

