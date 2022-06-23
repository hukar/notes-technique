# 02 REST, HTTP et codes de status

## RESTful

### REST = REpresentational State Transfer

4 critères :

1. Les ressources doivent être accessible par une `URL Endpoints`
2. Utilisation de JSON ou XML pour les données
3. Stateless (sans états) Une requête ne peut dépendre d'une autre requête
4. Utilisation des verbes HTTP GET, POST, DELETE, PUT ...

## Requête HTTP

```
GET http://www.google.com HTTP/1.1
```

### Les verbes d'action

* GET : retrouver une ressource
* POST : créer une nouvelle ressource
* PUT / PATCH : mettre à jour une ressource
* DELETE : effacer une ressource

### Le message de requête

* Une ligne de requête
* L'en-tête de la requête avec des champs d'informations additionnelles
* Une ligne vide
* Le contenue du message [optionnel]

### La réponse

* Une ligne de status
* L'en-tête (informations additionnelles)
* Une ligne vide
* Le contenu du message [optionnel]

```
HTTP/1.1 200 OK
```

## Les codes de status

| Groupe de Code de status | Examples                                                     |
| ------------------------ | ------------------------------------------------------------ |
| 1xxx - Informatif        |                                                              |
| 2xx - Succès             | 200 OK<br />201 Created                                      |
| 3xx - Redirection        |                                                              |
| 4xx - Erreur Client      | 400 Bad Request<br />401 Unauthorized<br />403  Forbidden<br />404 Not Found<br />405 Method Not Allowed |
| 5xx - Erreur Serveur     |                                                              |

