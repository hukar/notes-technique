
# Flask HTTP 02

**GET** Retrouver quelque chose GET /item/1  
**POST** Recevoir des données POST /item
**PUT** Créer un objet s'il n'existe pas ou bien le met à jour PUT /item  
**DELETE** Effacer des données DELETE /item/1

# REST principes 
## Representational State Transfer

C'est une façon de penser la communication avec le serveur web.  
On parle de ressources et plus de données.
Le serveur renvoie des ressources

#### Item Ressource

`GET /item/chair`  
`POST /item/chair` avec extra-données  
`PUT /item/chair` avec extra-données  
`DELETE /item/chair`  

Toutes ces requête ont lemême **endpoint** /item/chair

Toutes ces requêtes intéragissent avec la même **ressource**

#### ItemList Ressource

GET /items

## Stateless

Une requête ne dépend d'aucune autre requête.  
Le serveur ne connait que la présente requête, aucune autre avant.
