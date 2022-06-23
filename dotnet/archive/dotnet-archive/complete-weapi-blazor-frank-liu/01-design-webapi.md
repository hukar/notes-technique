# 01 Designig a Web `API`

`Web API` Ce sont des fonctions à distance qui communiquent via `HTTP`.

## `RESTFull` API

Pour parler de `RESTFull API` il faut respecter 6 contraintes.

1. Client-Serveur
2. Stateless (sans état)
3. Avec mise en cache
4. En couches
5. Interface Uniforme
6. [Facultative] Code à la demande

Voire ce lien : https://restfulapi.net/



## Points importants

- Gérer les versions
- Authorisation
- Model binding / Lier les données importantes (paramètres de l'`url`)
- Model Validation : ne pas faire confiance aux données entrantes
- Gére les exceptions
- Formater le résultat

Le développeur attend de son framework qu'il soit capable de gérer ces choses pour lui.

Il doit garder son focus sur la logique business et sur la manipulation de données.



## `API` Design

On a déjà les verbes `HTTP` on peut donc utiliser uniquement les noms des entités (au pluriel)


### E-Commerce

#### Products

`Create (Post)`

```
/api/producst
```

`Read (Get)`

```
/api/products
```

`Read One (Get)`

```
/api/products/{id}
```

`Update (Put)`

```
/api/products
```

`Delete (Delete)`

```
/api/products/{id}
```

Pour les catégories on aura la même chose.

Maintenant on voudrait avoir tous les produits pour une catégorie donnée :

```
/api/categories/{id}/products
```

Et maintenant on veut un produit particulier dans une catégorie :

```
/api/categories/{id}/products/{id}
```

#### C'est un template de routage : `routing template`.

## Exercice

```
// Create
/api/shirts

// read all
/api/shirts

// read one
/api/shirts/{id}

// read shirts (many) with particular size
/api/brands/{name}/sizes/{size}

// read one with particular color and particular size
/api/brands/{name}/colors/{color}/sizes/{size}

// update one
/api/shirts

// delete one
/api/shirts/{id}
```

