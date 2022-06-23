# 00 Cosmos DB concept et utilisation

## Imbriqué ou référencé ?

<img src="assets/Screenshot2020-07-19at08.47.17.png" alt="Screenshot 2020-07-19 at 08.47.17" style="zoom:50%;" />

Le modèle NoSQL va tendre vers une représentation imbriquée des données, l'écriture JSON étant adapté à cela.

## Modèle de départ

<img src="assets/Screenshot2020-07-19at09.40.08.png" alt="Screenshot 2020-07-19 at 09.40.08" style="zoom: 25%;" />

On va essayer de représenter ce modèle avec la logique de `Cosmos DB`.

## Partition Key `PK`

<img src="assets/Screenshot2020-07-19at08.53.48.png" alt="Screenshot 2020-07-19 at 08.53.48" style="zoom:50%;" />

Cosmos DB est une abstraction au-dessus de d'un cluster de plusieurs serveurs physiques.

`cluster` = grappe de serveur

<img src="assets/Screenshot2020-07-19at08.58.51.png" alt="Screenshot 2020-07-19 at 08.58.51" style="zoom:50%;" />

Les documents sont dispatchés sur plusieurs serveurs.

<img src="assets/Screenshot2020-07-19at09.00.14.png" alt="Screenshot 2020-07-19 at 09.00.14" style="zoom: 33%;" />

### Comment choisir quel serveur ?

Les documents sont en fait copiés sur des partitions logiques.

<img src="assets/Screenshot2020-07-19at09.02.09.png" alt="Screenshot 2020-07-19 at 09.02.09" style="zoom:50%;" />

C'est la `Partition Key` qui permet de créer différentes partitions.

Ici c'est le prénom qui est utilisé pour créer des partitions.

<img src="assets/Screenshot2020-07-19at09.03.48.png" alt="Screenshot 2020-07-19 at 09.03.48" style="zoom:25%;" />

Deux contraintes:

- un document ne peut exéder `2 MB`
- une partition ne peut exéder `10 GB`

## Choix de la `Partition Key` pour `User`

On veut que les données et les requêtes soit bien réparties, on ne veut pas ça :

<img src="assets/Screenshot2020-07-19at09.05.50.png" alt="Screenshot 2020-07-19 at 09.05.50" style="zoom:50%;" />

ou ça :

<img src="assets/Screenshot2020-07-19at09.06.12.png" alt="Screenshot 2020-07-19 at 09.06.12" style="zoom:50%;" />

#### Le choix de la `Partition Key` est donc stratégique et important.

<img src="assets/Screenshot2020-07-19at09.09.07.png" alt="Screenshot 2020-07-19 at 09.09.07" style="zoom:50%;" />

Si une requête n'impact qu'une `Partition Key` (condition sur la `PK`), elle sera rapide, car toutes les informations sont au même endroit.

Une tel requête ne sera routée que vers un seul serveur.

<img src="assets/Screenshot2020-07-19at09.11.48.png" alt="Screenshot 2020-07-19 at 09.11.48" style="zoom:50%;" />

Par contre si la condition ne correspond pas à une `PK`, la requête sera routée à tous les serveurs.

Chaque requête à un prix en `Request Units`, dans ce cas notre requête sera plus lente mais aussi potentiellement plus **chère** car nécessitant (impliquant) plusieurs serveurs.

### Ce n'est pas un problème de choisir une `PK` avec une grande cardinalité

Par exemple l'`userid` peut être un bon candidat comme `PK` pour un `user`.

Plusieurs partitions logiques (des millions) ne correspondent pas à autant de serveurs.

## à tester `Notebook`

<img src="assets/Screenshot2020-07-19at09.22.00.png" alt="Screenshot 2020-07-19 at 09.22.00" style="zoom:50%;" />

## Avoir toutes ses données dans une même partition logique

<img src="assets/Screenshot2020-07-19at09.29.28.png" alt="Screenshot 2020-07-19 at 09.29.28" style="zoom:50%;" />

## `PK` pour des catégories ou des tags

Par exemple si on enregistre des catégorie, on veut que celle-ci soit retournée ensemble, donc qu'elles soient dans la même partition logique.

Pour cela on va créer un attribut `type` :

<img src="assets/Screenshot2020-07-19at09.31.28.png" alt="Screenshot 2020-07-19 at 09.31.28" style="zoom:50%;" />

Le prix de ce genre de requête est de `3,6 RU`, le prix moyen attendu est de `3 ru`.

## Traitement des `product`

### Many To Many

<img src="assets/Screenshot2020-07-19at11.08.41.png" alt="Screenshot 2020-07-19 at 11.08.41" style="zoom:25%;" />

Il y a moins de teg par produit que l'inverse, c'est pour ça que c'est dans `product` que l'on va stocker les `ProductTags`.

<img src="assets/Screenshot2020-07-19at11.13.51.png" alt="Screenshot 2020-07-19 at 11.13.51" style="zoom:50%;" />

On crée un nouveau conteneur (container) pour `product`.

La `Partition Key` la plus utile est en fait la catégorie pour pouvoir retrouver facilement tous les produits par catégorie.

`PK = categoryId`.

## Le nom de la catégorie et des tags associés

<img src="assets/Screenshot2020-07-19at11.16.28.png" alt="Screenshot 2020-07-19 at 11.16.28" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-19at11.17.41.png" alt="Screenshot 2020-07-19 at 11.17.41" style="zoom:50%;" />

On peut faire une requête pour chaque chose a récupérer :

- les produits
- le nom de la catégorie
- le nom des tags

<img src="assets/Screenshot2020-07-19at11.20.59.png" alt="Screenshot 2020-07-19 at 11.20.59" style="zoom:50%;" />

Dans `Cosmos DB` on ne peut pas faire de `join` entre différents conteneur.

### Dé-normalistaion

<img src="assets/Screenshot2020-07-19at11.21.46.png" alt="Screenshot 2020-07-19 at 11.21.46" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-19at11.23.04.png" alt="Screenshot 2020-07-19 at 11.23.04" style="zoom:50%;" />

On ajoute le nom de la catégorie et des tags directement dans notre `Product` enregistrement.

Cela va fonctionner tant que aucune catégorie et aucun tags ne soient renommés.

### Synchroniser les données

<img src="assets/Screenshot2020-07-19at11.25.59.png" alt="Screenshot 2020-07-19 at 11.25.59" style="zoom: 25%;" />

Il faut qu'à chaque fois qu'une catégorie ou qu'un tag est renommé cela met à jour `Product`.

![Screenshot 2020-07-19 at 11.28.25](assets/Screenshot 2020-07-19 at 11.28.25.png)

`Cosmos DB` propose la solution avec `the change feed API`.

Dès qu'un élément est modifié, un message est envoyé pour signaler cette modification.

On peut s'inscrire à ces modification :

<img src="assets/Screenshot2020-07-19at11.30.21.png" alt="Screenshot 2020-07-19 at 11.30.21" style="zoom:25%;" />

Les `Function Azure` permettent de garder la `DB` correcte et consistante.

## Sales Order : La facture

<img src="assets/Screenshot2020-07-19at15.49.36.png" alt="Screenshot 2020-07-19 at 15.49.36" style="zoom:50%;" />

On a une relation `one -> fiew` mais aussi quand on va chercher une facture on veut en général les produits associés.

### Choix de la `Partition Key`

<img src="assets/Screenshot2020-07-19at15.52.35.png" alt="Screenshot 2020-07-19 at 15.52.35" style="zoom:50%;" />

La requête la plus courante est de retrouver toutes les factures d'un seul client, `customerId` fournit donc une `PK` très correcte.

#### ! se poser la question des requêtes les plus courantes pour choisir sa `Partition Key`.

## Optimisation

<img src="assets/Screenshot2020-07-19at15.56.17.png" alt="Screenshot 2020-07-19 at 15.56.17" style="zoom:50%;" />

On remarque que `customers` et `salesOrders` ont la même `Partition Key`, on peut donc les regrouper dans le même conteneur.

`Cosmos DB` étant **schema agnostic** cela ne pose pas de problème.

<img src="assets/Screenshot2020-07-19at16.00.17.png" alt="Screenshot 2020-07-19 at 16.00.17" style="zoom:50%;" />

Pour cela on va créer un attribut `id` séparé de `customerId` et un attribut `type`.

On peut facilement allez chercher tout ce qui appartient à un `customer`.

<img src="assets/Screenshot2020-07-19at17.04.53.png" alt="Screenshot 2020-07-19 at 17.04.53" style="zoom:50%;" />

### JOIN avec le nom

Plutôt que de faire un `JOIN` ici on va juste profiter du fait que le customer est dans le même conteneur :

<img src="assets/Screenshot2020-07-19at17.07.26.png" alt="Screenshot 2020-07-19 at 17.07.26" style="zoom: 33%;" />

Avec le `ORDER BY c.type` on sait que le premier enregistrement sera notre `customer`.

### Classer les clients par le nombre de commandes

<img src="assets/Screenshot2020-07-19at17.10.31.png" alt="Screenshot 2020-07-19 at 17.10.31" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-19at17.14.43.png" alt="Screenshot 2020-07-19 at 17.14.43" style="zoom:33%;" />

On crée un attribut qui retiendra cette information.

### Utiliser les procédures stockées

Elles sont écrites en javascript et leur scope exécution est une partition logique.

<img src="assets/Screenshot2020-07-19at17.20.18.png" alt="Screenshot 2020-07-19 at 17.20.18" style="zoom:50%;" />

Voici donc la requête finale :

<img src="assets/Screenshot2020-07-19at17.22.37.png" alt="Screenshot 2020-07-19 at 17.22.37" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-19at17.26.13.png" alt="Screenshot 2020-07-19 at 17.26.13" style="zoom:50%;" />

Cette requête vaut un peu plus de `5 ru` (`Request Unit`) car elle impact plusieurs partition, cela est normal, on ne peut pas avoir que des requêtes impactant à chaque fois seulement une partition.

Cette requête est plutôt destinée au **backoffice** et ce n'est pas un problème qu'elle coûte un peu plus et soit un peu plus lente.

L'important c'est que les requête les plus courantes soit optimisées (c'est à dire ne sollicitant qu'une partition).

## Design final

<img src="assets/Screenshot2020-07-19at17.31.06.png" alt="Screenshot 2020-07-19 at 17.31.06" style="zoom:50%;" />

### Dernière optimisation

On peut mettre les `tags` et les `catégories` dans le même conteneur :

<img src="assets/Screenshot2020-07-19at17.35.30.png" alt="Screenshot 2020-07-19 at 17.35.30" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-19at17.38.29.png" alt="Screenshot 2020-07-19 at 17.38.29" style="zoom:50%;" />

Bien réfléchir à son cadre d'utilisation, à ses requêtes type.

Choisir ses `Partition Key` en conséquence.

Matérialiser ses relations avec :

- l'imbrication
- la dé-normalisation et la pré-agrégation (en ajoutant le nom des tags par exemple)
- Stocker des entités en relation dans le même conteneur

Les différentes méthodes pour implémenter la dé-normalisation sont :

- Utiliser les `change feed`
- Utiliser les procédures stockées : `stored procedures`

### Lien

https://docs.microsoft.com/en-us/azure/cosmos-db/how-to-model-partition-example

https://azure.microsoft.com/en-gb/services/cosmos-db/

https://gotcosmos.com/learn
