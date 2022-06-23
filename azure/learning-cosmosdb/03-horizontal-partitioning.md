# 03 Horizontal Partitioning

## Mise à l'échelle élastique

<img src="assets/Screenshot2020-07-28at08.51.28.png" alt="Screenshot 2020-07-28 at 08.51.28" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-28at08.54.04.png" alt="Screenshot 2020-07-28 at 08.54.04" style="zoom:33%;" />

Quand une partition est remplie dans un `container`, **Cosmos DB** en crée une nouvelle automatiquement.

Un `container` logique est reparti sur plusieurs `partition`  physiques. Il est aussi dupliqué sur au moins 4 `replicas` pour garantir une haute disponibilité des données.

Comment ces données sont réparties entre les différentes partitions ? `partition Key`.

<img src="assets/achieving-elastic-scale.png" alt="achieving-elastic-scale" style="zoom:50%;" />

La conséquence est d'avoir un `storage` et un `throughput` illimité :

<img src="assets/unlimited-storage-throughput.png" alt="unlimited-storage-throughput" style="zoom:50%;" />

## `partition` logique

Toutes les données d'une `partition` logique sont sur une même `partition` physique.

Alors que les `partition` physique sont gérées de manière transparente pour l'utilisateur, les `partitions` logique sont créé à l'aide de la `partition key`.

Cette `partition key` formée d'un ou plusieurs champs des documents, permet de ranger les documents ensemble.

Si on utilise le `username` comme `partition key` on pourrait avoir :

<img src="assets/logical-partition-by-name.png" alt="logical-partition-by-name" style="zoom:50%;" />

Ces `partitions` logiques sont elles-même dispatchées sur des `partitions` physiques.

### Un document ne peut excéder `2 MB`

C'est une limite qui nous dit quand on doit créer un deuxième document au lieu d'encapsuler toujours plus de données dans un seul.

### Une `partition` logique ne peut excéder `20 GB`

Il vaut mieux alors que la `partition key` prenne une large étendue de valeurs, pour avoir beaucoup de `partitions` logiques de petite taille.

Ainsi le `container` peut se mettre à l'échelle indéfiniment.

<img src="assets/limit-partition.png" alt="limit-partition" style="zoom:50%;" />



`Cosmos DB` ajoute automatiquement, sans interruption et instantanément une `partition` physique pour répartir les `partions` logique si nécessaire

<img src="assets/partition-split.png" alt="partition-split" style="zoom:50%;" />

## `hot partition`

Si la `partition key` est mal choisie, on peut avoir une partition logique qui se remplie plus vite que les autres. Cela crée une `hot partition`.

En fait le `throughput` global est partagé entre les `partition` physique, ce qui a comme conséquence un étranglement du `throughput` en cas de `hot partition`.

Par exemple si on a un `throughput` de `5000RUs/s` pour le `container`, si on attaque une seule `partition` physique, le `throughput` réel sera alors `700RUs/s`. 

<img src="assets/hot-partitions-schema-1.png" alt="hot-partitions-schema-1" style="zoom:50%;" />

À la place, on veut une `partition key` permettant une répartition uniforme des documents.

La répartition doit-être pensée en terme de stockage mais aussi de débit.

Si on prends la date du jour comme `partition key`, la répartition sera bonne, mais le débit ne sera pas uniforme car il est probable que seul les plus récent documents soit demandés (news) :

<img src="assets/hot-partition-on throughput.png" alt="hot-partition-on throughput" style="zoom:50%;" />

On a de nouveau une `hot partition`, plus sur le `storage` mais sur le `throughput`.

Le choix du nom comme `partition key` permet une bonne répartition en terme de `storage` et de `throughput` dans le cadre d'une application sociale :

<img src="assets/resolution-of-hot-partition.png" alt="resolution-of-hot-partition" style="zoom:50%;" />

## Choisir une `partition key`

<img src="assets/choosing-best.png" alt="choosing-best" style="zoom:50%;" />

#### ! Un choix judicieux permettra une mise à l'échelle très grande, un coût plus bas (`cross query`) et une vitesse plus grande (meilleur distribution du `throughput`).

Les `items` doivent pouvoir se répartir correctement avec une bonne `partition key`.

Une `partition` réunis physiquement tous les `items` de même valeur de `partition key`.

Une partition est une limite pour les procédures stockées.

Il faut éviter les goulot d'étranglement pour le stockage et les performances => repartition uniforme des `items` dans les partitions.

<img src="assets/Screenshot2020-07-28at09.17.35.png" alt="Screenshot 2020-07-28 at 09.17.35" style="zoom:33%;" />

Ici le choix de la ville comme `partition key` n'est pas forcement judicieux, car il y a des grosses villes et des petites villes.

Le code postal (`zipCode`) serait un choix plus judicieux au niveau de la repartition des données.

<img src="assets/Screenshot2020-07-28at09.22.40.png" alt="Screenshot 2020-07-28 at 09.22.40" style="zoom:50%;" />

Une requête sera toujours plus efficace si elle n'impact qu'une partition (une machine physique).

La `partition key` doit être choisie en fonction des requêtes les plus courantes.

Pour un réseau social l'`userID` peut être un choix judicieux pour récupérer tous les `posts` de quelqu'un.

### Choisir la date de création : mauvaise idée

<img src="assets/Screenshot2020-07-28at09.28.14.png" alt="Screenshot 2020-07-28 at 09.28.14" style="zoom:33%;" />

la répartition des données est bonne, mais par contre toutes les écritures se font sur la même partition => `hot partition`.

<img src="assets/Screenshot2020-07-28at09.30.21.png" alt="Screenshot 2020-07-28 at 09.30.21" style="zoom:33%;" />

Si on a `400 RUs` de débit pour un `container`, celui-ci est distribué uniformément entre toutes les partitions.

Si on a dix partitions, le débit (`throughput`) est donc de `40RUs` , avec la **date de création** comme `partition key` on risue de vite dépasser le `throughput` alloué.

<img src="assets/Screenshot2020-07-28at09.34.33.png" alt="Screenshot 2020-07-28 at 09.34.33" style="zoom:33%;" />

Avec `UserId` comme `partition key` les écritures sont uniformément réparties.

<img src="assets/choosing-right.png" alt="choosing-right" style="zoom:50%;" />

## Résolution de problème

<img src="assets/Screenshot2020-07-28at09.43.00.png" alt="Screenshot 2020-07-28 at 09.43.00" style="zoom:33%;" />

Il est possible qu'un habitant (`tenant`) est plus de données que les autres, par exemple une grosse compagnie.

On peut alors dédier un container spécialement à ce `tenant 10` et organiser les partitions avec une autre `partition key`.

<img src="assets/Screenshot2020-07-28at09.46.51.png" alt="Screenshot 2020-07-28 at 09.46.51" style="zoom:33%;" />

ce `container` possède alors son propre débit (`throughput`).

## Cross Partition Query

### `Single partition query`

<img src="assets/single-partition-query.png" alt="single-partition-query" style="zoom:50%;" />

Une `query` exécutée sur une même `partition` logique est peu coûteuse en terme de `RU`.

Voici une requête faite sur la même `partition key` : `location/state`.

```sql
SELECT * FROM c WHERE c.location.state = "NY"
```

Elle me renvoie deux documents et coûte `2.87RUs`

Maintenant une requête `cross-partition` :

```sql
SELECT * FROM c WHERE ARRAY_LENGTH(c.children) = 2
```

Dans ce cas la requête renvoie aussi deux documents mais son coût est de `3RUs`

### `cross-partition query`

Elle sont parfois inévitable, mais un bon ratio serait `80%` de `single partition query` pour `20%` de `cross-partition query`.

<img src="assets/cross-partition-query.png" alt="cross-partition-query" style="zoom:50%;" />

### Utiliser un sous-ensemble de `partition`

Pour éviter le surcoût d'une `cross-partition query`, on peut utiliser l'opérateur `IN` :

#### Pas bien

<img src="assets/not-good-cross-query.png" alt="not-good-cross-query" style="zoom:50%;" />

#### Bien

<img src="assets/in-operator.png" alt="in-operator" style="zoom:50%;" />

#### ! de manière générale, il faut essayer d'utiliser sa `partion key` dans la pluspart des requêtes

## Définir sa `partition key`

Comprendre comment ses données sont demandées permet de choisir la bonne `partition key`.

<img src="assets/right-partition-key.png" alt="right-partition-key" style="zoom:50%;" />

- Il faut la choisir en fonction des usages fréquents.
- elle doit garantir une bonne répartition du `storage` et du `throughput`.
- elle définit les limites pour les requêtes et les `transactions` : procédure stockées.

<img src="assets/Screenshot2020-07-28at09.52.06.png" alt="Screenshot 2020-07-28 at 09.52.06" style="zoom:33%;" />

`fan-out execution` : déployer l'exécution.

Une requête Cross-Partition sera exécuté à travers plusieurs machines physique, elle sera donc plus coûteuse qu'une requête sur une même partition.



## Pattern de partitionnement communs

### Utiliser l'`id`.

Si on doit lire ou écrire un document à la fois, cela peut être un bon pattern.

Ici on a une `partition` / `document`.

Pour de l'`iot` où chaque document représente une machine et son état, et que cet état est mis très régulièrement à jour par les capteurs, cela va bien convenir :

<img src="assets/id-key-partition-pattern.png" alt="id-key-partition-pattern" style="zoom:50%;" />

Bien entendu si on doit modifier ou consulter plusieurs documents en même temps, c'est une mauvaise idée que de choisir l'`Id` comme `partion key`.

### Utiliser le `type`

Un avion et tous ses documents, un aéroport, une catégorie de produit, un département.

On défini une liste de recherche : `lookup list`

<img src="assets/partitionning-by-type.png" alt="partitionning-by-type" style="zoom:50%;" />

### Autre scénario

Pour un e-commerce on peut utiliser le `customerId` comme `partition key`.

<img src="assets/customer-id-partition-key.png" alt="customer-id-partition-key" style="zoom:50%;" />

Pour `eprolex` le `representativeId` serait certainement le bon choix.

Comme `Cosmos DB` est `schema-free`, on peut enregistrer des documents très différents sur la même `partition key`.



## Changer de `partition key`

<img src="assets/Screenshot2020-07-28at10.42.14.png" alt="Screenshot 2020-07-28 at 10.42.14" style="zoom:50%;" />

La `partition key ` ne peut pas être changée.

Il est donc préférable de créer une propriété `pk` avec une copie de la valeur de la `partition key` réel choisie.

Deux documents peuvent avoir le même `id` à partir du moment où il sont dans deux `partitions` logiques différentes.

C'est la combinaison de l'`id` et de la `partition key` qui doit être unique.

Cependant Il vaut mieux toujours utiliser un `guid` comme propriété `id` pour éviter les collisions en cas de migration sur place.
