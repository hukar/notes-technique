# 19 Cloud Storage

<img src="assets/components-cloud-storage.png" alt="components-cloud-storage" style="zoom:50%;" />

- les données sont préservées d'une altération physique
- l'accès est configurable et sécurisé
- On peut rendre les données accessible largement.
- On peut configurer son infrastructure
- Les données sont accessible dans le cloud en lecture et en écriture



## Blobs

### `B`inary `l`arge `ob`ject`s`

<img src="assets/azure-core-storage-service.png" alt="azure-core-storage-service" style="zoom:50%;" />

On peut avoir des blobs jusqu'à `4,7TB`.

<img src="assets/azure-blobs-storage-description.png" alt="azure-blobs-storage-description" style="zoom:50%;" />

### Anatomy

## Créer un `storage account`

performance :

`Standard` : Disque Dur HDD Hard Disk Drive (`drive` : lecteur)

`Prenium` : SSD Solid State Drive

Choisir `StorageV2` qui a plus d'options (à vérifier ?).

access tier : niveau d'accès

`cold` stockage à froid -> écriture, très peu de lecture

`hot`  -> lecture écriture

Il existe aussi dans les `Blobs storage` un autre choix : `archive`.

### `Replication` 

Les données sont au moins répliquées sur trois endroit physique.

`Locally Redundant Storage` option la moins chère, trois différente place dans le même endroit physique (le même data center).

`Zone Redundant Storage` données sauvegardées dans trois Zone distante de la même région.

`Geo-redundant Storage` Les données sont stockées dans trois endroits physiques dans une seule **zone** et une autre **zone** dans une autre **région**.

`Geo-zone-redundant Storage` combine **Zone** et **Region**

<img src="assets/azure-storage-data-redundancy.png" alt="azure-storage-data-redundancy" style="zoom:50%;" />

Il y a aussi des options en lecture :

`Read Access for Geo-redundant Storage` Les données sont stockées dans une unique **Zone**, on peut lire les données dans les différentes **régions**, si une **région** n'était plus disponible, on pourrait directement lire les données dans l'autre **région**.

`Read Access for Geo-zone Redundant Storage` Plusieurs **Zone** et une autre **région**.

<img src="assets/data-redundancy-read-options.png" alt="data-redundancy-read-options" style="zoom:50%;" />

## Azure calculator

<img src="assets/pricing-calculator.png" alt="pricing-calculator" style="zoom:50%;" />



De base : 

### `$20.80`

<img src="assets/storage-account-pricing-1.png" alt="storage-account-pricing-1" style="zoom:50%;" />

Super `Geo-zone Redundant Storage` pour `1000TB` :

### `$45 143.65`

<img src="assets/super-storage-hot-price-1000-tb.png" alt="super-storage-hot-price-1000-tb" style="zoom:50%;" />

#### ! si on prend des options `cool storage` ou `archive storage` le coût de conservation est faible mais le coût des opérations est très élevé.



## Créer l'`account storage`

<img src="assets/ create-storage-account.png" alt=" create-storage-account" style="zoom:50%;" />



## Azure storage container

`public access level` règle les accès anonyme :

`Private` pas d'accès anonyme.

`Blob` un accès anonyme au fichier.

`Container` un accès anonyme à la totalité des fichiers.



## Microsoft Azure Storage Explorer

Application desktop pour visionner ses données.

<img src="assets/microsoft-azure-storage-explorer.png" alt="microsoft-azure-storage-explorer" style="zoom:50%;" />

