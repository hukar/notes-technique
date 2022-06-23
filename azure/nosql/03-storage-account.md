# 03 Azure Storage Account

<img src="assets/Screenshot2020-07-20at09.53.52.png" alt="Screenshot 2020-07-20 at 09.53.52" style="zoom:50%;" />

Quatre type `table` `blob` `file` `queue`.

#### `BLOB` : `B`inary `L`arge `OB`ject = Données binaires (images, video)

<img src="assets/Screenshot2020-07-20at10.03.17.png" alt="Screenshot 2020-07-20 at 10.03.17" style="zoom:50%;" />

On va voire le `general-purpose v2`.

<img src="assets/Screenshot2020-07-20at10.06.33.png" alt="Screenshot 2020-07-20 at 10.06.33" style="zoom:50%;" />

Utile pour stocker des données structurées simple n'ayant pas besoin de jointures ou de procédures stockées.

`Table Storage` est de type `NoSQL`.

<img src="assets/Screenshot2020-07-20at10.08.12.png" alt="Screenshot 2020-07-20 at 10.08.12" style="zoom:50%;" />

## Table Storage Concept

<img src="assets/Screenshot2020-07-20at10.11.27.png" alt="Screenshot 2020-07-20 at 10.11.27" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-20at10.14.07.png" alt="Screenshot 2020-07-20 at 10.14.07" style="zoom: 25%;" />

<img src="assets/Screenshot2020-07-20at10.18.48.png" alt="Screenshot 2020-07-20 at 10.18.48" style="zoom:25%;" />

`Partition Key` et `Row Key` sont de la responsabilité du développeur, le `Timestamp` est automatique et ne peut être modifié.

Une entité ne pas être plus grande que `1 MB`.

## Azure Blob

<img src="assets/Screenshot2020-07-20at10.23.39.png" alt="Screenshot 2020-07-20 at 10.23.39" style="zoom:50%;" /><img src="assets/Screenshot2020-07-20at10.25.08.png" alt="Screenshot 2020-07-20 at 10.25.08" style="zoom:50%;" />

On voit que `Azure Blob Storage` peut servir pour rendre disponible des documents directement pour le navigateur en fournissant une `url`.

## Azure Queue

<img src="assets/Screenshot2020-07-20at10.27.36.png" alt="Screenshot 2020-07-20 at 10.27.36" style="zoom:50%;" />

## Azure File

<img src="assets/Screenshot2020-07-20at10.28.34.png" alt="Screenshot 2020-07-20 at 10.28.34" style="zoom:50%;" />

Utilisation du protocole `SMB`.

#### `S`erver `M`essage `B`lock

Protocole utilisable via internet, se greffe au système de fichier `Windows`.

Solution possible pour `eprolex`.

## Azure Storage Security

<img src="assets/Screenshot2020-07-20at15.41.30.png" alt="Screenshot 2020-07-20 at 15.41.30" style="zoom:50%;" />

`Storage Account Keys` octroie un accès complet.

`Shared Access Signatures` attribut les permissions requises pour un temps limité.

### Encryption for data intransit

Utilisation de `HTTPS`.

<img src="assets/Screenshot2020-07-20at15.46.06.png" alt="Screenshot 2020-07-20 at 15.46.06" style="zoom:50%;" />

## Access Key

Permet d'accéder aux ressource depuis une application.

<img src="assets/Screenshot2020-07-22at09.31.33.png" alt="Screenshot 2020-07-22 at 09.31.33" style="zoom:33%;" />

Pour la régénérer il suffit d'appuyer sur l'icône bleue.

## Shared Access Signature `SAS`

Permet de gérer finement les accès : droit, type, durée et période.

<img src="assets/Screenshot2020-07-22at09.34.06.png" alt="Screenshot 2020-07-22 at 09.34.06" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-22at09.37.30.png" alt="Screenshot 2020-07-22 at 09.37.30" style="zoom:50%;" />

On récupère l'`url` de connection :

```
https://storagehukar1999.table.core.windows.net/?sv=2019-10-10&ss=t&srt=sco&sp=rwlacu&se=2020-07-23T21:32:32Z&st=2020-07-22T07:32:32Z&spr=https&sig=8HrdL6cTR8N0mpqaZNmwWMICl4%2BXj1ooTNvt7PH6Kag%3D
```

## Firewalls and virtual network

<img src="assets/Screenshot2020-07-22at09.40.51.png" alt="Screenshot 2020-07-22 at 09.40.51" style="zoom:50%;" />

On peut _white lister_ une adresse `IP` ou ajouter un réseau virtuel.
