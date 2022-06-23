# Créer une DB `Cosmos DB`

## Basic

<img src="assets/Screenshot2020-07-24at10.37.45.png" alt="Screenshot 2020-07-24 at 10.37.45" style="zoom: 25%;" />

`Account Type` : `Non-production` permet d'avoir une DB moins chère pour le développement.

## Networking

<img src="assets/Screenshot2020-07-24at14.28.17.png" alt="Screenshot 2020-07-24 at 14.28.17" style="zoom: 33%;" />

Pour la production `Private endpoint` permet de ne pas faire passer la connexion sur internet.

C'est un choix à retenir pour `eprolex`.

## Encryption et Tags

<img src="assets/Screenshot2020-07-24at14.31.08.png" alt="Screenshot 2020-07-24 at 14.31.08" style="zoom:33%;" />

<img src="assets/Screenshot2020-07-24at14.31.33.png" alt="Screenshot 2020-07-24 at 14.31.33" style="zoom:33%;" />

On peut proposer une clé d'encryptage personnel ou utiliser celle d'Azure.

Ajouter des tags est une bonne pratique pour un projet réel.

## Ressource `Cosmos DB`

<img src="assets/Screenshot2020-07-24at14.50.58.png" alt="Screenshot 2020-07-24 at 14.50.58" style="zoom:50%;" />

Ma ressource est créée, on voit qu'elle est gratuite pour `400RU/s` et `5 GB`.

## Recommendations

<img src="assets/Screenshot2020-07-24at14.55.14.png" alt="Screenshot 2020-07-24 at 14.55.14" style="zoom:50%;" />

Dans les notifications j'ai accès aux recommendations.

## Création d'un `container`

<img src="assets/Screenshot2020-07-24at15.05.08.png" alt="Screenshot 2020-07-24 at 15.05.08" style="zoom:50%;" />

Une base de données peut avoir un ou plusieurs `container`.

Un `container` est une unités définissant la scalabilité du débit ains que le stockage.<img src="assets/Screenshot2020-07-24at15.10.43.png" alt="Screenshot 2020-07-24 at 15.10.43" style="zoom: 50%;" />

Avec `Provision database throughput` coché, le débit est partagé entre tous les `container` de la base de données.

<img src="assets/Screenshot2020-07-24at15.12.43.png" alt="Screenshot 2020-07-24 at 15.12.43" style="zoom:50%;" />

Sinon le débit (`throughput`) est défini pour ce seul `container`.

<img src="assets/Screenshot2020-07-24at15.17.02.png" alt="Screenshot 2020-07-24 at 15.17.02" style="zoom:50%;" />

Bien choisir sa `partition key`.

<img src="assets/Screenshot2020-07-24at15.17.47.png" alt="Screenshot 2020-07-24 at 15.17.47" style="zoom:50%;" />

## Ajout d'un item

<img src="assets/Screenshot2020-07-24at15.20.31.png" alt="Screenshot 2020-07-24 at 15.20.31" style="zoom:50%;" />

On clique sur `New Item` :

<img src="assets/Screenshot2020-07-24at15.24.39.png" alt="Screenshot 2020-07-24 at 15.24.39" style="zoom:50%;" />

On insère ses données et on clique cette fois sur `save` :

<img src="assets/Screenshot2020-07-24at15.25.58.png" alt="Screenshot 2020-07-24 at 15.25.58" style="zoom:50%;" />

`Cosmos DB` ajoute cinq attributs automatiquement.

## TTL Time To Live

On peut régler la durée de vie :

<img src="assets/Screenshot2020-07-24at15.29.20.png" alt="Screenshot 2020-07-24 at 15.29.20" style="zoom:50%;" />

Dans `settings` on passe `Time To Live ` à `On`.

On défini ensuite une durée de vie en secondes :

<img src="assets/Screenshot2020-07-24at15.29.38.png" alt="Screenshot 2020-07-24 at 15.29.38" style="zoom:50%;" />

Ici la durée de vie est réglée à `10 s`.

J'ai deux `items` dans mon `container`:

<img src="assets/Screenshot2020-07-24at15.29.50.png" alt="Screenshot 2020-07-24 at 15.29.50" style="zoom:50%;" />

Et après `10 s` le nettoyage automatique est opéré :

<img src="assets/Screenshot2020-07-24at15.33.01.png" alt="Screenshot 2020-07-24 at 15.33.01" style="zoom:50%;" />

## Supprimer son `container` et sa `DB`

### le `container`

<img src="assets/Screenshot2020-07-24at15.34.22.png" alt="Screenshot 2020-07-24 at 15.34.22" style="zoom:50%;" />

### Puis la `DB`

<img src="assets/Screenshot2020-07-24at15.35.36.png" alt="Screenshot 2020-07-24 at 15.35.36" style="zoom: 33%;" />
