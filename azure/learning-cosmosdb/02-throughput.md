# 02 Le débit : `throughput`

<img src="assets/Screenshot2020-07-27at08.03.04.png" alt="Screenshot 2020-07-27 at 08.03.04" style="zoom:50%;" />

### La latence

Combien de temps doit on attendre entre la requête et la réponse.

### Le débit

Le nombre de requête par seconde que la DB peut gérer.

## `RU` : Request Unit

C'est l'unité de mesure du débit.

<img src="assets/Screenshot2020-07-27at08.06.20.png" alt="Screenshot 2020-07-27 at 08.06.20" style="zoom:50%;" />

Les `RU` mesure le coût d'une requête en tenant compte des divers facteurs hardware.

Toutes les requêtes n'ont pas le même prix, une écriture est plus chère qu'une lecture car implique plus de serveurs par exemple.

Une même requête coutera toujours un même nombre de `RU`, Les `Request Units` sont déterministes.

## Réserver des `RU`

<img src="assets/Screenshot2020-07-27at08.18.42.png" alt="Screenshot 2020-07-27 at 08.18.42" style="zoom:50%;" />

On réserve des `Request Units` pour un `container`.

Si on dépasse notre réserve de débit, nos requêtes arrivent dans un goulot d'étranglement et une requête de rejet nous est adressée `HTTP 429` : `Too many request`.

Une stratégie de ré-envoie peut être implémentée.

## Observer la consommation en `Request Units`

On peut facilement savoir combien de `RU` sont dépensés par une requête dans l'interface de `Cosmos DB` :

<img src="assets/Screenshot2020-07-27at08.36.15.png" alt="Screenshot 2020-07-27 at 08.36.15" style="zoom:50%;" />

On voit ici `2.96 RUs`.

<img src="assets/Screenshot2020-07-27at08.44.19.png" alt="Screenshot 2020-07-27 at 08.44.19" style="zoom:50%;" />

Maintenant `3.13 RUs` pour requête plus complexes.

On peut le savoir programmatiquement grace au `sdk` (ici en `dotnet`)

```c#
var result = await container.CreateItemAsync(document, new PartitionKey(document.address.zipCode));
var consumdRUs = result.RequestCharge
```

### `result.RequestCharge`

Ce sera certainement très proche dans `Node js`.

Dans le portail on a accès à toutes les infos dans `Metrics` :

> **update** maintenant on trouve cet écran dans `Metrics (Classic)` , `Metrics` étant paramétrable.

<img src="assets/Screenshot2020-07-27at08.48.04.png" alt="Screenshot 2020-07-27 at 08.48.04" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-27at08.48.54.png" alt="Screenshot 2020-07-27 at 08.48.54" style="zoom:33%;" />

Tout en bas on peut voire `Number of request exceed capacity Http 429`, cela peut nous permettre de savoir s'il faut gérer ce cas de figure et/ou ajouter du débit.

### Nouvel interface `Metrics`

<img src="assets/new-metrics-page.png" alt="new-metrics-page" style="zoom:50%;" />



## `HTTP 429 Too Many Requests`

Si on dépasse le `throughput` alloué, une erreur `HTTP 429` est levée.

<img src="assets/http-too-many-requests.png" alt="http-too-many-requests" style="zoom:50%;" />

`x-ms-retry-after-ms: 2853` indique le temps en milliseconde avant que la requête soit relancée automatiquement.

Une conception judicieuse de sa `DB` peut éviter des surcoûts et un effet de `throttling` lorsque l'approvisionnement est dépassé.

### Gestion programmatique

<img src="assets/programmatic-handling-error.png" alt="programmatic-handling-error" style="zoom:50%;" />

```c#
try
{
  await container.createItemAsync(docDef, new PartitionKey(docDef.pk));
}
catch(CosmosException ex) when (ex.StatusCode == HttpStatusCode.TooManyRequests) // 429
{
  Console.WriteLine("Can't create document, request was throttled")
}
```

On peut renvoyer la requête, le `SDK .net` a déjà ré-essayer d'envoyer la requête avant de lancer l'exception.

## Outil : `Fiddler Everywhere`

Pour observer les requêtes.

On peut voire que Azure relance automatiquement la requête `Cosmos DB` avant d'envoyer une erreur `429`.

<img src="assets/retry-429-fddler.png" alt="retry-429-fddler" style="zoom:50%;" />

## Les offres de débit (`throughput`)

### 1. Provisioned Throughput (manual) : manuellement

Quand on a un débit prévisible :

<img src="assets/provisioned-throughput.png" alt="provisioned-throughput" style="zoom:50%;" />

Il suffit d'ajuster manuellement son débit à la densité de la période :

<img src="assets/previsble-throughput.png" alt="previsble-throughput" style="zoom:50%;" />

Pour cela il faut pouvoir prédire les périodes

### 2. Provisioned Throughput (auto-scale) : mise à l'échelle automatique

<img src="assets/throughput-auto-scale.png" alt="throughput-auto-scale" style="zoom:50%;" />

Ce service est plus cher par `RU` que la version `manual`.

<img src="assets/throughput-adaptative.png" alt="throughput-adaptative" style="zoom:50%;" />

### 3. Serverless

On ne paye que les `RU` dépensés au moment où ils sont utilisés.

<img src="assets/serverless-throughput.png" alt="serverless-throughput" style="zoom: 33%;" />

<img src="assets/spicky-serverless-0618193.png" alt="spicky-serverless" style="zoom:50%;" />

Avec ce système on ne paye que ce qu'on consomme.



## Approvisionner le débit de la DB

<img src="assets/prov-db.png" alt="prov-db" style="zoom:50%;" />

On peut approvisionner :

- chaque `container` indépendamment.
- La base de données qui partagera son débit entre les différents `container`.
- Un mix des deux, en sachant que l'approvisionnement d'un `container` override celui de la `DB`.

<img src="assets/Screenshot2020-07-27at15.03.36-5855054.png" alt="Screenshot 2020-07-27 at 15.03.36" style="zoom:33%;" />

`provision database throughput` n'est pas coché.

<img src="assets/Screenshot2020-07-27at15.03.44.png" alt="Screenshot 2020-07-27 at 15.03.44" style="zoom:50%;" />

Ici on approvisionne le `container` indépendamment de la `DB`.



## Approvisionner le débit

3 scénario sont possible :

### Approvisonner `container` par `container`

<img src="assets/approvisionning-per-container.png" alt="approvisionning-per-container" style="zoom:50%;" />

C'est la solution la plus courante.

### Approvisionner la `database`

<img src="assets/provisionning-databse.png" alt="provisionning-databse" style="zoom:50%;" />

Le débit (`throughput`) est divisé également (? ou pas justement : par exemple 100RU sur un et 2000RU sur un autre si nécessaire ?) entre les `container`.

- Pour des raisons de migration d'application.
- Quand on a différentes clés de partition.

### Mix des deux stratégies

<img src="assets/mix-throughput-aprovisionning-strategy.png" alt="mix-throughput-aprovisionning-strategy" style="zoom:50%;" />



## Établir les coûts

<img src="assets/whiteboard-costs.png" alt="whiteboard-costs" style="zoom:50%;" />

De manière générale, les lectures coûtent moins que les écritures et plus une requête est complexe, plus elle coûte.

<img src="assets/Screenshot2020-07-27at09.25.19.png" alt="Screenshot 2020-07-27 at 09.25.19" style="zoom:50%;" />

Une estimation des coûts peut être réalisée en listant les requêtes et les items, en évaluant le nombre d'écriture et de lectures par seconde et en choisissant la politique d'indexation et de cohérence (`consistency`).

## Capacity calculator

https://cosmos.azure.com/capacitycalculator/

<img src="assets/Screenshot2020-07-27at14.54.20.png" alt="Screenshot 2020-07-27 at 14.54.20" style="zoom:50%;" />

## Le prix

<img src="assets/Screenshot2020-07-27at14.57.01.png" alt="Screenshot 2020-07-27 at 14.57.01" style="zoom:50%;" />

Le `storage` est facturé mensuellement. Le prix est fixé à la consommation.

Pour le `throughput` le prix est fixé à la réservation. Il est facturé à l'heure (si on le change manuellement par exemple ou en auto-scaling).

Le minimum est donc `24$`.

<img src="assets/all-prices.png" alt="all-prices" style="zoom:50%;" />

Il est clair que pour un petit projet, `serverless` est le plus adapté.

