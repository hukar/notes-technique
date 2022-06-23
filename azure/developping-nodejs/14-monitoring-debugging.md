# 14 monitoring et debugging

## Log stream

Dans `App service > Monitoring > Log stream` on peut retrouver les logs en live :

<img src="assets/Screenshot2020-07-17at09.16.37.png" alt="Screenshot 2020-07-17 at 09.16.37" style="zoom:50%;" />

On peut voire les requêtes HTTP et le démarrage.

### Log stream dans VSCode

On retrouve `Log stream` directement dans **VSCode** :

<img src="assets/Screenshot2020-07-17at09.26.10.png" alt="Screenshot 2020-07-17 at 09.26.10" style="zoom:50%;" />

Chez moi ça ne fonctionne pas très bien.

<img src="assets/Screenshot2020-07-17at09.27.26.png" alt="Screenshot 2020-07-17 at 09.27.26" style="zoom:50%;" />

Après un certain temps j'ai bien eu les logs.

Pour fermer le `stream` de `logs` : `cmd + shift +p`

<img src="assets/azure-stop-stream-logs.png" alt="azure-stop-stream-logs" style="zoom:50%;" />

## Monitoring / Diagnostic setting

On peut ajouter d'une manière flexible une collecte de logs ou de métriques.

On peut les archiver dans un `storage account`.

<img src="assets/Screenshot2020-07-17at09.32.35.png" alt="Screenshot 2020-07-17 at 09.32.35" style="zoom:50%;" />

## Application insight

<img src="assets/Screenshot2020-07-17at09.35.59.png" alt="Screenshot 2020-07-17 at 09.35.59" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-17at09.36.32.png" alt="Screenshot 2020-07-17 at 09.36.32" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-17at09.38.29.png" alt="Screenshot 2020-07-17 at 09.38.29" style="zoom:50%;" />

On peut cliquer sur le lien :

<img src="assets/Screenshot2020-07-17at10.25.25.png" alt="Screenshot 2020-07-17 at 10.25.25" style="zoom:50%;" />

### Live Metrics

Un peu comme `Log stream`.

<img src="assets/Screenshot2020-07-17at10.28.43.png" alt="Screenshot 2020-07-17 at 10.28.43" style="zoom:50%;" />

### `robots933456.txt`

Fichier test du conteneur utilisé par Azure pour voire si l'application répond correctement.

### Performance

<img src="assets/Screenshot2020-07-17at10.33.43.png" alt="Screenshot 2020-07-17 at 10.33.43" style="zoom:50%;" />

On peut voire les temps de réponse des requêtes.

On peut aussi voire d'où viennent les requêtes :

<img src="assets/Screenshot2020-07-17at10.35.21.png" alt="Screenshot 2020-07-17 at 10.35.21" style="zoom:50%;" />

## Failures

On peut voire toutes les requêtes ayant provoquées une erreur :

<img src="assets/Screenshot2020-07-17at10.39.47.png" alt="Screenshot 2020-07-17 at 10.39.47" style="zoom:50%;" />
