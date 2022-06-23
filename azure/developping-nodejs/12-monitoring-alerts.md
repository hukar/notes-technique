# 12 Monitoring

## `Metrics`

Le service `metrics` permet d'avoir une vue sur nos applications.

Ce service fait maintenant partie de `monitor`:

<img src="assets/monitor-metrics.png" alt="monitor-metrics" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-16at14.37.03.png" alt="Screenshot 2020-07-16 at 14.37.03" style="zoom:50%;" />

Je choisie d'avoir des infos sur la consommation `cpu` ainsi que la mémoire :

<img src="assets/Screenshot2020-07-16at14.38.31.png" alt="Screenshot 2020-07-16 at 14.38.31" style="zoom:50%;" />

## Alertes

<img src="assets/Screenshot2020-07-16at14.46.44.png" alt="Screenshot 2020-07-16 at 14.46.44" style="zoom:50%;" />

On va créer une alerte basée sur le monitoring.

On choisie `App Resource Plan` et `webapp-plan` :

<img src="assets/Screenshot2020-07-16at14.48.31.png" alt="Screenshot 2020-07-16 at 14.48.31" style="zoom:50%;" />

### Problème

Le bouton `condition` n'apparaissait pas.

J'ai dû enregistrer des `provider` :

<img src="assets/Screenshot2020-07-16at15.04.53.png" alt="Screenshot 2020-07-16 at 15.04.53" style="zoom:50%;" />

`Microsoft.Insight` et `Microsoft.AlertsManagment` n'étaient pas enregistrés.

### Retour à l'alerte

Ajout d'une condition :

<img src="assets/Screenshot2020-07-16at15.07.29.png" alt="Screenshot 2020-07-16 at 15.07.29" style="zoom:50%;" />

Condition sur les erreurs HTTP.

On défini un seuil :

<img src="assets/Screenshot2020-07-16at15.07.21.png" alt="Screenshot 2020-07-16 at 15.07.21" style="zoom:50%;" />

Plus de deux erreurs sur 5mn.

On crée ensuite un Action Group :

<img src="assets/Screenshot2020-07-16at15.15.05.png" alt="Screenshot 2020-07-16 at 15.15.05" style="zoom:50%;" />

On peut choisir le type d'action, et il y a plein de possibilités : Logic App Azure Function, email-sms, ...

### Email / SMS

<img src="assets/Screenshot2020-07-16at15.20.20.png" alt="Screenshot 2020-07-16 at 15.20.20" style="zoom:33%;" />

Il ne reste plus qu'à cliquer sur `ok` :

<img src="assets/Screenshot2020-07-16at15.30.17.png" alt="Screenshot 2020-07-16 at 15.30.17" style="zoom:50%;" />

<img src="assets/Screenshot2020-07-16at15.31.25.png" alt="Screenshot 2020-07-16 at 15.31.25" style="zoom:50%;" />

J'obtiens aussi une estimation du coût, ici 0.10 $ par mois.

### Détails de l'alerte

On doit maintenant donner un nom à l'alerte :

<img src="assets/Screenshot2020-07-16at15.36.08.png" alt="Screenshot 2020-07-16 at 15.36.08" style="zoom:50%;" />

On la retrouve dans l'onglet Alert Managment :

<img src="assets/Screenshot2020-07-16at15.37.04.png" alt="Screenshot 2020-07-16 at 15.37.04" style="zoom:50%;" />

### Reception de l'alerte

<img src="assets/Screenshot2020-07-17at10.15.27.png" alt="Screenshot 2020-07-17 at 10.15.27" style="zoom:50%;" />

Après avoir volontairement déclenché des erreurs http.
