# 02 structure de `mongoDB`

Une base de données `mongoDB` consite en un ensemble de collections contenant des documents.

Par défaut `mongoDB` fournis deux bases de données `admin` et `local`.

<img src="assets/Screenshot2020-03-24at09.55.43.png" alt="Screenshot 2020-03-24 at 09.55.43" style="zoom: 67%;" />

Ce sont les outils fournis par `mongoDB`

- `mongod` le serviec de base de données à proprement parlé
- `mongo shell` un shell permettant de gérer les opérations `crud` sur la `bdd`
- `nomdump` et `mongorestore` pour réaliser un `dump` et le restaurer
- `mongoexport` et `mongoimport` pour importetr et exporter des données
- `mongostat` pour avoir des infos de performance de sa `bdd`

<img src="assets/Screenshot2020-03-24at10.02.48.png" alt="Screenshot 2020-03-24 at 10.02.48" style="zoom:67%;" />

`ssl` est vivement recommandé pour communiquer avec `mongod` (le serveur `mongoDB`).

`mongo shell` peut être sur une autre machine.

il faut un `mongo driver` pour se connecter à la `bdd` depuis le serveur de code (serveur web).
