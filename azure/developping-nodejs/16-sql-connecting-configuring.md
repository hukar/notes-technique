# 16 Connexion et configuration Azure SQL

<img src="assets/Screenshot2020-07-17at11.39.04.png" alt="Screenshot 2020-07-17 at 11.39.04" style="zoom:50%;" />

## Alertes

Un click sur le graphe permet de faire apparaître une option d'alerte :

<img src="assets/Screenshot2020-07-17at11.40.59.png" alt="Screenshot 2020-07-17 at 11.40.59" style="zoom:50%;" />

## Firewall

On ajoute son adresse `IP` :

<img src="assets/Screenshot2020-07-17at11.43.43.png" alt="Screenshot 2020-07-17 at 11.43.43" style="zoom: 33%;" />

Seul cette adresse peut accéder à la base de données.

## VSCode

On installe l'extension `mssql`.

<img src="assets/Screenshot2020-07-17at11.53.01.png" alt="Screenshot 2020-07-17 at 11.53.01" style="zoom:50%;" />

### Connexion à la DB

<img src="assets/Screenshot2020-07-17at11.58.01.png" alt="Screenshot 2020-07-17 at 11.58.01" style="zoom:33%;" />

On crée une nouvelle connexion.

<img src="assets/Screenshot2020-07-17at11.57.55.png" alt="Screenshot 2020-07-17 at 11.57.55" style="zoom:33%;" />

On renseigne les credentials, on choisie le SQL login.

<img src="assets/Screenshot2020-07-17at12.00.55.png" alt="Screenshot 2020-07-17 at 12.00.55" style="zoom: 33%;" />

On est connecté.

## Création d'un `USER`

On crée à la racine un fichier `setup.sql` :

<img src="assets/Screenshot2020-07-17at12.04.15.png" alt="Screenshot 2020-07-17 at 12.04.15" style="zoom:33%;" />

On peut taper dedans ses requêtes :

```sql
CREATE USER weappuser WITH PASSWORD="ferTREgh56yhjju89"
```

> **new** : `webhuk` pwd:`abc123babyYouAndMe`

Et l'exécuter :

<img src="assets/Screenshot2020-07-17at13.38.40.png" alt="Screenshot 2020-07-17 at 13.38.40" style="zoom:33%;" />

<img src="assets/Screenshot2020-07-17at13.43.34.png" alt="Screenshot 2020-07-17 at 13.43.34" style="zoom:33%;" />

### Ajouter des droits en lecture et en écriture

Ce sont des procédures stockées `stored procedure` = `sp_`

```sql
EXECUTE sp_addrolemember db_datareader, "weappuser"
EXECUTE sp_addrolemember db_datawriter, "weappuser"
```

Puis `execute query`.

## Création d'une table `users`

```sql
CREATE TABLE users
(
    id INT IDENTITY PRIMARY KEY,
    name NVARCHAR(255),
    email NVARCHAR(255),
)
```

<img src="assets/Screenshot2020-07-17at13.56.04.png" alt="Screenshot 2020-07-17 at 13.56.04" style="zoom:50%;" />

Pour l'instant la table est vide.
