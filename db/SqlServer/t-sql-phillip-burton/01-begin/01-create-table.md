# 01 Créer une `table`



## Créer une `table` avec `T-SQL`

`[dbo]` est un standard pour `database owner`.

```sql
CREATE TABLE tblFirst 
( myNumbers int )
```



## Insérer des données `INSERT INTO`

```sql
INSERT INTO tblSecond
VALUES (234)
```

### Rafraîchir le cache de l'`Intellisense`

<img src="assets/intellisense-not-know-daa.png" alt="intellisense-not-know-daa" style="zoom:67%;" />

L'`intellisense` ne semble pas connaître la table fraîchement créée `tblSecond`.

Il faut rafraîchir le cache de l'`intellisense` : on ouvre la palette de commande.

<img src="assets/intellisense-refresh-jpl.png" alt="intellisense-refresh-jpl" style="zoom:50%;" />

Et voila cela fonctionne :

<img src="assets/cache-refreshing-ggh.png" alt="cache-refreshing-ggh" style="zoom: 67%;" />

> On peut `drag and drop` le nom de la table dans l'interface de `Query`
>
> <img src="assets/drag-and-drop-table-name-rrt.png" alt="drag-and-drop-table-name-rrt" style="zoom:50%;" />



## Insérer plusieurs lignes

```sql
INSERT INTO tblFirst VALUES (543), (619), (200)
```



## Écriture avec crochets `[]`

```sql
SELECT * FROM [dbo].[tblSecond]
```

Les crochets ne sont nécessaire que si le nom de la table contient des caractères interdits ou des espaces.

On peut les enlever :

```sql
SELECT * FROM dbo.tblSecond
```



## Effacer des données `DELETE`, `TRUNCATE` et `DROP`

```sql
DELETE FROM tblSecond
```

Efface les données pas la table.

```sql
TRUNCATE TABLE tblFirst
```

Vide la table des ses données.

```sql
DROP TABLE tblFirst
```

Détruit la table et les données.