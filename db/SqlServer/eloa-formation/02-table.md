# 02 `table`



## Création d'une `table`

Une table est une relation.

```sql
-- utilise cet BDD
USE EloaBdd

-- Création d'une table simple
CREATE TABLE ma_table (nom varchar(200))

-- création d'une table simple avec deux colonnes
CREATE TABLE ma_table2 (nom varchar(200), prenom varchar(200))
```

On peut écrire `[EloaBdd]`, les crochets sont facultatifs.

## Modifier une `table`

La modification d'une `table` passe par la procédure stockée : `Sp_rename`.

```sql
-- renommer une table
sp_rename 'ma_table', 'new_table'
```



## Supprimer une `table`

On utilise `DROP TABLE`.

```sql
-- Supprimer une table
DROP TABLE new_table
```

