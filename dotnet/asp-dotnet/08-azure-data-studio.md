# 08 Azure Data Studio

## Supprimer une `BDD`

Souvent il est impossible de supprimer une base de données car elle est utilisée.

On doit utiliser ce code :

```sql
USE master;
GO

ALTER DATABASE WizLib_db SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
GO
DROP DATABASE WizLib_db;
GO
```

Où `WizLib_db` est la `BDD` à supprimer.



## Vider une `table`

```sql
DELETE FROM ExpenseCategories;
DBCC CHECKIDENT (ExpenseCategories, RESEED, 0);
```

La deuxième ligne sert à remettre au début le compte des `id` (commence à `1`).

