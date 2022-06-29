# 01 `TABLE`

## Renommer une table

`sql server` ne dispose pas de commande pour cela mais d'une procédure stockée :

```sql
EXEC sp_rename 'ancienNom', 'nouveauNom'
```

attention à la virgule.



## Ajouter une colonne

```sql
ALTER TABLE Product 
ADD RetireDate DATETIME;
GO
```



## Modifier le type d'une colonne

Pour éviter une erreur de type 

```
ALTER TABLE ALTER COLUMN Price failed because one or more objects access this column.
```

On doit d'abord supprimer la `CONSTRAINT`.

```sql
ALTER TABLE Product
DROP CONSTRAINT DF__Product__Price__267ABA7A;
GO

ALTER TABLE Product
ALTER COLUMN Price decimal
```



## Modifier la valeur par défaut d'une colonne

```sql
ALTER TABLE Product ADD CONSTRAINT DF_IsDeletedDefault DEFAULT 1 FOR IsDeleted
```

Sans utiliser de contrainte :

```sql
ALTER TABLE Product ADD DEFAULT 1 FOR IsDeleted
```

