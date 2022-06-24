# 01 `TABLE`

## Renommer une table

`sql server` ne dispose pas de commande pour cela mais d'une procédure stockée :

```sql
EXEC sp_rename 'ancienNom', 'nouveauNom'
```

attention à la virgule.
