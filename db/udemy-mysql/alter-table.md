# Modifier une table

## ALTER TABLE

### Ajouter une colonne

```sql
ALTER TABLE customers
ADD COLUMN gender ENUM('m', 'f') AFTER last_name;
```

`AFTER` pour positionner la nouvelle colonne.