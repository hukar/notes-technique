# 00 - Intro

## Première requête

```sql
SELECT 1+1 AS Result
```

le mot clé `AS` est optionnel mais aide à la lisibilité.

Le point virgule à la fin est optionnel (utile dans de rare cas).

`SELECT` et `AS` ne sont pas sensible à la case est sont en majuscule par convention.



### `SELECT`

`SELECT` est l'équivalent de `print` pour `SQL`.



### `GO`

Sans le mot clé `GO`, les instructions `SQL`sont exécutées en `Batch` (toutes ensembles).

Pour séparer les instruction en `Batch` séparés, on utilise le mot clé `GO`.

```sql
SELECT 1+1 AS Result
SELECT 1/0 AS Result
SELECT 1*3 AS Result
```

ou

```sql
SELECT 1+1 AS Result
GO
SELECT 1/0 AS Result
GO
SELECT 1*3 AS Result
```

Les deux codes s'exécute à l'identique. L'erreur de la ligne avec la division n'empêche pas dans les deux cas l'exécution des deux autres requêtes.

