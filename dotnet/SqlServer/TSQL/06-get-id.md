# 06 Récupérer l'`Id`



## Avec `SCOPE_IDENTITY`

```sql
INSERT INTO Product (Name, Price)
VALUES (@Name, @Price);
SELECT CAST(SCOPE_IDENTITY() as int)
```



## Avec `OUTPUT`

```sql
INSERT INTO Product (Name, Price)
OUTPUT INSERTED.Id
VALUES (@Name, @Price);
```



### Récupérer toute la ligne

```sql
INSERT INTO Product (Name, Price)
OUTPUT INSERTED.*
VALUES (@Name, @Price);
```

