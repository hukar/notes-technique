# CC Recevoir - récupérer l'`Id`



## `INSERT INTO`

On veut récupérer l'`Id` du nouvel enregistrement en base de données.



### 1. `SCOPE_IDENTITY`

```cs
var sql = @"INSERT INTO Product (Name, Price)
            VALUES (@Name, @Price);SELECT CAST(SCOPE_IDENTITY() as int)";
// ...

var idInserted = cmd.ExecuteScalar();
```



### 2. `OUTPUT` parameter

Récupérer juste l'`Id`

```cs
var sql = @"INSERT INTO Product (Name, Price)
            OUTPUT INSERTED.Id
            VALUES (@Name, @Price);";
// ...

var idInserted = cmd.ExecuteScalar();
```



Récupérer l'objet insérer

```cs
var sql = @"INSERT INTO Product (Name, Price)
            OUTPUT INSERTED.*
            VALUES (@Name, @Price);";
// ...

using var reader = cmd.ExecuteReader();

reader.Read();

var objectInserted = new ProductDto(
    reader.GetInt32(0), 
    reader.GetString(1), 
    reader.GetDecimal(2)
);

return Created($"product/{objectInserted.Id}", objectInserted);
```

<img src="assets/beautiful-request-get-by-id-ado-net-ijj.png" alt="beautiful-request-get-by-id-ado-net-ijj" style="zoom:50%;" />