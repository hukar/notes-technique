# 03 Un `CRUD` avec `Dapper` et `Sqlite`



## La classe `RobotRepository`

```cs
public class RobotRepository
{
    private readonly DapperContext _context = new();
    
    // Les méthodes du CRUD
}
```

Voire la fiche `O1-installation` pour le `DapperContext`.

## Lire : `GetAll` et `GetById`

```cs
public async Task<IEnumerable<Robot>> GetAll()
{
    var sql = @"SELECT * FROM Robot";

    using var connection = _context.CreateConnection();

    return await connection.QueryAsync<Robot>(sql);
}
```

```cs
public async Task<Robot> GetById(int id)
{
    var sql = @"SELECT * 
                FROM Robot 
                WHERE Id = @Id";

    using var connection = _context.CreateConnection();

    return await connection.QuerySingleOrDefaultAsync<Robot>(sql, new { Id = id});
}
```



### Dans le cas d'un `rowid` automatique :

```cs
public async Task<IEnumerable<Weapon>> GetAll()
{
    var sql = @"SELECT rowid AS Id, BrandName, Power FROM Weapon";

    using var connection = _context.CreateConnection();

    return await connection.QueryAsync<Weapon>(sql);
}
```



## Créer : `Create`

```cs
public async Task<int> Create(Robot robot)
{
    var sql = @"INSERT INTO Robot 
                VALUES (null, @CodeName, @CreatedAt)";

    using var connection = _context.CreateConnection();

    var rowsAffected = await connection.ExecuteAsync(sql, robot);

    return rowsAffected;
}
```

On insère `null` pour l'`Id`.

On peut modifier le `sql` comme ceci:

```csharp
var sql = @"INSERT INTO Robot (CodeName, CreatedAt) 
            VALUES (@CodeName, @CreatedAt)";
```



## Modifier : `Update`

```cs
public async Task<int> Update(Robot robot)
{
    var sql = @"UPDATE Robot 
                SET CodeName=@CodeName 
                WHERE Id = @Id";

    using var connection = _context.CreateConnection();

    var rowsAffected = await connection.ExecuteAsync(sql, robot);
    return rowsAffected;
}
```



## Supprimer : `Delete`

```cs
public async Task<int> Delete(int id)
{
    var sql = @"DELETE FROM Robot 
                WHERE Id = @Id";

    using var connection = _context.CreateConnection();

    var rowsAffected = await connection.ExecuteAsync(sql, new { Id = id});
    return rowsAffected;
}
```

