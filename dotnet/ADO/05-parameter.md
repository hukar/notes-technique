# 05 `Parameter Class`

La pluspart du temps on reçoit des variables dont les valeurs sont à insérer en `DB`.

```cs
sql = "... VALUES (@ProductName ...)";

cmd.Parameters.Add(new SqlParameter("@ProductName", "New Product"));
```

`@Parameter` est une propriété de `SQL Server`.

## OUTPUT parameters

On peut aussi recevoir des `sql parameters` :

```cs
cmd.Parameters.Add(
	new SqlParameter {
        ParameterName = "#ProductId",
        Value = 1,
        DbType = DbType.Int32,
        Direction = ParameterDirection.Output
    }
)
```

Utilisé pour les `Procédures Stockées`.



## Exemple avec `ExecuteScalar`

```cs
public int GetProductCountScalarUsingparameters()
{
    var sql = @"SELECT COUNT(*) FROM Product WHERE Price > @value";
	// var sql = @"SELECT COUNT(*) FROM Product WHERE Name LIKE @value";
    
    using var cnn = new SqlConnection(cnnString);
    using var cmd = new SqlCommand(sql, cnn);

    cmd.CommandType = CommandType.Text;

    cmd.Parameters.Add(new SqlParameter("@value", 700));

    cnn.Open();

    var countOfProduct = cmd.ExecuteScalar(); // retourne [object]
    // int countOfProduct = (int)cmd.ExecuteScalar()
    return countOfProduct;
}
```

La valeur sera en réalité passée par une variable reçu de l'utilisateur.



## Exemple avec `ExecuteNonQuery`

```cs
IResult ExecuteNonQuery() {
    var sql = @"INSERT INTO Product (Name, Price) VALUES (@ProductName, @ProductPrice)";

    try {
        using var cnn = new SqlConnection(cnnString);
        using var cmd = new SqlCommand(sql, cnn);

        cmd.CommandType = CommandType.Text;

        cmd.Parameters.Add(new SqlParameter("@ProductName", "New Bike"));
        cmd.Parameters.Add(new SqlParameter("@ProductPrice", 899));
        
        cnn.Open();
            
        var rowsAffected = cmd.ExecuteNonQuery();
        return Ok(rowsAffected);
    }
    catch(Exception ex)
    {
        return BadRequest(ex.ToString());
    }
};
```

