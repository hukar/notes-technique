# EE `CRUD` avec `ado.net`



## `ProductEndpoints`

 Création de la méthode d'extension pour regrouper les `endpoints` nécessaires

```cs
public static class ProductEndpoints
{
    public static WebApplication MapProduct(this WebApplication app)
    {
        app.MapGet("/product", GetAllProduct);
        app.MapGet("/product/{id:int}", GetProductById);
        app.MapPost("/product", AddProduct);
        app.MapPut("/product", UpdateProduct);
        app.MapDelete("/product/{id:int}", DeleteProduct);
        
        // Implémentation des différentes méthodes
        
    	return app;
    }   
}

record ProductDto(int Id, string Name, decimal? Price);
record ProductAddDto(string Name, decimal? Price);
```



### Remarque

`HuConnection` est un service qui expose `SqlConnection` , on peut tout simplement le remplacer par un :

```cs
var connectionString = configuration.GetConnectionString("HukarConnect");
using var cnn = new SqlConnection(connectionString);
```

 `configuration` est passé par `DI`. On pourrait passer directement le `connection string` par `DI` avec une classe dédiée.

## `GET ALL PRODUCTS`

```cs
IResult GetAllProduct(HuConnection connection)
{
    List<ProductDto> products = new();

    var sql = "SELECT * FROM Product";

    var cnn = connection.Connection;
    cnn.Open();

    using var cmd = new SqlCommand(sql, cnn);
    using var reader = cmd.ExecuteReader();

    var indexId = reader.GetOrdinal("Id");
    var indexName = reader.GetOrdinal("Name");
    var indexPrice = reader.GetOrdinal("Price");

    while(reader.Read())
    {
        products.Add(new ProductDto(
            reader.GetInt32(indexId),
            reader.GetString(indexName),
            reader.IsDBNull(indexPrice) ? null : reader.GetDecimal(indexPrice)
        ));
    }

    return Ok(products);
}
```



## `GET PRODUCT BY ID`

```cs
IResult GetProductById(int id, HuConnection connection)
{
    var slq = @"SELECT * 
                FROM Product 
                WHERE Id=@Id";

    var cnn = connection.Connection;
    using var cmd = new SqlCommand(slq, cnn);
    cnn.Open();

    cmd.Parameters.Add(new SqlParameter("@Id", id));
    using var reader = cmd.ExecuteReader();

    if(reader.Read() == false) {
        return NotFound(new { error = "This id doesn't exist in the DB" });
    }

    var product = new ProductDto(
        reader.GetInt32(reader.GetOrdinal("Id")),
        reader.GetString(reader.GetOrdinal("Name")),
        reader.IsDBNull(reader.GetOrdinal("Price")) ? null : reader.GetDecimal(reader.GetOrdinal("Price"))
    );

    return Ok(product);
} 
```



## `CREATE PRODUCT`

```cs
IResult AddProduct(ProductAddDto product, HuConnection connection)
{
    var sql = @"INSERT INTO Product (Name, Price)
                OUTPUT INSERTED.*
                VALUES (@Name, @Price);";
    var cnn = connection.Connection;
    cnn.Open();

    using var cmd = new SqlCommand(sql, cnn);
    cmd.CommandType = CommandType.Text;

    cmd.Parameters.Add(new SqlParameter("@Name", product.Name));
    cmd.Parameters.Add(new SqlParameter("@Price", product.Price));

    using var reader = cmd.ExecuteReader();

    reader.Read();

    var objectInserted = new ProductDto(
        reader.GetInt32(0), 
        reader.GetString(1), 
        reader.GetDecimal(2)
    );

    return Created($"product/{objectInserted.Id}", objectInserted);
}
```



## `UPDATE PRODUCT`

```cs
IResult UpdateProduct(ProductDto product, HuConnection connection)
{
    var sql = @"UPDATE Product
                SET Name = @Name, Price = @Price
                WHERE Id=@Id";

    var cnn = connection.Connection;
    cnn.Open();
    using var cmd = new SqlCommand(sql, cnn);

    cmd.Parameters.Add(new SqlParameter("@Id", product.Id));
    cmd.Parameters.Add(new SqlParameter("@Name", product.Name));
    cmd.Parameters.Add(new SqlParameter("@Price", product.Price));

    var rowsAffected = cmd.ExecuteNonQuery();

    Console.WriteLine($"rows affected {rowsAffected}");

    if(rowsAffected == 0) {
        return NotFound(new { error = "This id doesn't exist in the DB" });
    }

    return NoContent();
}
```



## `DELETE PRODUCT`

```cs
IResult DeleteProduct(int id, HuConnection connection)
{
    var sql = @"DELETE FROM Product WHERE Id = @Id";

    var cnn = connection.Connection;
    cnn.Open();

    using var cmd = new SqlCommand(sql, cnn);

    cmd.Parameters.Add(new SqlParameter("@Id", id));

    var rowsAffected = cmd.ExecuteNonQuery();

    if(rowsAffected == 0) {
        return NotFound(new { error = "The Id is not in the DB" });
    }

    return NoContent();
}
```

