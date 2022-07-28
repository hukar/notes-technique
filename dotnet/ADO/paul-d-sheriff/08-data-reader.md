# 08 `DataReader`

- C'est un curseur `en-avant seulement` : `forward-only`.
- la méthode la plus rapide pour lire des données
- S'assurer de fermer (utiliser un `using block`)



## Lire des données

```cs
public static IResult GetProductsAsDataReader(ConnectionString cnnStr)
{
    var sql = "SELECT * FROM Product";

    List<string> products = new();

    using var cnn = new SqlConnection(cnnStr.Get());
    using var cmd = new SqlCommand(sql, cnn);

    cnn.Open();

    using SqlDataReader dr = cmd.ExecuteReader(CommandBehavior.CloseConnection);

    while(dr.Read()) {
        products.Add($"{dr["Id"].ToString()} - {dr["Name"]} : {dr["Price"].ToString()}");
    }


    return Ok(products);
}
```

`CommandBehaviour.CloseConnection` ferme la `connection` lorsque le `reader` est fermé.

Sinon on doit ajouter `reader.Close()`

Si le `reader` n'est pas fermé, la `connection`  reste bloqué sur ce `reader`. 

<img src="assets/data-reader-in-product-table-eeo.png" alt="data-reader-in-product-table-eeo" style="zoom:50%;" />





## Multiple `Result Set`

- On passe plusieurs clause `SELECT`
- Le retour de deux ou plus clause `SELECT` d'une procédure stockée
- Utilisation de la méthode `NextResult`
- Évite plusieurs appelles à la `DB`



## Exemple : `reader.NextResult`

On a deux tables `Product` et `Category`

<img src="assets/product-category-err-schema-poi.png" alt="product-category-err-schema-poi" style="zoom:50%;" />

On crée un `endpoint` pour tester :

```cs
public static WebApplication MapMultipleResultSet(this WebApplication app)
{
    app.MapGet("multiple", GetMultipleResultSet);

    IResult GetMultipleResultSet(SqlConnection connection)
    {
        List<ProductCategoryDto> products = new();
        List<CategoryDto> categories = new();

        var sql = @"SELECT * FROM Product;
                        SELECT * FROM Category";

        using var cmd = new SqlCommand(sql, connection);
        connection.Open();

        using var reader = cmd.ExecuteReader(CommandBehavior.CloseConnection);

        while(reader.Read())
        {
            products.Add(new ProductCategoryDto(
                reader.GetFieldValue<int>("Id"),
                reader.GetFieldValue<string>("Name"),
                reader.GetFieldValue<decimal>("Price"),
                reader.GetFieldValue<int>("CategoryId")
            ));
        }

        reader.NextResult();

        while(reader.Read())
        {
            categories.Add(new CategoryDto(
                reader.GetFieldValue<int>("Id"),
                reader.GetFieldValue<string>("Name")
            ));
        }

        return Ok(new { products, categories });
        }

        return app;
    }
}

public record ProductCategoryDto(int Id, string Name, decimal Price, int CategoryId);
public record CategoryDto(int Id, string Name);
```

```json
{
  "products": [
    {
      "id": 1,
      "name": "Pan Updated",
      "price": 199,
      "categoryId": 0
    },
    // ...
  ],
  "categories": [
    {
      "id": 1,
      "name": "Computer"
    },
    // ...
  ]
}
```







