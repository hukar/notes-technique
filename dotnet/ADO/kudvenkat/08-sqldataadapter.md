# 08 `SqlDataAdapter`

`SqlDataReader` est orienté connection et une connection ouverte et active est obligatoire.

`SqlDataAdapter` et `DataSet` fournissent un modèle déconnecté à l'accès aux données.

De même que pour `SqlCommand`, il faut fournir une commande `SQL` et une connexion à `SqlDataAdapter`.

## Implémentation

```cs
string connectionString = builder.Configuration.GetConnectionString("HukarConnection");

builder.Services.AddScoped(_ => new SqlConnection(connectionString));

app.MapGet("/products/dataadapter", GetProductsDataAdapter);
```

```cs
List<ProductDto> GetProductsDataAdapter(SqlConnection con)
{
	SqlDataAdapter da = new("SELECT * FROM ProductInventoty", con);
    DataSet ds = new();
    
    da.Fill(ds);
    
    List<ProductDto> products = new();

    foreach(DataRow row in ds.Tables["Table"]!.Rows)
    {
        products.Add(new(
            Convert.ToInt32(row["ProductId"]),
            row["ProductName"].ToString()!,
            Convert.ToInt32(row["UnitPrice"]),
            0
        ));
    }

    return products;
}
```

`DataSet` est indépendant des providers (`Sql`, `Oracle`,  `Odbc`) et est contenu dans `System.Data`.

Un `DataSet` est une représentation en mémoire de la base de données, tables et relations.

La méthode `Fill` ouvre la connexion, exécute la commande, rempli le `DataSet` et ferme la connexion pour nous.

