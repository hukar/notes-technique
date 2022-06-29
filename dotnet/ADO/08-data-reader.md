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











