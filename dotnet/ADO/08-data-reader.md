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



## `Cast` des données en entrée

<img src="assets/cast-type-returned-pcj.png" alt="cast-type-returned-pcj" style="zoom:50%;" />



## Faire des conversions

```cs
List<Product> products = new();

while(dr.Read())
{
    var product = new {
        Id = dr.GetInt32(dr.GetOrdinal("Id"),
        Name = dr.GetString(dr.GetOrdinal("Name")),
     	Price = dr.GetDecimal(dr.GetOrdinal("Price")),
        RetireDate = dr.IsDBNull(dr.GetOrdinal("RetireDate")) ?
                     (DateTime?)null :
                     Convert.ToDateTime(dr["RetireDate"])
    };
                         
   	products.Add(product);
}
```

`GetOrdinal` retrouve l'index de la colonne en lui passant son nom (de colonne).

```cs
Column1 = reader.GetValue(reader.GetOrdinal("Column1Name")).ToString();
```

On peut ainsi mettre en cache la valeur de `GetOrdinal` :

> https://stackoverflow.com/questions/1079366/why-use-the-getordinal-method-of-the-sqldatareader
>
> I think that the reason to use GetOrdinal() is so that you can cache the result and re-use it multiple times for performance.

```cs
int valueOrdinal = reader.GetOrdinal("value");
while ( ... )
{
    var value = reader.GetString(valueOrdinal);
}
```

On évite d'exécuter GetOrdinal à chaque passage de boucle.

Sinon il existe `GetValue(2)` qui correspond à la colonne `3`.

C'est plus rapide et moins lisible, cela utilise uniquement l'index de l'emplacement d'une colonne.

```cs
Column1 = reader.GetValue(0).ToString().Trim();
```

