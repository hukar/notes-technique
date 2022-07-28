# 03 La classe `SqlCommand`

Soumettre une requête à la `DB`

```cs
sql = "INSERT INTO Product ...";
var cmd = new SqlCommand(sql, cnn);
```

Appeler la méthode appropriée

```cs
cmd.ExecuteNonQuery();
cmd.ExecuteScaler();
```



## `ExecuteScalar`

> `Scalar` = valeur atomique (un `objet` pas un `tableau` ou une `list`)

On doit utiliser un `using block` à chaque fois qu'on utilise une ressource non géré.

`SqlCommand` doit aussi avoir son propre `using block`.

```cs
public int GetProductsCountScalar()
{
    string sql = "SELECT COUNT(*) FROM Product";
    
    using var cnn = new SqlConnection(cnnString);
    using var cmd = new SqlCommand(sql, cnn);
    
    cnn.Open();
    
    return (int)cmd.ExecuteScalar(); // reourne 4 par exemple
}
```

