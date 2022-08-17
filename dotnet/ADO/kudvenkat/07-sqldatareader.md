# 07 `SqlDataReader`

## Les avantages de `SqlDataReader`

`SqlDataReader` lit les données de la manière la plus efficace possible.

`SqlDataReader` est `read-only` et `forward-only` (`seulement en avant`), c'est à dire que si le `reader` est sur le deuxième enregistrement, il ne peut pas revenir en arrière sur le premier.

C'est la nature `forward-only` de `SqlDataReader` qui le rend efficace.

`SqlDataReader` est orienté connexion, il requière une connexion active à la source de données pour les lire.

Une instance de `SqlDataReader` ne peut pas être créée avec l'opérateur `new`.

La méthode `ExecuteReader` d'un objet de type `SqlCommand` crée et retourne une instance de `SqlDataReader`.



## Mise en place

```c#
string connectionString = builder.Configuration.GetConnectionString("HukarString");

builder.Services.AddScoped(_ => new SqlConnection(connectionString));

app.MapGet("/products", GetProduct);
```

```c#
List<ProductDto> GetProduct(SqlConnection con)
{
    SqlCommand cmd = new SqlCommand("SELECT * FROM ProductInventory", con);
    con.Open();

    using SqlDataReader reader = cmd.ExecuteReader(); 
    
    while(reader.Read())
    {
        // do something ...
    }
}
```

`con.Open()` doit obligatoirement être appelée avant l'appelle à `ExecuteReader`.

`SqlDataReader` doit être fermé (`Close`) si on veut que l'application soit scalable.

On peut utiliser un `using block` ou dans une clause `finally`.

La méthode `Read` d'un objet `SqlDataReader` est utilisée pour boucler sur les données.