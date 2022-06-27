# BB `Dependency Injection`

## Test avec `Dependency Injection`

Si on transforme `SQlConnection` en service grâce à `builder.Services.AddScoped`, la connection est `Dispose` pour nous :

`Program.cs`

```cs
string connString = builder.Configuration.GetConnectionString("HukarConnect");
builder.Services.AddScoped(_ => new SqlConnection(connString));
```

On remarque la syntaxe spéciale `_ => new SqlConnection`. On peut ainsi passer le `connection string`.

Puis plus loin :

```cs
app.MapGet("/connect", (SqlConnection connection) => {
    connection.Open();
    return Ok(connection.ClientConnectionId);
});
```

Malfré le `pool` restreint (à `3` connections), je n'ai plus d'erreur en solicitant plusieurs fois mon `EndPoint`.

Le `Conteneur de dépendances` utilise lui-même un `block using`.

> ## Inspiré du `github` de Damian Edwards :
>
> https://github.com/DamianEdwards/MinimalApiPlayground/blob/main/src/Todo.Dapper/Program.cs
>
> Exemple utilisant `Dapper`
>
> ```cs
> using Dapper;
> // ...
> 
> var connectionString = builder.Configuration.GetConnectionString("TodoDb") ?? "Data Source=todos.db;Cache=Shared";
> builder.Services.AddScoped(_ => new SqliteConnection(connectionString));
> // ...
> 
> app.MapGet("/todos", async (SqliteConnection db) =>
>  await db.QueryAsync<Todo>("SELECT * FROM Todos"))
> .WithName("GetAllTodos");
> ```
>
> 



## Test de `Dispose`

Création d'une classe envellopant `SqlConnection` :

`HuConnection`

```cs
namespace AdoTestApi;

public class HuConnection : IDisposable
{
    private string ConnString { get; }
    public SqlConnection Connection { get; }

    public HuConnection(IConfiguration configuration)
    {
        ConnString = configuration.GetConnectionString("HukarConnect");
        Connection = new SqlConnection(ConnString);
    }

    public void Close()
    {
        Console.WriteLine("Close is called");
    }

    public void Dispose()
    {
        Console.WriteLine("HuConnection is disposing ...");
        Connection.Dispose();
        Console.WriteLine("... HuConnection is disposed.");
    }
}
```

J'ajoute une méthode `Close` pour voire si celle-ci est appelée par la `Dependency Injection`.

Enregistrement du service dans `Program.cs`

```cs
builder.Services.AddScoped<HuConnection>();
```

Test dans un `endpoint` :

```cs
public static IResult DiConnection(HuConnection connection)
{
    List<Product> products = new();


    var sql = "SELECT * FROM Product";

    var cmd = new SqlCommand(sql, connection.Connection);
    connection.Connection.Open();

    var reader = cmd.ExecuteReader();

    var indexId = reader.GetOrdinal("Id");
    var indexName = reader.GetOrdinal("Name");
    var indexPrice = reader.GetOrdinal("Price");
    var indexIntroductionDate = reader.GetOrdinal("IntroductionDate");
    var indexRetireDate = reader.GetOrdinal("RetireDate");

    while(reader.Read())
    {
        products.Add(new Product {
            Id = reader.GetInt32(indexId),
            Name = reader.GetString(indexName),
            Price = reader.IsDBNull(indexPrice) ? null : reader.GetDecimal(indexPrice),
            IntroductionDate = reader.IsDBNull(indexIntroductionDate) ? null : reader.GetDateTime(indexIntroductionDate),
            RetireDate = reader.IsDBNull(indexRetireDate) ? null : reader.GetDateTime(indexRetireDate),
        });
    }

    return Ok(products);
}
```

Tout fonctionne correctement sans utilisation de `using` et sans appelle direct de `Dipose`.

On voit dans la console :

```
info: Microsoft.Hosting.Lifetime[0]
      Content root path: /Users/hukar/Documents/programmation/ADO/AdoTestApi/
HuConnection is disposing ...
... HuConnection is disposed.

```

Ce qui prouve que le conteneur de service appelle bien `Dispose` mais pas `Close`.