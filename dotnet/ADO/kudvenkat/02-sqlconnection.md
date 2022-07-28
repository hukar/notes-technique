# 02 `SqlConnection`

## Résumé

`ADO.NET` est un ensemble de classe permettant la connexion à une source de données.

Plusieurs `provider` sont fournis pour différentes sources de données.

Les classes les plus importantes sont :

- `Connection` : `SqlConnection`
- `Command` : `SqlCommand`
- `DataReader` : `SqlDataReader`
- `DataAdapter` : `SqlDataAdapter`
- `DataSet` qui ne fait pas partie de la librairie d'un `provider` mais de `System.Data`

Les classes sont préfixées par le provider (ici `sql` pour `SQL Server`).

`DataSet` ne dépendant pas d'un provider n'est pas préfixé.



## `SqlConnection`

### La `connection string`

On a besoin de renseigner ces informations :

- Le `Server` et son numéro de port
- La `Database`
- Le `User`
- Le `Password`

```c#
var connectionString = "Server=localhost,1433;Database=EloaBdd;User=sa;Password=huk@r2Xmen99";
```

Dernièrement par défaut le réglage `Encrypt=true` et cela créé une erreur.

```
Unhandled exception. Microsoft.Data.SqlClient.SqlException (0x80131904): A connection was successfully established with the server, but then an error occurred during the pre-login handshake. (provider: TCP Provider, error: 35 - An internal exception was caught)
 ---> System.Security.Authentication.AuthenticationException: The remote certificate was rejected by the provided RemoteCertificateValidationCallback.
```

Il faut donc ajouter `Encrypt=false` :

```c#
var connectionString = "Server=localhost,1433;Database=EloaBdd;User=sa;Password=huk@r2Xmen99;Encryot=false";
```

> C'est un point à comprendre !



### Deux manière de passer la `connection string`

```c#
var con = new SqlConnection(connectionString);

// OU

var con = new SqlConnection();
con.ConnectionString = connectionString;
```



### Créer une `Command`

On passe la `Connection` au constructeur de `Command`

```c#
SqlCommand cmd = new SqlCommand("SELECT * FROM Client", con);
```

Ici la `Connection` sait sur quel `serveur` et quelle `base de données`exécuter la `commande SQL`.

On doit ensuite ouvrir la `Connection` :

```c#
con.Open();
```

Et on exécute la `commande` :

```c#
SqlDataReader reader = cmd.ExecuteReader();
```

On doit fermer la `Connection` dès que possible :

```c#
con.Close();
```

> Si on ferme pas les `connexions`, les performances et la mise à l'échelle (`scalability`) peuvent être drastiquement impactées.

#### ! On doit ouvrir la `connection` le plus tard possible et fermer la `connection` le plus tôt possible.

Ouvrir la `connection` n'est nécessaire que lorsqu'on veut exécuter une `command`.

```c#
con.Open();
var reader = cmd.ExecuteReader();
```



### Gérer les `Exception`

On doit obligatoirement fermer la `connection`, même si une `exception` est lancée à un moment donné.

On utilise pour cela un bloc `try catch finally`, `finally` garantissant que la `connection` sera fermée quoi qu'il arrive.

```c#
var con = new SqlConnection(connectionString);
try
{
    var cmd = new SqlCommand("SELECT * FROM Client", con);
    con.Open();
    var reader = cmd.ExecuteReader();
    // get data with reader.Read()
}
catch
{
    
}
finally
{
    con.Close();
}
```

à la place on peut utiliser `using` :

```c#
using(var con = new SqlConnection(connectionString))
{
    var cmd = new SqlCommand("SELECT * FROM Client", con);
    con.Open();
    var reader = cmd.ExecuteReader();
    // Consume data ...
}
```

Ou encore :

```cs
using var con = new SqlConnection(connectionString);
var cmd = new SqlCommand("SELECT * FROM Client", con);
con.Open();
var reader = cmd.ExecuteReader();
```

