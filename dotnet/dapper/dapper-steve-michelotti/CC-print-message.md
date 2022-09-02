# CC Récupération des messages envoyés avec `PRINT`

En `TSQL` on peut ajouter des messages aux données grâce à la commande `PRINT` :

```sql
SELECT * FROM Contact
PRINT 'Super ces données !!'
```

Pour les récupérer avec son code `C#` on utilise le gestionnaire d'évènement `InfoMessage`.

Ce code est valable aussi bien pour `Dapper` que pour `ADO`.

```c#
using Dapper;
using Microsoft.Data.SqlClient;

var connectionString = "Server=localhost,1433;Database=EloaBdd;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true;Encrypt=false";

using var connection = new SqlConnection(connectionString);

connection.InfoMessage += ReceiveMessage;

dynamic responses = connection.Query<dynamic>("SELECT * FROM Contact PRINT 'Hello Info Hukar'");

foreach(var resp in responses)
{
    // Console.WriteLine(resp.Nom);
}

void ReceiveMessage(Object sender, SqlInfoMessageEventArgs evt)
{
    Console.WriteLine(evt.Message);
}
```

<img src="assets/print-event-message-receive-ggq.png" alt="print-event-message-receive-ggq" style="zoom:67%;" />