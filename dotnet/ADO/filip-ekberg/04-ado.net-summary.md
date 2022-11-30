# 04 Résumé `ADO.Net`

Pour `Sql Server` et `Sqlite`



## Provider

```cs
using Microsoft.Data.SqlClient;

using Microsoft.Data.Sqlite;
```



## `Connection`

```cs
using var connection = new SqlConnection(connectionString);

using var connection = new SqliteConnection(connectionString);
```



## Créer une `Command`

```cs
using var command = connection.CreateCommand();
```

Va retourner le type correcte :

`SqlCommand` ou `SqliteCommand`.

On doit ensuite ajouter le `sql` :

```cs
command.CommandText = sqlQuery;
```

On peut aussi utiliser les différentes classes directement

```cs
using var command = new SqlCommand(sqlQuery, connection);

using var command = new SqliteCommand(sqlQuery, connection);
```



## Exécution de la `command`

```cs
// On ouvre la connexion
connection.Open() // existe aussi une version Async
    
using var reader = command.ExecuteReader(); 
```

`ExecuteScalar` retourne la première colonne de la première ligne du résultat.

`ExecuteNonQuery` retourne le nombre de ligne(s) affectée(s).

On a un `reader` associé au provider : `SqlDataReader` ou `SqliteDataReader`.

On demande au `reader` s'il a récupéré des enregistrements :

```cs
if(reader.HasRows == false)
{
    return;
}

while(reader.Read())
{
    
}
```



## Récupérer les données

Le `reader` possède plusieurs méthode ds le style `GetType` :

<img src="assets/get-something-freader-can.png" alt="get-something-freader-can" style="zoom:50%;" />

Pour les utiliser, il faut connaitre l'index de la donnée dans la base (le numéro de la colonne).

```cs
var OrderId = reader.GetGuid(0);
```

On peut aussi les récupérer avec leur nom de colonne :

```cs
var OrderId = reader["Id"];
```

On peut aussi créer une méthode d'extension pour simplifier :

```cs
public static class SqliteDataReaderExtension
{
    public static T? GetByType<T>(this SqliteDataReader reader, string fieldName)
    {
        if(reader[fieldName].Equals(DBNull.Value))
        {
            return default(T);  
        }

        return (T)reader[fieldName];
    }
}
```

```cs
while(reader.Read())
{
    employees.Add(
        new Employee(
            reader.GetByType<Int64>("Id"),
            reader.GetByType<string>("Name")!,
            reader.GetByType<string?>("LastName")
        )
    );
}
```

> C'est `Int64` qui matche avec le type `INTEGER` de `Sqlite`. `int` étant un alias de `Int32`.

#### ! Il faut utiliser `DBNull.Value` et pas `null`, ce n'est pas la même chose

```cs
WriteLine($"null == DbNull.Value : {null == DBNull.Value}");
```

```
null == DbNull.Value : False
```



## Version sur `stackoverflow`

https://stackoverflow.com/questions/2609875/null-safe-way-to-get-values-from-an-idatareader

> J'ai cependant des problèmes de conversion avec `Guid` et `Sqlite`.
>
> ```
> Unhandled exception. System.InvalidCastException: Unable to cast object of type 'System.String' to type 'System.Guid'.
> ```

```cs
public static class NullSafeGetter
{
   public static T GetValueOrDefault<T>(this IDataRecord row, string fieldName)
   {
       int ordinal = row.GetOrdinal(fieldName);
       return row.GetValueOrDefault<T>(ordinal);
   }

   public static T GetValueOrDefault<T>(this IDataRecord row, int ordinal)
   {
       return (T)(row.IsDBNull(ordinal) ? default(T) : row.GetValue(ordinal));
   }
}
```

