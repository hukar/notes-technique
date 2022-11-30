# 05 Insérer des données en utilisant des paramètres

## Injection `SQL` : `);DROP TABLE X --`

> ```cs
> var sql = $"INSERT INTO Employee ('Name') VALUES ('{name}');";
> 
> var command = new SqliteCommand(sql, connection);
> 
> connection.Open();
> 
> var rowsAffected = command.ExecuteNonQuery();
> ```
>
> Et voila l'attaque :
>
> ```cs
> var name = "');DROP TABLE Employee -- ";
> ```
>
> ```
> Unhandled exception. Microsoft.Data.Sqlite.SqliteException (0x80004005): SQLite Error 1: 'no such table: Employee'.
> ```



## Paramètre `SQL` : `SqlParameter`, `SqliteParameter`

- Les valeurs sont traitées comme des litéraux plutôt que comme des morceaux de `SQL`
- La valeur dans un paramètre est traitée comme le type spécifié.

```cs
var sql = @$"INSERT INTO Employee ('Name', 'LastName') VALUES (@name, @lastname)";

using var command = new SqliteCommand(sql, connection);

var nameParameter = new SqliteParameter("name", SqliteType.Text);
nameParameter.Value = name;
var lastNameParameter = new SqliteParameter("lastname", SqliteType.Text);
lastNameParameter.Value = lastName;

command.Parameters.Add(nameParameter);
command.Parameters.Add(lastNameParameter);

connection.Open();

var rowsAffected = command.ExecuteNonQuery();
```

