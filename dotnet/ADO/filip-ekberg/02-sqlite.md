# 02 `Sqlite`

<img src="assets/sqlite-advantage.png" alt="sqlite-advantage" style="zoom:50%;" />



## Connexion

On ajoute le package nuget :

```
dotnet add package Microsoft.Data.Sqlite
```



## Types

On a `4` types :

- `INTEGER` : `Boolean`, `byte`,  `int` et `uint`
- `REAL` : `Single`, `Double`
- `BLOB` : `byte[]`
- `TEXT` : `char`, `string`, `guid`, toutes les types `date`



## Créer une table et insérer des données

Créer la connexion : `SqliteConnectio,`

```cs
var connectionString = "Data Source=mydb.db;";

using var cnn = new SqliteConnection(connectionString);
```

écrire le `sql` :

```cs
var sql = @"CREATE TABLE Employee (
    			Id INTEGER,
    			Name TEXT
			);
			INSERT INTO Employee 
			VALUES (1, 'Jonh'), (2, 'Remy');";
```

créer et exécuter la commande : `SqliteCommand`

```cs
using var cmd = new SqliteCommand(sql, cnn);

cnn.Open();

var numberOfRows = cmd.ExecuteNonQuery();

WriteLine(numberOfRows);
```



## Version améliorée

Pour éviter d'écrire du `sql` un peu partout, on crée une classe `static` pour centralisé les requêtes :

`CommandSql.cs`

```cs
public static class CommandSql
{
     public static string CreateTable => @"
            CREATE TABLE Employee (
                Id INTEGER PRIMARY KEY AUTOINCREMENT,
                Name TEXT
            );
            CREATE TABLE Skill (
                Id INTEGER PRIMARY KEY AUTOINCREMENT,
                Label TEXT
            );
            CREATE TABLE EmployeeSkill (
                IdEmployee INTEGER,
                IdSkill INTEGER
            );";
    
    public static string FillTable => @"
            INSERT INTO Employee ('Name') 
            VALUES ('Marc'), ('Jean'), ('Rachel');
            INSERT INTO Skill ('Label') 
            VALUES ('Programming'), ('Cooking'), ('Drawing');
            INSERT INTO EmployeeSkill 
            VALUES (0, 1), (0, 2), (1, 0), (2, 0), (2, 1), (2, 2)";

    public static string SelectEmployee => @"
            SELECT * FROM Employee";
}
```

Le code est bien plus propre sans `magic string` :

```cs
var connectionString = "Data Source=mydb.db;";

using var connection = new SqliteConnection(connectionString);

using var command = new SqliteCommand(CommandSql.FillTable, connection);

connection.Open();

var rowsAffected = 0;

try
{
    rowsAffected = command.ExecuteNonQuery();
}
catch (SqliteException ex)
{  
    WriteLine($"HUKAR: {ex.Message}");
}
```

On utilise une `exception` dédiée : `SqliteException`.



## Lecture des données : `ExecuteReader`

Le `reader` est le lecteur des données dans un scénario connecté. Il permet une empreinte légère en mémoire car il ne va chercher les données que lorsque `reader.Read` est appelée.

```cs
// ...
using var command = new SqliteCommand(CommandSql.SelectEmployee, connection);

connection.Open();

try
{
    using var reader = command.ExecuteReader();

    if(reader.HasRows)
    {
        while(reader.Read())
        {
            WriteLine($"employee {reader["name"]}");
        }
    }
    else
    {
        WriteLine("There're no data");
    }
    
}
catch (SqliteException ex) // ...
```

```
employee Marc
employee Jean
employee Rachel
```



`reader.HasRows` permet de vérifier qu'on a bien quelque chose :

```sql
SELECT * FROM Employee WHERE Name LIKE '%n'
```

```
employee Jean
```

```sql
SELECT * FROM Employee WHERE Name LIKE '%e'
```

```
There're no data
```

