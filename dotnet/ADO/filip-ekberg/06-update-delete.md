# 06 `UPDATE` et `DELETE`



## `UPDATE`

```cs
var sql = @"UPDATE Employee 
            SET Name = @name
            WHERE Id = @employeeId";

using var command = new SqliteCommand(sql, connection);

var nameParameter = new SqliteParameter("name", SqliteType.Text);
nameParameter.Value = newName;
command.Parameters.Add(nameParameter);

var idParameter = new SqliteParameter("employeeId", SqliteType.Text);
idParameter.Value = employeeId;
command.Parameters.Add(idParameter);

connection.Open();

var rowsAffected = command.ExecuteNonQuery();
```

Utilisation des `Parameters` avec `new SqliteParameter` et `command.Parameters.Add`.



## `DELETE`

```cs
var sql = @"DELETE FROM Employee 
            WHERE Id = @employeeId";

using var command = new SqliteCommand(sql, connection);

var idParameter = new SqliteParameter("employeeId", SqliteType.Text);
idParameter.Value = employeeId;
command.Parameters.Add(idParameter);

connection.Open();

var rowsAffected = command.ExecuteNonQuery();
```

