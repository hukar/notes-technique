# 02 Créer une `DB`



## la classe `DatabaseBootstrap`

```cs
public class DatabaseBootstrap
{
 	public void Setup()
    {
        using var connection = new SqliteConnection("Data Source=Robot.db");
        
        var testIfTableExists = @"SELECT name
        							FROM sqlite_master
        							WHERE type='table' AND name='Robot'";
        
        var table = connection.Query<string>(testIfTableExists);
        
        connection.Execute(@"Create Table Robot (
        	CodeName VARCHAR(100) NOT NULL
        )")
    }
}
```

