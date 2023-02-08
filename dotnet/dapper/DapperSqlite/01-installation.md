# 01 installer `Dapper` et `Sqlite` ensemble



## Installer les dépendances

```bash
dotnet add package Microsoft.Data.Sqlite

dotnet add package dapper
```





## Créer une `DB` dans un fichier

Il suffit de créer une connexion et de l'ouvrir pour créer un fichier `DB`.

### `Connection String` : `"Data Source=DbName.db"`

```cs
using var conn = new SqliteConnection("Data Source=robot.db");

conn.Open();
```

<img src="assets/db-in%20-file-sqlite.png" alt="db-in -file-sqlite" style="zoom:50%;" />

> ### `In Memory`
>
> Avec `Sqlite` on peut aussi créer la `DB` juste en mémoire plutôt que dans un fichier :
>
> ```cs
> Data Source=:memory:
> ```
>
> Partagée entre plusieurs connexion :
>
> ```cs
> Data Source=InMemorySample;Mode=Memory;Cache=Shared
> ```
>
> La `DB` persiste tant qu'il reste au moins une connexion ouverte.



## Créer un `DapperContext` pour `Sqlite`

```cs
public class DapperContext
{
    private readonly string _connectionString = "Data Source=RobotDb.db";

	public IDbConnection CreateConnection() => new SqliteConnection(_connectionString);
}
```

Si on utilise `appsettings.json` pour conserver son `connection string`:

`appsettings.json`

```json
// ...  
"ConnectionStrings": {
    "ConnectionSqlite": "Data Source=RobotApp.db"
  }
}
```

`DapperContext.cs`

```cs
public class DapperContext
{
    private readonly string _connectionString;
    
    public DapperContext(IConfiguration configuration)
    {
        _connectionString = configuration!.GetConnectionString("connectionSqlite")!;
    }
    
    public IDbConnection CreateConnection() => new SqliteConnection(_connectionString);
}
```

