# AA Utiliser `SQLite` 

## Créer une application console

```bash
🦄 dotnet dotnet new console -o SqliteAppcode

🦄 dotnet code /SqliteApp -r
```

`-r` reuse the same window

## Installer le package nécéssaire

```bash
dotnet add package Microsoft.EntityFrameworkCore.Sqlite --version 3.1.9
```

`SqliteApp.csproj`

```csharp
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>netcoreapp3.1</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.EntityFrameworkCore.Sqlite" Version="3.1.9" />
  </ItemGroup>

</Project>
```

## Code

```cs
using System;
using Microsoft.Data.Sqlite;

namespace sqlite_app
{
    class Program
    {
        static void Main(string[] args)
        {
            // Create connection
            var connectionStringBuilder = new SqliteConnectionStringBuilder();

            // crée la db si elle n'existe pas sinon l'utilise
            connectionStringBuilder.DataSource = "./Hukar.db";

            // using ferme la connexion pour nous
            // connectionStringBuilder.ConnectionString => "Data Source=./Hukar.db"
            using (var connection = new SqliteConnection(connectionStringBuilder.ConnectionString))
            {
                // Create table (drop if already exist)
                connection.Open();

                var delTableCmd = connection.CreateCommand();
                delTableCmd.CommandText = "DROP TABLE IF EXISTS favourite_beers";

                delTableCmd.ExecuteNonQuery();

                var tableCmd = connection.CreateCommand();
                tableCmd.CommandText = "CREATE TABLE favourite_beers (name VARCHAR(50))";

                tableCmd.ExecuteNonQuery();

                // Insert some records
                using (var transaction = connection.BeginTransaction())
                {
                    var insertCmd = connection.CreateCommand();
                    insertCmd.CommandText = "INSERT INTO favourite_beers VALUES('Chimay bleue')";
                    insertCmd.ExecuteNonQuery();

                    insertCmd.CommandText = "INSERT INTO favourite_beers VALUES('Belgoo')";
                    insertCmd.ExecuteNonQuery();

                    insertCmd.CommandText = "INSERT INTO favourite_beers VALUES('Bière des Fagnes')";
                    insertCmd.ExecuteNonQuery();

                    transaction.Commit();
                }

                // // Read records
                var selectCmd = connection.CreateCommand();
                selectCmd.CommandText = "SELECT * FROM favourite_beers";
                using (var reader = selectCmd.ExecuteReader())
                {
                    while (reader.Read())
                    {
                        var result = reader.GetString(0);
                        Console.WriteLine(result);
                    }
                }

            }
        }
    }
}
```

