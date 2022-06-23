# 01 Installation de `Dapper`

## Installer le `nuget`

```cs
dotnet add package Dapper --version 2.0.123
```

## Créer le modèle

`Company.cs`

```cs
public class Company
{
    public int CompanyId { get; set; }
    public string Name { get; set; }
    public string Address { get; set; }
    public string City { get; set; }
    public string PostalCode { get; set; }
}
```



## Créer le repository `DapperRepository`
### Connection de la `DB`

On injecte le service `IConfiguration`.

Ne pas oublier `using Dapper` pour la complétion des méthodes d'extensions.

> `Dapper` fournit des méthodes d'extension à `DbConnection`.

```cs
using System.Data;  // IDbConnection
using Dapper;
using Microsoft.Data.SqlClient;

namespace DapperWebMinimal.Data;

public class DapperRepository : IAsyncRepository
{
	private IDbConnection db;
    public DapperRepository(IConfiguration configuration)
    {
        db = new SqlConnection(
            configuration.GetConnectionString("HukarConnection")
        );  
    }
```

`SqlConnection` est déjà présent dans le package `Microsoft.Data.SqlClient`

> `Microsoft.Data.SqlClient` est fournit en dépendance de `Microsoft.EntityFrameworkCore.SqlServer` :
>
> <img src="assets/dependency-enigme-data-sqlclient-ecw.png" alt="dependency-enigme-data-sqlclient-ecw" style="zoom:50%;" />
>
> Si on utilise `Dapper` seul, il faut ajouter ce `nuget` soi-même :
>
> ```bash
> dotnet add package Microsoft.Data.SqlClient
> ```
>
> 



