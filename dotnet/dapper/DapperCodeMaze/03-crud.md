# 03 Un `crud` avec `Dapper`





## Premier `Endpoint` : `GetCompanies`

Méthode du `CompanyRepository` :

```cs
public async Task<IEnumerable<Company>> GetCompanies()
{
    var sql = "SELECT * FROM Companies";

    using var connection = _context.CreateConnection();
    var companies = await connection.QueryAsync<Compagny>();

    return companies.ToList();
}
```

Utilisation des méthodes d'extension pour organiser les `Endpoints` :

```cs
public static class CompanyEndpointExtension
{
    public static WebApplication MapCompagny(this WebApplication app)
    {
        var companyRoute = app.MapGroup("/companies");

        companyRoute.MapGet("/", async (ICompanyRepository db) => await db.GetCompanies());

        return app;
    }
}
```

La partie importante :

```cs
async (ICompanyRepository db) => await db.GetCompanies()
```

#### ! c'est bien `ICompanyRepository` et non pas `CompanyRepository` pour que l'injection de dépendance fonctionne.



## Nom de Colonne et nom de propriété différents

On modifie le nom de la propriété de l'entité `Company` :

```cs
public class Company
{
    public int Id { get; set; }
    // public string? Name { get; set; }
    public string? CompanyName { get; set; }
```

<img src="assets/crash-with-different-name-column-property.png" alt="crash-with-different-name-column-property" style="zoom:50%;" />

Boum, ça crash.

On peut arranger cela en modifiant le `sql` du `repository` :

```cs
var sql = @"SELECT Id, Name as CompanyName, Address, Country 
			FROM Companies";
```

`Name as CompanyName`.



## `GetCompanyById` : utilisation des `query parameters`

```cs
// Dans le CompanyRepository

public async Task<Company> GetCompanyById(int id)
{
    var sql = "SELECT * FROM Companies WHERE Id = @Id";
    using var connection = _context.CreateConnection();
    
    var company = await connection.QuerySingleOrDefaultAsync<Company>(sql, new { id });
    
    return company;
}
```

On ajoute le `endpoint` :

```cs
companyRoute.MapGet("/{id:int}", async (ICompanyRepository db, int id) => await db.GetCompanyById(id) is Company company ? Ok(company) : NotFound());
```

