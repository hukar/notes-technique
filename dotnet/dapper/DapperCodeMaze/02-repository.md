# 02 `Repository` pattern

## `DapperContext`

```cs
public class DapperContext
{
    private readonly string _connectionString;
    
    public DapperContext(IConfiguration configuration)
    {
        _connectionString = configuration.GetConnectionString("DapperConnection");
    }
    
    public IDbConnection CreateConnection() => new SqlConnection(_connectionString);
}
```

> Cela permet d'avoir une nouvelle instance de `IDbConnection` à chaque fois.

On l'enregistre comme un service :

```cs
builder.Services.AddSingleton<DapperContext>();
```



## Model

`Employee`

```cs
public class Employee
{
    public int Id { get; set; }
    public string? Name { get; set; }
    public int Age { get; set; }
    public string? Position { get; set; }
    public int CompanyId { get; set; }
}
```

`Company`

```cs
public class Company
{
    public int Id { get; set; }
    public string? Name { get; set; }
    public string? Address { get; set; }
    public string? Country { get; set; }
    public List<Employee> Employees { get; set; } = new();
}
```





## `Contracts` et `Repository`

`Contracts/ICompanyRepository`

```cs
namespace Scratchpad.Contracts;

public interface ICompanyRepository
{
    public Task<IEnumerable<Company>> GetCompanies();
}
```



`Repository/CompanyRepository`

```cs
public class CompanyRepository : ICompanyRepository
{
    private readonly DapperContext _dapperContext;
    public CompanyRepository(DapperContext dapperContext)
    {
        _dapperContext = dapperContext;
    }
    
    public async Task<IEnumerable<Company>> GetCompanies()
    {
        var sql = "SELECT * FROM Companies";
        
        using var connection = _context.CreateConnection();
        var companies = await connection.QueryAsync<Compagny>();
        
        return companies.ToList();
    }
    
    // ...
}
```



On injecte le `repository` :

```cs
builder.Services.AddScoped<ICompanyRepository, CompanyRepository>();
```