# 02 `Repository` pattern

## `DapperContext`

```cs
public class DapperContext
{
    private readonly IConfiguration _configuration;
    private readonly string _connectionString;
    
    public DapperContext(IConfiguration configuration)
    {
        _configuration = configuration;
        _connectionString = _configuration.GetConnectionString("DapperConnection");
    }
    
    public IDbConnection CreateConnection() => new SqlConnection(_connectionString);
}
```

Peut-être pas nécessaire (???)

On l'enregistre comme un service :

```cs
builder.Services.AddSingleton<DapperContext>();
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