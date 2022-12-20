# 04 `Dapper` et les `Stored Procedure`



## La `Stored Procedure`

```sql
CREATE PROCEDURE ShowCompanyByEmployeeId
	@id int
AS
	SELECT c.Id, c.Name, c.Address, c.Country
	FROM Companies c JOIN Employees e ON c.Id = e.CompanyID
	WHERE e.Id = @Id
```



## L'interface `ICompanyRepository`

```cs
public interface ICompanyRepository
{
    // ...
    public Task<Company> GetCompanyByEmployeeId(int id);
}
```



## Le `repository` : `CompanyRepository`

```cs
public async Task<Company> GetCompanyByEmployeeId(int id)
{
    var procedureName = "ShowCompanyByEmployeeId";
    var parameters = new DynamicParameters();
    parameters.Add("Id", id, DbType.Int32, ParameterDirection.Input);
    
    using var connection = _dapperContext.CreateConnection();
    var company = await connection.QuerySingleOrDefaultAsync<Company>(
    	procedureName, parameters, commandType: CommandType.StoredProcedure
    );
    
    return company;
}
```

`ParameterDirection.Input` n'est pas nécessaire.

Par contre préciser `commandType: CommandType.StoredProcedure` est obligatoire.

## Le `Endpoint`

```cs
companyRoute.MapGet("/employee/{id:int}", 
  async (ICompanyRepository db, int id) 
    => await db.GetCompanyByEmployeeId(id) is Company company ? Ok(company) : NotFound());
```

