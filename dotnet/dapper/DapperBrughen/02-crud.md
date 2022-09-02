# 02 `Crud` avec `Dapper`



## `GetAll`

```cs
public async Task<IEnumerable<Company>> GetAll()
{
    var sql = "SELECT * FROM Companies";
    return await _db.QueryAsync<Company>(sql);
}
```

`QueryAsync<T>` renvoie un `IEnumerable<T>`.



## `Get(int id)`

```cs
public async Task<Company> Get(int id)
{
    var sql = @"SELECT * 
                FROM Companies
                WHERE CompanyId = @id";

    return await _db.QueryFirstOrDefaultAsync<Company>(sql, new { id });
}
```



## `Add`

### Avec `OUTPUT`

```cs
public Task<int> Add(Company company)
{
    var sql = @"INSERT INTO Companies (Name, Address, City, PostalCode) 
    			OUTPUT inserted.CompanyId 
    			VALUES (@Name, @Address, @City, @PostalCode)";
    
    var id = await _db.ExecuteScalarAsync(sql, company);

    return Convert.ToInt32(id);
}
```

On récupère ici l'`id` de l'élément ajouté avec `OUTPUT inserted.<NomId>`.

### Deuxième méthode avec `SELECT CAST(SCOPE_IDENTITY() as int) `

```cs
public async Task<int?> Add(Company company)
{
    var sql = @"INSERT INTO Companies (Name, Address, City, PostalCode) 
    			VALUES (@Name, @Address, @City, @PostalCode);
               SELECT CAST(SCOPE_IDENTITY() as int)";

    return await _db.QueryFirstOrDefaultAsync<int?>(sql, company);
}
```

> Solution inspirée de :
>
> https://stackoverflow.com/questions/50650985/dapper-for-net-core-insert-into-a-table-and-return-id-of-inserted-row
>
> Et aussi du `crud` de `Dapper` : `Dapper.Rainbow`
>
> https://github.com/DapperLib/Dapper/blob/main/Dapper.Rainbow/Database.Async.cs
>
> Il existe un autre projet de `crud` :
>
> https://github.com/ericdc1/Dapper.SimpleCRUD/tree/master/Dapper.SimpleCRUD



## `Update`

```cs
public async Task<int> Update(Company company)
{
    var sql = @"UPDATE Companies 
                SET Name=@Name, 
                	Address=@Address, 
                	City=@City, 
                	PostalCode=@PostalCode 
                WHERE CompanyId=@CompanyId";

    return await _db.ExecuteAsync(sql, company);
}
```

Attention après `SET` il n'y a pas de parenthèses.



## `Delete`

```cs
public async Task<int> Delete(int id)
{
    var sql = @"DELETE 
                    FROM Companies
                    WHERE CompanyId=@id";

    return await _db.ExecuteAsync(sql, new { id });
}
```





## `Endpoints` en `Minimal API`

```cs
public static class CompanyEndpoints
{
    public static WebApplication MapCompanyEndpoints(this WebApplication app)
    {
        app.MapGet("/company", GetAllCompany);
        app.MapGet("/company/{id:int}", GetCompanyById);
        app.MapPost("/company", AddCompany);
        app.MapPut("/company", UpdateCompany);
        app.MapDelete("/company/{id:int}", DeleteCompany);

        return app;
    }

    public static async Task<IResult> GetAllCompany(DapperRepository repo) 
        => Ok(await repo.GetAll());

    public static async Task<IResult> GetCompanyById(DapperRepository repo, int id) => await repo.Get(id) is Company company ? Ok(company) : NotFound();

    public static async Task<IResult> AddCompany(DapperRepository repo, Company company)
    {
        var id = await repo.Add(company);
        return Ok(id);
    }
    public static async Task<IResult> UpdateCompany(DapperRepository repo, Company company)
    {
        var nbRows = await repo.Update(company);
        return nbRows != 0 ? NoContent() : NotFound();
    }
    public static async Task<IResult> DeleteCompany(DapperRepository repo, int id)
    {
        var nbRows = await repo.Delete(id);
        return nbRows != 0 ? NoContent() : NotFound();
    }
}
```

