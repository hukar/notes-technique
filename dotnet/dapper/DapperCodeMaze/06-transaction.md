# 06 `Transaction` avec `Dapper`

On modifie notre `interface` :

```cs
public Task<int> CreateMultipleCompanies(List<CreateCompanyDto> companies);
```

Puis on l'implémente dans notre `repository` :

```cs
public async Task<int> CreateMultipleCompanies(List<CreateCompanyDto> companies)
{
    var sql = @"INSERT INTO Companies (Name, Address, Country) VALUES (@Name, @Address, @Country)";
    var rowsAffected = 0;

    using var connection = _dapperContext.CreateConnection();

    connection.Open();

    using var transaction = connection.BeginTransaction();

    foreach(var company in companies)
    {
        // Pour tester la transaction
        if(company.Name == "Exception") throw new Exception("Transaction broken");
        var parameters = new DynamicParameters();

        parameters.Add("Name", company.Name, DbType.String);
        parameters.Add("Address", company.Address, DbType.String);
        parameters.Add("Country", company.Country, DbType.String);

        rowsAffected += await connection.ExecuteAsync(sql, parameters, transaction: transaction);
    }

    transaction.Commit();

    return rowsAffected;
}
```

Et le `endpoint` associé :

```cs
companyRoute.MapPost("/multiple", async (ICompanyRepository db, List<CreateCompanyDto> companiesToCreate) =>  {
    var rowsAffected = await db.CreateMultipleCompanies(companiesToCreate);

    return Ok($"rows affected: {rowsAffected}");
});
```

