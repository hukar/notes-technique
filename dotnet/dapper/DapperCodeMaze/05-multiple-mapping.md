# 05 `Multiple Query` et `Mapping`

## `GetMultipleResults`

On ajoute cette méthode à l'interface :

```cs
public interface ICompanyRepository
{
    // ...
    public Task<Company?> GetMultipleResults(int id);
```

Puis on l'implémente :

```cs
public async Task<Company?> GetMultipleResults(int id)
{
    var sql = @"SELECT * FROM Companies WHERE Id = @id;
                    SELECT * FROM Employees WHERE CompanyId = @id";

    using var connection = _context.CreateConnection();
    using var multi = await connection.QueryMultipleAsync(sql, new { id });

    var company = await multi.ReadFirstOrDefaultAsync<Company>();

    if(company is not null)
    {
        company.Employees = (await multi.ReadAsync<Employee>()).ToList();
    }

    return company;
}
```



### Le `endpoint`

```cs
routeCompany.MapGet("/{id:int}/employees", 
   async (ICompanyRepository db, int id) 
       => await db.GetMultipleResults(id) is Company company ?
          Ok(company) : 
          NotFound()
);
```

```http
GET {{root}}/companies/1/employees HTTP/1.1
```

```json
{
  "id": 1,
  "name": "IT_Solutions Ltd",
  "address": "583 Wall Dr. Gwynn Oak, MD 21207",
  "country": "USA",
  "employees": [
    {
      "id": 1,
      "name": "Sam Raiden",
      "age": 26,
      "position": "Software developer",
      "companyId": 1
    },
    {
      "id": 3,
      "name": "Jana McLeaf",
      "age": 30,
      "position": "Software developer",
      "companyId": 1
    }
  ]
}
```



## Avec un `JOIN` et `QueryAsync`

```cs
public interface ICompanyRepository
{
    // ...
    public Task<List<Company>> MultipleMapping();
```

```cs
public async Task<List<Company>> MultipleMapping()
{
	var sql = @"SELECT * FROM Companies c 
				JOIN Employees e
				ON c.Id = e.CompanyId";
    
    using var connection = _context.CreateConnection();
    var companyDict = new Dictionary<int, Company>();
    
    var companies = await connection.QueryAsync<Company, Employee, Company>( sql, (company, employee) => {
        if (!companyDict.TryGetValue(company.Id, out var currentCompany))
        {
            currentCompany = company;
            companyDict.Add(currentCompany.Id, currentCompany);
        }
        
        currentCompany.Employees.Add(employee);
        
        return currentCompany;
    });
    
    return companies.Distinct().ToList();
}
```

Le `Distinct` car on renvoie `companies` et pas `companyDict`.

On peut aussi renvoyer ceci :

```cs
return companyDict.Values.ToList();
```

```json
[
  {
    "id": 1,
    "name": "IT_Solutions Ltd",
    "address": "583 Wall Dr. Gwynn Oak, MD 21207",
    "country": "USA",
    "employees": [
      {
        "id": 1,
        "name": "Sam Raiden",
        "age": 26,
        "position": "Software developer",
        "companyId": 1
      },
      {
        "id": 3,
        "name": "Jana McLeaf",
        "age": 30,
        "position": "Software developer",
        "companyId": 1
      }
    ]
  },
  {
    "id": 2,
    "name": "Admin_Solutions Ltd",
    "address": "312 Forest Avenue, BF 923",
    "country": "USA",
    "employees": [
      {
        "id": 2,
        "name": "Kane Miller",
        "age": 35,
        "position": "Administrator",
        "companyId": 2
      }
    ]
  }
]
```

