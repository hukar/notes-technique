# 05 `Multiple Query` et `Mapping`

## `QueryMultipleAsync` et deux `SELECT`

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



## Avec un `JOIN` , `QueryAsync` et un `Dictionary`

### `conn.QueryAsync<T1, T2, T3, ..., TReturn>(sql, Func<T1, T2, T3, ..., TReturn>)`

```cs
connection.QueryAsync<Company, Employee, Company>
```

Ici on récupère donc en argument de la `Func lambda` une `Company`, un `Employee` et on retourne une `Company`.

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

Et simplifier la méthode :

```cs
public async Task<List<Company>> MultipleMappingDictionary()
{
    var sql = @"SELECT * FROM Companies c
                    LEFT JOIN Employees e
                    ON c.Id = e.companyId";

    var companiesDictionary = new Dictionary<int, Company>();

    using var connection = _context.CreateConnection();

    var companies = 
        await connection.QueryAsync<Company, Employee, Company>(
        sql, (company, employee) => 
        {
            if(companiesDictionary.ContainsKey(company.Id) == false) 
                companiesDictionary.Add(company.Id, company);

            if(employee is not null) 
                companiesDictionary[company.Id].Employees.Add(employee);

            return company;
        });

    return companiesDictionary.Values.ToList();
}
```

De plus cette méthode utilise un `LEFT JOIN` et récupère aussi les `companies` sans `employees` :

```json
[
  {
    "id": 1,
    "name": "IT_Solutions Ltd",
    "employees": [ { "name": "Sam Raiden" }, { "name": "Jana McLeaf" } ]
  },
  {
    "id": 2,
    "name": "Admin_Solutions Ltd",
    "employees": [ { "name": "Kane Miller" } ]
  },
  {
    "id": 3,
    "name": "Hukar & Co Lc",
    "employees": []
  }
]
```



## Version sans `Dictionary`

Cette version gère les valeur `null` et fonctionne avec un `LEFT JOIN`

```cs
var sql = @"SELECT * 
			FROM Companies c
            LEFT JOIN Employees e 
            ON e.CompanyId = c.Id";

using var connection = _dapperContext.CreateConnection();
```
Utilisation du `LEFT JOIN` pour avoir aussi les `companies` sans `employees`.
```cs
var companies = await connection.QueryAsync<Company, Employee, Company>(sql, (company, employee) => {

    if(employee is not null) company.Employees.Add(employee);

    return company;
});
```

<img src="assets/company-with-employee-join.png" alt="company-with-employee-join" style="zoom:50%;" />

Companies contient une collection d'objet `company` où chaque `company` contient un ou zéro `employee`.

```cs
var companiesGroup = companies.GroupBy(company => company.Id);
```
Pour fusionner les `company` identique (même `Id`), on utilise un `GroupBY`.
Le `GroupBy`créé une collection de tableau qu'on va projeter avec `SELECT`.

```cs
var companiesOutput = companiesGroup.Select(cg => {
    var company = cg.First();
    var employees = cg.Where(c => c.Employees.Count >= 1)
                      .Select(c =>  c.Employees.First());
    
    // company.Employees.AddRange(employees); Provoqie des doublons
    company.Employees = employees;

    return company;
});
```
La projection renvoie la première `company` (on la veut en un seul exemplaire) et lui ajoute la liste des `employees`.
```cs
return companiesOutput;
```

### Résultat

```json
[
  {
    "id": 1,
    "name": "IT_Solutions Ltd",
    "employees": [ { "name": "Sam Raiden" }, { "name": "Jana McLeaf" } ]
  },
  {
    "id": 2,
    "name": "Admin_Solutions Ltd",
    "employees": [ { "name": "Kane Miller" } ]
  },
  {
    "id": 3,
    "name": "Hukar & Co Lc",
    "employees": []
  }
]
```

Il n'y a pas de doublons et la dernière `company` a bien une liste vide d'`employee`.



## Comparaison de performance

```
MultipleMappingWithoutDictionary : 00:00:03.2630628
MultipleMappingDictionary : 	   00:00:02.8894060

MultipleMappingWithoutDictionary : 00:00:03.0548791
MultipleMappingDictionary : 	   00:00:02.8252030

MultipleMappingWithoutDictionary :  00:00:02.8749393
MultipleMappingDictionary : 		00:00:02.8443562

MultipleMappingWithoutDictionary : 00:00:02.9407072
MultipleMappingDictionary : 	   00:00:03.1429534

MultipleMappingWithoutDictionary : 00:00:03.0395199
MultipleMappingDictionary : 	   00:00:02.9371880
```

La méthode avec `Dictionary` semble un tout petit peu plus efficace.

### Code de test

```cs
routeCompany.MapGet("/testperformances", async (ICompanyRepository db) => {

    int nbOfLoops = 3000;

    Stopwatch stopwatch = new();

    stopwatch.Start();
    for(int i = 0; i < nbOfLoops; i++)
    {
        await db.MultipleMappingWithoutDictionary();
    }
    stopwatch.Stop();

    Console.WriteLine($"MultipleMappingWithoutDictionary : {stopwatch.Elapsed}");

    stopwatch.Restart();
    for(int i = 0; i < nbOfLoops; i++)
    {
        await db.MultipleMappingDictionary();
    }
    stopwatch.Stop();

    Console.WriteLine($"MultipleMappingDictionary : {stopwatch.Elapsed}");

    return Ok();
});
```

