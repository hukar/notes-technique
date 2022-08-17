# 06 Procédure stockée avec `output parameter`

## Créer la procédure

```sql
CREATE PROCEDURE spAddEmployee 
    @Name nvarchar(50),
    @Gender nvarchar(20),
    @Salary int,
    @EmployeeId int Out
AS
BEGIN
	INSERT INTO Employee VALUES (@Name, @Gender, @Salary)
	SELECT @EmployeeId = SCOPE_IDENTITY()
END
```

### Tester la procédure stockée

```sql
DECLARE @EmpId int

EXECUTE spAddEmployee 'Sarah', 'female', 5670, @EmpId out
PRINT 'EmployeeId = ' + CAST(@EmpId as nvarchar(2))
```

> Pour vider une `table` : `TRUNCATE`
>
> ```sql
> TRUNCATE TABLE Employee
> ```
>
> 



## Appeler une procédure stockée depuis une application `.NET`

Mise en place

```c#
string connectionString = builder.Configuration.GetConnectionString("HukarConnection");

builder.Services.AddScoped(_ => new SqlConnection(connectionString));

app.MapPost("/employee", AddEmployee);
```

Méthode du `Endpoint`

```c#
EmployeeResponseDto AddEmployee(SqlConnection con,EmployeeRequestDto employee)
{
    int employeeId = 0;
    
    SqlCommand cmd = new SqlCommand("spAddEmployee", con);
    cmd.CommandType = CommandType.StoredProcedure;
    
    cmd.Parameters.AddWithValue("@Name", employee.Name);
    cmd.Parameters.AddWithValue("@Gender", employee.Gender);
    cmd.Parameters.AddWithValue("@Salary", employee.Salary);
    
    // GESTION DU OUTPUT PARAMETER
    SqlParameter outputParameter = new SqlParameter();
    outputParameter.ParameterName = "@EmployeeId";
    outputParameter.SqlDbType = SqlDbType.Int;
    outputParameter.Direction = ParameterDirection.Output;
    
	cmd.Parameters.Add(outputParameter);
    
    con.Open();
    cmd.ExecuteNonQuery();

    var employeeId = (int)outputParameter.Value;

    return new EmployeeResponseDto(employeeId, employee.Name, employee.Gender, employee.Salary);
}
```

On récupère la valeur de sortie avec `outputParameter.Value`.

```http
### TEST AVEC L'EXTENSION REST CLIENT VSCODE
POST https://localhost:7147/employee
Content-Type: application/json

{
    "Name": "Raymond",
    "Gender": "male",
    "Salary": 3480
}
```

```json
{
  "employeeId": 3,
  "name": "Raymond",
  "gender": "male",
  "salary": 3480
}
```













































