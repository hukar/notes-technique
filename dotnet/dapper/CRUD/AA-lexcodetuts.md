# `Crud` avec `Dapper`

## Proposer sur `Youtube` par `alexcodetuts`

https://www.youtube.com/watch?v=3moKgzS7AWo&ab_channel=alexcodetuts



## Model

```cs
public class TaskToDo 
{
    public int Id { get; set; }
    public string Name { get; set; } = string.Empty;
    public string description { get; set; } = string.Empty;
    public TaskToDoStatus status { get; set; }
    public DateTime DueDate { get; set; }
    public DateTime DateCreated { get; set; }
    public DateTime DateModified { get; set; }
}

public enum TaskToDoStatus 
{
    created,
    Active,
    Done
}
```



## `ConnectionString`

`appsettings.json`

```json
{
    "ConnectionStrings": {
        "HukarConnect": "Server=localhost,1433;Database=NewDapperTest;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true;Encrypt=false"
    },
```

> `Encrypt=false` corrige un problème d'encryption => à étudier pour être sécure

## Repository Interface

```cs
public interface IGenericRepository<T> where T : class
{
    Task<T?> Get(int id);
    Task<IEnumerable<T>> GetAll();
    Task<int> Create(T item);
    Task<int> Update(T item);
    Task<int> Delete(int id);
}
```

```cs
public interface ITaskToDoRepository : IGenericRepository<TaskToDo> { }
```



## Repository Implémentation

```cs
public class TaskToDoRepository : ITaskToDoRepository
{
    private readonly IConfiguration _configuration;

    public TaskToDoRepository(IConfiguration configuration)
    {
        _configuration = configuration;
    }
```

### `Get`

```cs
public async Task<TaskToDo?> Get(int id)
{
    var sql = "SELECT * FROM Tasks WHERE Id = @Id;";
    using var connection = new SqlConnection(_configuration.GetConnectionString("HukarConnect"));
    connection.Open();
    var result = await connection.QueryAsync<TaskToDo>(sql, new { Id = id });
    return result.FirstOrDefault();
}
```



### `GetAll`

```cs
public async Task<IEnumerable<TaskToDo>> GetAll()
{
    var sql = "SELECT * FROM Tasks;";
    using var connection = new SqlConnection(_configuration.GetConnectionString("HukarConnect"));
    connection.Open();
    var result = await connection.QueryAsync<TaskToDo>(sql);
    return result;
}
```



### `Create`

```cs
public async Task<int> Create(TaskToDo item)
{
    item.DateCreated = DateTime.Now;
    var sql = "INSERT INTO Tasks (Name, Description, Status, DateCreated, DueDate) VALUES (@Name, @Description, @Status, @DateCreated, @DueDate);";
    using var connection = new SqlConnection(_configuration.GetConnectionString("HukarConnect"));
    connection.Open();
    var affectedRows = await connection.ExecuteAsync(sql, item);
    return affectedRows;
}
```



### `Updtate`

```cs
public async Task<int> Update(TaskToDo item)
{
    item.DateModified = DateTime.Now;
    var sql = "UPDATE Tasks SET Name = @Name, Description = @Description, Status = @Status, DateCreated = @DateCreated, DueDate = @DueDate WHERE Id = @Id;";
    using var connection = new SqlConnection(_configuration.GetConnectionString("HukarConnect"));
    connection.Open();
    var affectedRows = await connection.ExecuteAsync(sql, item);
    return affectedRows;
}
```



### `Delete`

```cs
public async Task<int> Delete(int id)
{
    var sql = "DELETE FROM Tasks WHERE Id = @Id;";
    using var connection = new SqlConnection(_configuration.GetConnectionString("HukarConnect"));
    connection.Open();
    var affectedRows = await connection.ExecuteAsync(sql, new { Id = id });
    return affectedRows;
}
```



## définition d'un `endpoint` avec `minimal api`

```cs
namespace CrudBasicMinimal.Endpoints;

public static class TaskToDoEndpoints
{
    public static IServiceCollection RegisterTaskToDoServices(this IServiceCollection services)
    {
        services.AddScoped<ITaskToDoRepository, TaskToDoRepository>();
        
        return services;
    }

    public static IEndpointRouteBuilder MapTaskToDoEndpoint(this IEndpointRouteBuilder endpoints)
    {
        // GET ALL
        endpoints.MapGet(
            "/task", 
            async (ITaskToDoRepository repo) => {
            var tasks = await repo.GetAll();
            return Ok(tasks);
        });

        // GET BY ID
        endpoints.MapGet(
            "/task/{id:int}", 
            async (ITaskToDoRepository repo, int id) => {
            return await repo.Get(id) is TaskToDo task ? Ok(task) : NotFound();
        });

        // CREATE
        endpoints.MapPost(
            "/task", 
            async (ITaskToDoRepository repo, TaskToDo task) => {
            var numberOfRows = await repo.Create(task);
            return Ok(numberOfRows);
        });
        
        // DELETE
        endpoints.MapDelete(
            "/task/{id:int}", 
            async (ITaskToDoRepository repo, int id) => {
            var numberOfRows = await repo.Delete(id);
            return Ok(numberOfRows);
        });       
        
        // UPDATE
        endpoints.MapPut(
            "/task", 
            async (ITaskToDoRepository repo, TaskToDo task) => {
            var numberOfRows = await repo.Update(task);
            return Ok(numberOfRows);
        });

        return endpoints;
    }
}
```

