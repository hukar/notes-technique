# DD Création d'un `CRUD` avec `Repository`



## `Repository`

On va d'abord créer un `repository` très simple :

```cs
class CustomerRepository
{
  private readonly Dictionary<Guid, Customer> _customers = new();
  
  public void Create(Customer customer)
  {
    if(customer is null)
    {
      return;
    }
    
    _customers[customer.Id] = customer;
  }
  
  public Customer GetById(Guid id) => _customer[id];
  
  public List<Customer> GetAll() => _customers.Values.ToList();
  
  public void Update(Customer customer)
  {
    var existingCustomer = GetById(customer.Id);
    if(existingCustomer is null)
    {
      return;
    }
    
    _customers[customer.Id] = customer;
  }
  
  public void Delete(Guid id)
  {
    _customers.Remove(id);
  }
}
```



`Program.cs`

```cs
var builder = WebApplication.CreateBuilder(args);
builder.Services.AddSingleton<CustomerRepository>();

var app = builder.Build();
```

On enregistre un service avec `builder.Services.AddSomething`.



## `GetAll`

```cs
app.MapGet("/customers", ([FromServices] CustomerRepository repo) => repo.GetAll());
```

On injecte ensuite le `service` avec l'annotation `[FromServices]`.



## `GetById`

> ### `Results` => `IResult`
>
> Les methods static de `Results` renvoie toute un nouveau type de `.net 6` : `IResult`.
>
> C'est l'équivalent de `IActionResult` de `Mvc`.

```cs
app.MapGet("/customers/{id}", ([FromServices] CustomerRepository repo, Guid id) => repo.GetById(id) is Customer customer ? Results.Ok(customer) : Results.NotFound()
);
```

On un paramètre d'`url` avec `/{id}` et on le récupère en argument de la `lambda` :

`(Guid id) => ...`



## `Create`

```cs
app.MapPost("/customers", ([FromServices] CustomerRepository repo, Customer customer) => {
  repo.Create(customer);
  return Results.Created(&"/customers/{customer.Id}", customer);
});
```

`ASP.Net` sait que le `customer` doit être mappé à partir du `body` de la requête.

On retourne l'`URL` de la ressource ainsi que la ressource elle-même (`customer`).



## `Update`

```cs
app.MapPut("/customers/{id}", ([FromServices] CustomerRepository repo, Guid id, Customer updatedCustomer) => {
  var customer = repo.GetById(id);
  if(customer is null)
  {
    return Results.NotFound();
  }
  
  repo.Update(updatedCustomer);
  return Results.Ok(updateCustomer);
});
```



## `Delete`

```cs
app.MapDelete("/customers/{id}", ([FromServices] CustomerRepository repo, Guid id) => {
  repo.Delete(customer);
  return Results.Ok(); // NoContent();
});
```

```cs
app.Run();

record Customer(Guid Id, string FullName);
```

> Il semblerait que si un `customer` était envoyé en `payload`, il ne serait pas inféré dans une méthode `MapDelete`, il faudrait alors utiliser `[FromBody]` :
>
> ```cs
> app.MapDelete("/customers", (IRepository db, [FromBody] Customer deletedCustomer) => {
>     db.Delete(deletedCustomer);
>     return Ok();
> });
> ```
>
> Le `binding` des données par inférence est donc différent suivant le `Verb` de la méthode `MapVerb`.
