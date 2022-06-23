# 25 `Unit Of Work`

## Création des `repository`

On crée deux dossiers `IRepository` et `Repository`.

On crée une intreface générique dans `IRepository` :

### `IGenericRepository.cs`

```cs
namespace Server.IRepository;

public interface IGenericRepository<T> where T : class
{
    Task<IList<T>> GetAll(
    	Expression<Func<T, bool>> expression = null,
      Func<IQueryable<T>, IOrderedQueryable<T>> orderBy = null,
      List<string> includes = null
    );
  
  Task<T> Get(Expression<Func<T, bool>> expression, List<string> includes = null);
  
  Task Insert(T entity);
  Task InsertRange(IEnumerable<T> entities);
  
  Task Delete(int id);
  void DeleteRange(IEnumerable<T> entities);
  
  void update(T entity);
}
```

> ### Différence entre un simple `Delegate` et une `Expression`
>
> ```cs
> Func<int, bool> Is42 = (value) => value == 42;
> // vs
> Expression<Func<int, bool>> Is42 = (value) => value == 42;
> ```
>
> En stockant une `lambda` comme un `delegate`, vous stockez une instance spécifique d'un `delegate` qui effectue une certaine action. Il ne peut pas être modifié, il suffit de l'appeler. Une fois que vous avez votre `delegate`, vous avez des options limitées pour inspecter ce qu'il fait et ainsi de suite.
>
> En stockant une `lambda` sous forme d'`expression`, vous stockez un `expression tree` qui représente le `delegate`. Il peut être manipulé pour faire d'autres choses comme changer ses paramètres, changer le corps et lui faire faire quelque chose de radicalement différent. Elle peut même être compilée en un `delegate` pour que vous puissiez l'appeler si vous le souhaitez. Vous pouvez facilement inspecter l'`expression` pour voir quels sont ses paramètres, ce qu'elle fait et comment elle le fait. C'est quelque chose qu'un `query provider` peut utiliser pour comprendre et traduire une expression dans un autre langage (comme écrire une requête `SQL` pour un `expression tree` correspondant).
>
> https://stackoverflow.com/questions/7765932/whats-the-purpose-of-the-expression-class



### `GenericReposotory`

Implémentation avec le `DbContext` de l'application : `ApplicationDbContext`.

```cs
public class GenericRepository<T> : IGenericRepository<T> where T : class
{
  private readonly ApplicationDbContext _applicationDbContext;
  private readonly DbSet<T> _dbSet;

  public GenericRepository(ApplicationDbContext applicationDbContext)
  {
    _applicationDbContext = applicationDbContext;
    _dbSet = _applicationDbContext.Set<T>();
  }
```

### `Delete`

```cs
public async Task Delete(int id)
{
  var record = await _dbSet.FindAsync(id);
  _dbSet.Remove(record);
}

public void DeleteRange(IEnumerable<T> entities)
{
  _dbSet.RemoveRange(entities);
}
```

### `Read`

Pour des raisons de performances on va `Detach` les `entities` récupérées.

On ne veut pas `tracker` toutes les `entities` : `AsNoTracking()`.

```cs
public async Task<T> Get(Expression<Func<T, bool>> expression, List<string> includes = null)
{
  // On initialise une query
  IQueryable<T> query = _dbSet;
  
  if(includes is not null)
  {
    foreach(var include in includes)
    {
      query = query.Include(include);
    }
  }
  
  return await query.AsNoTracking().FirstOrDefaultAsync(expression);
}

public async Task<IList<T>> GetAll(Expression<Func<T, bool>> expression = null, Func<IQueryable<T>, IOrderedQueryable<T>> orderBy = null, List<string> includes = null)
{
  IQueryable<T> query = _dbSet;
  
  if(expression is not null)
  {
    query = query.Where(expression);
  }
  
  if(includes is not null)
  {
    foreach(var include in includes)
    {
      query = query.Include(include);
    }
  }
  
  if(orderBy is not null)
  {
    query = orderBy(query);
  }
  
  return await query.AsNoTracking().ToListAsync();
}
```


### `Create`
```cs
public async Task Insert(T entity)
{
  await _dbSet.AddAsync(entity);
}
public async Task InsertRange(IEnumerable<T> entities)
{
  await _dbSet.AddRangeAsync(entities);
}
```

### `Update`

Comme on a `Detach` les `entities` en lecture, on doit ici la `Attach` pour qu'elle soit `trackée` par `Entity Framework`.

```csharp
public void update(T entity)
{
  _dbSet.Attach(entity);
  // On doit dire à EF Core que entity a été modifiée
  _applicationDbContext.Entry(entity).State = EntityState.Modified;
}
```



## Création de `Unit Of Work`

### Interface : `IUnitOfWork`

```cs
namespace Server.IRepository;
public interface IUnitOfWork : IDisposable
{
    Task Save();
    
    IGenericRepository<Make> MakeRepository { get; }
    // IGenericRepository<Model> ModelRepository { get; }
    // IGenericRepository<Vehicle> VehicleRepository { get; }
    // IGenericRepository<Customer> CustomerRepository { get; }
    // IGenericRepository<Colour> ColourRepository { get; }
    // IGenericRepository<Booking> BookingRepository { get; } 
}
```



### Implémentation : `UnitOfWork`

```cs
namespace Server.Repository;

public class UnitOfWork : IUnitOfWork
{
  private readonly ApplicationDbContext _applicationDbContext;
  private IGenericRepository<Make> _makeRepository;

  public UnitOfWork(ApplicationDbContext applicationDbContext)
  {
    _applicationDnContext = applicationDbContext;
  }

  public IGenericRepository<Make> MakeRepository 
    => _makeRepository ??= new GenericRepository<Make>(_applicationDbContext);

  public void Dispose()
  {
    _applicationDbContext.Dispose();
    GC.SuppressFinalize(this);
  }

  public async Task Save()
  {
    await _applicationDbContext.SaveChangesAsync();
  }
}
```



## Ajouter `UnitOfWork` comme service

Dans `Program.cs`

```cs
builder.Services.AddTransient<IUnitOfWork, UnitOfWork>();
```



## Utilisation dans un `controller`

Au lieu d'injecter directement le `context` dans un `controller`, on va injecter notre service d'`Unit Of Work`.

```cs
[Route("[controller]")]
[ApiController]
public class MakesController : ControllerBase
{
    private readonly IUnitOfWork _unitOfWork;

    public MakesController(IUnitOfWork unitOfWork)
    {
        _unitOfWork = unitOfWork;
    }
```



### `Read`

```cs
// GET: Makes
[HttpGet]
public async Task<ActionResult> GetMakes()
{
    var makes = await _unitOfWork.MakeRepository.GetAll();
    return Ok(makes);
}

// GET: Makes/5
[HttpGet("{id}")]
public async Task<ActionResult> GetMake(int id)
{
    var make = await _unitOfWork.MakeRepository.Get(q => q.Id == id);

    if (make == null)
    {
        return NotFound();
    }

    return Ok(make);
}
```

Exemple avec `includes` :

> `parameterName: parameter` cette syntaxe permet de passer un paramètre lorsque les précédents sont `null`. Cela lève l'ambiguïté.

```cs
// GET: Vehicles
[HttpGet]
public async Task<ActionResult> GetVehicles()
{
    var includes = new List<string> { "Make", "Model", "Colour" };
    var vehicles = await _unitOfWork.VehicleRepository.GetAll(includes: includes);
    return Ok(vehicles);
}
```



### `Update`

```cs
// PUT: Makes/5
// To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
[HttpPut("{id}")]
public async Task<IActionResult> PutMake(int id, Make make)
{
    if (id != make.Id)
    {
        return BadRequest();
    }

    _unitOfWork.MakeRepository.Update(make);

        try
        {
            await _unitOfWork.Save();
        }
    catch (DbUpdateConcurrencyException)
    {
        if (!await MakeExists(id))
        {
            return NotFound();
        }
        else
        {
            throw;
        }
    }

    return NoContent();
}
```



### `Create`

```cs
// POST: Makes
// To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
[HttpPost]
public async Task<ActionResult<Make>> PostMake(Make make)
{
    await _unitOfWork.MakeRepository.Insert(make);
    await _unitOfWork.Save();

    return CreatedAtAction("GetMake", new { id = make.Id }, make);
}
```



### `Delete`

```cs
// DELETE: Makes/5
[HttpDelete("{id}")]
public async Task<IActionResult> DeleteMake(int id)
{
    if(!await MakeExists(id))
    {
        return NotFound();
    }
    await _unitOfWork.MakeRepository.Delete(id);
    await _unitOfWork.Save();

    return NoContent();
}

private async Task<bool> MakeExists(int id)
{
    var make = await _unitOfWork.MakeRepository.Get(q => q.Id == id);

    return make != null;
}
```



## Enregistrer l'`Audit`

On veut centraliser pour toutes les entités la gestion du suivi (l'`audit`) en enregistrant systématiquement `DateUpdated` , `DateCreated`, `UpdatedBy` et `CreatedBy`.

On va se servir de l'utilité d'avoir une méthode `Save` centralisée dans le `UnitOfWork`.

```cs
public async Task Save(HttpContext httpContext)
{
    var user = httpContext.User.Identity.Name;
    // Attention si pas d'utilisateur connecté
    user ??= "No Body Connected";
    
    var entries = _applicationDbContext.ChangeTracker.Entries()
        .Where(q => q.State == EntityState.Modified || q.State == EntityState.Added);
    foreach(var entry in entries)
    {
        ((BaseDomainModel)entry.Entity).DateUpdated = DateTime.Now;
        ((BaseDomainModel)entry.Entity).UpdatedBy = user;

        if(entry.State == EntityState.Added)
        {
            ((BaseDomainModel)entry.Entity).DateCreated = DateTime.Now;
            ((BaseDomainModel)entry.Entity).CreatedBy = user;
        }
    }
    
    await _applicationDbContext.SaveChangesAsync();
}
```

On doit modifier l'interface :

```cs
public interface IUnitOfWork : IDisposable
{
    Task Save(HttpContext httpContext);
```

On passe le `httpContext` dans le `controller` :

```cs
[HttpPost]
public async Task<ActionResult<Make>> PostMake(Make make)
{
    await _unitOfWork.MakeRepository.Insert(make);
    await _unitOfWork.Save(HttpContext);
```

`HttpContext` étant une propriété du `controller` ( `this.HttpContext` ).









