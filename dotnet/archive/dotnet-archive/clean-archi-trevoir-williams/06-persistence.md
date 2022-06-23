# 06 Couche `Persistence`

> **Jason Taylor** place cette partie dans la couche `Infrastructure`.

## Ajouter `EF Core`

On crée un projet `classlib`, on lui ajoute une référence vers la couche application.

```bash
dotnet new classlib -o Persistence

dotnet add Persistence reference Application 
```

Puis on ajoute le package de `EF Core` pour `SQL Server`.

```bash
cd Persistence
dotnet add package Microsoft.EntityFrameworkCore.SqlServer --version 5.0.10
```

On doit aussi installer `Configuration Extensions`

```bash
dotnet add package Microsoft.Extensions.Options.ConfigurationExtensions --version 5.0.0
```

On va créer à la racine la classe `LeaveManagementDbContext`

```cs
using Domain;  // <= Domain Project
using Microsoft.EntityFrameworkCore;

namespace Persistence
{
    public class LeaveManagementDbContext : DbContext
    {
       public LeaveManagementDbContext(DbContextOptions<LeaveManagementDbContext> options) : base(options)
        {
            
        }

        public DbSet<LeaveRequest> LeaveRequests { get; set; }
        public DbSet<LeaveType> LeaveTypes { get; set; }
        public DbSet<LeaveAllocation> LeaveAllocations { get; set; } 
    }
}
```

Je remarque que j'atteins le projet `Domain` alors que je n'ai de référence que vers `Application`.

Les références sont donc transitives.

On va maintenant `overrider` la méthode `OnModelCreating` :

```cs
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
  modelBuilder
    .ApplyConfigurationsFromAssembly(typeof(LeaveManagementDbContext).Assembly);
}
```

Cette méthode est appelée à chaque fois que la `BDD` est générée.

> Pour l'instant je ne sais pas à quoi cela sert ?



### Configurer l'`Audit` des changements.

Pour rappel nos entités héritent de `AuditableEntity` :

```cs
public class AuditableEntity
{
  public int Id { get; set; }
  public DateTime DateCreated { get; set; }
  public string CreatedBy { get; set; }
  public DateTime LastModifiedDate { get; set; }
  public string LastModifiedBy { get; set; }
}
```

On va `overrider` aussi la méthode `SaveChangesAsync` pour pouvoir ajouter `DateCreated` et `LastModifiedDate` facilement.

```cs
public override Task<int> SaveChangesAsync(CancellationToken cancellationToken = default)
{
	foreach(var entry in ChangeTracker.Entries<AuditableEntity>())
  {
    entry.Entity.LastModifiedDate = DateTime.Now;
    
    if(entry.State == EntityState.Added)
    {
      entry.Entity.DateCreated = DateTime.Now;
    }
  }
  
  return base.SaveChangesAsync(cancellationToken);
}
```



## Implémentation de la couche `Persistence`

## `GenericRepository`

On crée un nouveau dossier `Repositories` et dedans une classe `GenericRepository` qui représente l'implémentation de l'interface `IGenericRepository`.

```cs
public class GenericRepository<T> : IGenericRepository<T> where T : class
{
	// 1. On injecte LeaveManagementDbContext
  private readonly LeaveManagementDbContext _dbContext;
  
  public GenericRepository(LeaveManagementDbContext dbContext)
  {
    _dbContext = dbContext;
  }
```

### `Add`

```cs
public async Task<T> Add(T entity)
{
  await _dbContext.AddAsync(entity);
  await _dbContext.SaveChangesAsync();

  return entity;
}
```

`EF Core` est assez intelligent pour reconnaître le type de `entity`.



### `Delete`

```cs
public async Task Delete(T entity)
{
  _dbContext.Set<T>().Remove(entity); // _dbContext.Remove(entity);
  await _dbContext.SaveChangesAsync();
}
```



### `Exists`

```cs
public async Task<bool> Exists(int id)
{
  var entity = await Get(id);
  return entity != null;
}
```



### `Get`

```cs
public async task<T> Get(int id) => await _dbContext.Set<T>().FindAsync(id);
```



### `GetAll`

```cs
public async Task<List<T>> GetAll() => await _dbContext.Set<T>().ToListasync();
```



### `Update`

```cs
public async Task Update(T entity)
{
  //await _dbContext.UpdateAsync(entity);
  _dbContext.Entry(entity).State = EntityState.Modified;
  await _dbContext.SaveChangesAsync();
}
```

ici on modifie le ststus de l'entrée avant de l'enregistrée.

## Des `repositories` spécifiques :

##  `LeaveTypeRepository`

Il ne faut pas oublier de passer le `dbContext` au constructeur du parent : `base(dbContext)`.

```cs
public class LeaveTypeRepository : GenericRepository<LeaveType>, ILeaveTypeRepository
{
  private readonly LeaveManagementDbContext _dbContext;
  public LeaveTypeRepository(LeaveManagementDbContext dbContext) : base(dbContext)
  {
    _dbContext = dbContext;
  }
}
```



## `LeaveRequestRepository`

```cs
public class LeaveRequestRepository : GenericRepository<LeaveRequest>, ILeaveRequestRepository
{
  private readonly LeaveManagementDbContext _dbContext;
  public LeaveRequestRepository(LeaveManagementDbContext dbContext) : base(dbContext)
  {
    _dbContext = dbContext;
  }
```



### `ChangeApprovalStatus`

```cs
public async Task ChangeApprovalStatus(LeaveRequest leaveRequest, bool? approvalStatus)
{
  leaveRequest.Approved = approvalStatus;
  _dbContext.Entry(leaveRequest).State = EntityState.Modofied;
  await _dbContext.SaveChangesAsync();
}
```

Toujours modifier le status d'une entrée non trackée.



### `GetLeaveRequestsWithDetails`

```cs
public async Task<List<LeaveRequest>> GetLeaveRequestsWithDetails() 
  => await _dbContext.LeaveRequests.Include(q => q.LeaveType).ToListAsync();
```



### `GetLeaveRequestWithDetails`

```cs
public async Task<LeaveRequest> GetLeaveRequestWithDetails(int id) 
  => _dbContext.LeaveRequests.Include(q => q.LeaveType).FirstOrDefaultAsync( q.Id == id); 
```

On ne peut pas utiliser `Include` et `FindAsync` ensemble, à la place on utilise `FirstOrDefaultAsync`.

On peut aussi utiliser `SingleOrDefaultAsync` (peut-être plus approprié pour un `Id` sensé être unique).



## `LeaveAllocationRepository`

```cs
public class LeaveAllocationRepository : GenericRepository<LeaveAllocation>, ILeaveAllocationRepository
{
  private readonly LeaveManagementDbContext _dbContext;
  public LeaveAllocationRepository(LeaveManagementDbContext dbContext) : base(dbContext)
  {
    _dbContext = dbContext;

  }
```



### `GetLeaveAllocationsWithDetails`

```cs
public async Task<List<LeaveAllocation>> GetLeaveAllocationsWithDetail() => await _dbContext.LeaveAllocations.Include(q => q.LeaveType).ToListAsync();
```



### `GetLeaveAllocationWithDetail`

```cs
public async Task<LeaveAllocation> GetLeaveAllocationWithDetail(int id) => await _dbContext.LeaveAllocations.Include(q => q.LeaveType).FirstOrDefaultAsync(q => q.Id == id);
```



## `PersistenceServicesRegistration`

On doit maintenant crée l'extension de méthode pour enregistrer dans le projet `Startup` nos services de `repository`.

```cs
public static class PersistenceServicesRegistration
{
  public static IServiceCollection AddPersitenceService(this IServiceCollection services, Iconfiguration configuration)
  {
    services.AddDbContext<LeaveManagementDbcontext>(options => 
        options.UseSqlServer(configuration.GetConnectionString("LeaveManagementConnectionString")));

    services.AddScoped(typeof(IGenericRepository<>), typeof(GenericRepository<>));
    
    services.AddScoped<ILeaveTypeRepository, LeaveTypeRepository>();
    services.AddScoped<ILeaveRequestRepository, LeaveRequestRepository>();
    services.AddScoped<ILeaveAllocationRepository, LeaveAllocationRepository>();

    return services;
  }
}
```

> ### `AddScoped`
>
> Pourquoi utiliser `AddScoped` plutôt que `AddSingleton` ou `AddTransient` ?
>
> Déjà il faut savoir que `DbContext` est un service `Scoped` par `EF Core`.
>
> `DbContext` est construit sur le concept de `Unit Of Work`, il gère les transactions.
>
> Si sa durée de vie est celle d'une requête, la `transaction` est effectivement lié à une seule requête.
>
> Quand `SaveChange` est appelé, on sait que cela ne concerne que la requête en cours.
>
> Si `DbContext` était un singleton, tous les changement de toutes les requêtes seraient affectés par l'appellle à `SaveChange`.
>
> Pour la durée de vie `Transient`, chaque appelle à `DbContext` créerait une nouvelle transaction, ce n'est en générale pas ce qu'on veut.
>
> ### Réponse complete traduite
>
> https://entityframeworkcore.com/knowledge-base/55722087/why-is-services-adddbcontext-dbcontext----makes-dbcontext-as-a-scoped-service-shoudln-t-be-a-singleton-service-
>
> `DbContext` ne doit pas être utilisé comme un `singleton` car il contient un objet de connexion qui ne peut pas être utilisé par plusieurs `threads` en même temps. Vous rencontrerez des erreurs si deux requêtes tentent de l'utiliser en même temps. Si votre service dépend du `context`, le service ne peut pas être un `singleton`.
>
> `Scoped` a du sens puisqu'il vous permet de faire circuler des objets de base de données entre les services et d'obtenir le même `DbContext` dans tous les services, de sorte que vous pouvez requêter des entités dans un service et enregistrer les modifications dans un autre.
>
> Vous pouvez le changer en `transient` si vous avez besoin d'exécuter des requêtes en parallèle dans deux services par exemple. La durée de vie du service est un paramètre de `AddDbContext()`.
>
> Traduit avec www.DeepL.com/Translator (version gratuite)

On passe la `configuration` pour récupérer le `ConnectionString`.

Bien remarquer la syntaxe particulière pour les type génériques :

```cs
services.AddScoped(typeof(IGenericRepository<>), typeof(GenericRepository<>));
```















