# 13 `EF Core`

## Création de `DataStore.Ef`

On crée un projet de `classlib` nommer `DataStore.EF`.

```bash
dotnet new classlib -o DataStore.EF
```

On ajoute le package `Microsoft.EntityFrameworkCore`

```bash
dotnet add package Microsoft.EntityFrameworkCore --version 5.0.5
```



## Création de la classe `BugsContext`

```cs
namespace DataStore.EF
{
    public class BugsContext : DbContext
    {
        public BugsContext(DbContextOptions options) : base(options)
        {}
        
        public DbSet<Project> Projects { get; set; }
        public DbSet<Ticket> Tickets { get; set; }
        
    }
}
```

Créer un constructeur et passer au constructeur de `base` les `options`

Le `context` représente la base de données en mémoire.

`BugsContext` représente la `BDD` et `DbSet` représente les `tables` de cette `BDD`.

Il faut ajouter une référence vers `Core`

```bash
dotnet add DataStore.EF reference Core
```



## Ajouter une relation

`EF Core` interprète par convention que `ClassNameId` ou `Id` est une `primary key`.

On va *overrider* la méthode `OnModelCreating` :

```cs
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<Project>()
        .HasMany(p => p.Tickets)
        .WithOne(t => t.Project)
        .HasForeignKey(t => t.ProjectId);
}
```



## data Seeding

```cs
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    modelBuilder.Entity<Project>()
        .HasMany(p => p.Tickets)
        .WithOne(t => t.Project)
        .HasForeignKey(t => t.ProjectId);
    
    // seeding
    modelBuilder.Entry<Project>.HasData(
    	new Project { ProjectId = 1, Name = "Project 1" },
        new Project { ProjectId = 2, Name = "Project 2" }
    );
    
    modelBuilder.Entity<Ticket>.HasData(
    	new Ticket { TicketId = 1, Title = "Bug 1", ProjectId = 1 },
		new Ticket { TicketId = 2, Title = "Bug 2", ProjectId = 1 },
    	new Ticket { TicketId = 3, Title = "Bug 3", ProjectId = 2 },
    );
}
```

Lorsqu'on `seed` les données, la génération d'`Id` automatique n'est pas activée, c'est pourquoi on doit renseigné l'`Id` à la main.



## `Startup.cs`

### Créer une référence vers `DataStore.EF`

```bash
cd WebApi
dotnet add redference ../DataStore.EF
```



### Configurer le service `In Memory database`

On va utiliser une `in memory database` lorsque l'on est en développement.

On va ajouter un package `EntityFrameworkCore.InMemory` dans notre projet `WebApi` :

```bash
cd WebApi
dotnet add package Microsoft.EntityFrameworkCore.InMemory --version 5.0.5
```

On va configurer l'injection de service :

```cs
public void ConfigureServices(IServiceCollection services)
{
	services.AddDbContext<BugsContext>(options => {
        options.UseInMemoryDatabase("Bugs");
    });
    services.AddControllers();
```

Les options passées ici sont ensuite récupérées dans le constructeur de la classe `BugsContext` et passées au constructeur de base (`DbContext`) :

```cs
// DataStore.EF/BugsContext.cs


public class BugsContext : DbContext
{
    public BugsContext(DbContextOptions options) : base(options)
    {

    }
```

On voit l'option `in-memory` dans le debugger :

<img src="assets/options-in-memory-0205414.png" alt="options-in-memory" style="zoom:50%;" />



On ne veut utiliser la `In Memory Database` que pour le développement :

```cs
public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
{
    if (env.IsDevelopment())
    {
        app.UseDeveloperExceptionPage();
        app.UseSwagger();
        app.UseSwaggerUI(c => c.SwaggerEndpoint("/swagger/v1/swagger.json", "PlatformDemo v1"));
    }
```

On va injecter l'environement drectement dans le constructeur de `Startup` pour pouvoir aussi l'utiliser dans `ConfigureServices` :

```cs
private readonly IWebHostEnvironment _env;
public Startup(IConfiguration configuration, IWebHostEnvironment env)
{
    _env = env;
    Configuration = configuration;
}
```

On peut maintenant l'utiliser dans `Configureservices` :

```cs
public void ConfigureServices(IServiceCollection services)
{
	if(_env.IsDevelopment())
    {
        services.AddDbContext<BugsContext>(options =>
        {
            options.UseInMemoryDatabase("Bugs");
        }); 
    }
   
```

### Construire la base de données `in-memory`

Dans la méthode `Configure` on injecte le `BugsContext context` et on crée la `BDD` :

```cs
public void Configure(IApplicationBuilder app, IWebHostEnvironment env, BugsContext context)
{
    if (env.IsDevelopment())
    {
        // Delete and Create in-memory database
        context.Database.EnsureDeleted();
        context.Database.EnsureCreated();
```



## Temps de vie d'un service

`scoped` le service est instancié pur une requête et une réponse d'un contrôleur.

<img src="assets/life-time-scoped.png" alt="life-time-scoped" style="zoom:50%;" />

Un `context` est donc instancié pour chaque nouveau cycle requête-réponse.

