# 03 `EF Core`



## Database `context`

```cs
public class AppDbContext : DbContext
{
  public AppDbContext(DbContextOptions<AppDbContext> options) : base(options)
  {}
  
  public DbSet<Pie> Pies { get; set; }
}
```



## Connection string

Elle se trouve dans le fichier `appsettings.json`

```json
{
    "ConnectionStrings": {
        "HukarConnection": "Server=localhost,1433; 
      						  Database=BethanyPieShop; 
      						  User=sa; 
      						  Password=huk@r2Xmen99"
    },
    // ...
}
```

> ### Retire le fichier `appsettings.json` du repository `GIT`
>
> ```bash
> git rm --cached appsettings.json
> ```
>
> `--cached` retire de l'index seulement.
>
> `-f` retire le chier physique.
>
> Et l'ajouter dans `.gitignore`
>
> ```bash
> /bin
> /obj
> appsettings.json
> ```
>
> Ensuite on `commit` le tout pour rendre cela actif.

## Injection : `Startup.cs`

```cs
public void ConfigureServices(IServiceCollection services)
{
  services.AddDbContext<AppDbContex>(options => options.UseSqlServer(Configuration.GetConnectionString("HukarConnection")));
}
```



### Utiliser la `Configuration`

Cela permet d'injecter directement le contenu de `appsettings.json`.

```cs
public class Startup
{
  public IConfiguration Configuration { get; }
  
  public Startup(IConfiguration configuration)
  {
    Configuration = configuration;
  }
  
  // ...
}
```

Sinon on ne peut pas utiliser l'objet `Configuration`.



## Rechercher des données `LINQ`

```cs
_appDbContext.Pies.Include(c => c.Category).Where(p => p.IsPieOfTheWeek);
```



## Modifier des données

```cs
order.OrderDetails.Add(orderDetail);

_appDbContext.Orders.Add(Order);
_appDbContext.SaveChanges();
```



## Implémentation d'un `repository` réel

`PieRepository.cs`

```cs
public class PieRepository : IPieRepository
{
  private readonly AppDbContext _appDbContext;
  public PieRepository(AppDbContext appDbContext)
  {
    _appDbContext = appDbContext;
  }

  public IEnumerable<Pie> AllPies => _appDbContext.Pies.Include(p => p.Category);

  public IEnumerable<Pie> PiesOfTheWeek => _appDbContext.Pies.Include(p => p.Category).Where(p => p.IsPieOfTheWeek);

  public Pie GetPieById(int pieId) => _appDbContext.Pies.Find(pieId);
}
```

`CategoryRepository.cs`

```cs
public class CategoryRepository : ICategoryRepository
{
    private readonly AppDbContext _appDbContext;
    public CategoryRepository(AppDbContext appDbContext)
    {
        _appDbContext = appDbContext;

    }
    public IEnumerable<Category> AllCategories => _appDbContext.Categories;
}
```



## Switcher sur le `repository` réel

C'est dans `Stratup.cs` => `ConfigureServices` :

```cs
// services.AddScoped<IPieRepository, MockPieRepository>();
// services.AddScoped<ICategoryRepository, MockCategoryRepository>();

services.AddScoped<IPieRepository, PieRepository>();
services.AddScoped<ICategoryRepository, CategoryRepository>();
```



## Créer la `DB`

### `Migration`

`EF Core` utilise le modèle pour générer des `migrations` permettants de créer la `DB`.

Il y a deux commandes à retenir :

```bash
dotnet ef migrations add <MyMigrationName>
```

et

```bash
dotnet ef database update
```

### `Seed Data` : Créer des données de départ

On va utiliser la méthode `HasData` pour savoir si la `DB` a déjà des données.



## Demo

```bash
dotnet ef migrations add InitialMigration
dotnet ef database update
```

`Seed` des données dans `AppDbContext` :

```cs
public class AppDbContext : DbContext
{
    public AppDbContext(DbContextOptions<AppDbContext> options) : base(options)
    {

    }
    public DbSet<Pie> Pies { get; set; }
    public DbSet<Category> Categories { get; set; }
    
     protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            //seed categories
            modelBuilder.Entity<Category>().HasData(new Category { CategoryId = 1, CategoryName = "Fruit pies" });
            // ...

            //seed pies

            modelBuilder.Entity<Pie>().HasData(new Pie
            {
				// ...
```

`OnModelCreating` permet de configurer comment `EF Core` va construire la `DB`. Cette méthode permet aussi de générer (`seed`) des données.

```cs
modelBuilder.Entity<MyEntity>().HasData(new MyEntity { ... });
```

Ce code fonctionne avec les `migrations` :

```bash
dotnet ef migratiosn add SeedingData
```

`xxx_SeedingData.cs`

```cs
public partial class SeedingData : Migration
{
    protected override void Up(MigrationBuilder migrationBuilder)
    {
        migrationBuilder.InsertData(
            table: "Categories",
            columns: new[] { "CategoryId", "CategoryName", "Description" },
            values: new object[] { 1, "Fruit pies", null });
```

On voit que la `migration` contient des `InsertData`.

Il ne reste plus qu'a mettre à jour la `DB`.

```bash
dotnet ef datbase update
```























