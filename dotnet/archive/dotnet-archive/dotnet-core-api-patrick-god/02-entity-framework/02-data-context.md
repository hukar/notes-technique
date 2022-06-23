# 02 Data Context

## Création de `Data/DataContext.cs`

On va créer un nouveau dossier `Data`.

On y ajoute une nouvelle class `DataContext.cs`.

```cs
using dotnet_rpg.Models;
using Microsoft.EntityFrameworkCore;

namespace dotnet_rpg.Data
{
    public class DataContext : DbContext
    {
		public DataContext(DbContextOptions<DataContext> options) : base(options)
        {
            
        }
        
        public DbSet<Character> Characters { get; set; }
    }
}
```

`DbContext` représente une session avec la base de données.

`DbSet` offre une représentation en base de données du `Model` associé. Le nom du modèle au pluriel est un bon choix de nom de `DbSet`.



## Connection String

### Dans `appsettings.json`

```json
{
    "ConnectionStrings": {
        "HukarConnection": "Server=localhost,1433; Database=dotnet-rpg; User=sa; Password=huk@r2Xmen99"
    },
    // ...
}
```

`"ConnectionStrings"` suit une convention de nom qui sera réutilisée dans `Startup.cs`.

`"HukarConnection"` est un nom choisie arbitrairement.



### Dans `Startup.cs`

```cs
// ...
using dotnet_rpg.Data;
using Microsoft.EntityFrameworkCore;

public void ConfigureServices(IServiceCollection services)
{
    services.AddDbContext<DataContext>(options => 
         options.UseSqlServer(Configuration.GetConnectionString("HukarConnection")));
    // ...
}
```

