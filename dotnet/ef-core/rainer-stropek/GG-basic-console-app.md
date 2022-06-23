# GG Une Application `Console` basique

On peut utiliser une application `console` pour tester son code `entity Framework`.

Voici un `template ` minimum.

```bash
dotnet new console
```

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer
```

```bash
dotnet add package Microsoft.EntityFrameworkCore.Design  
```

```cs
// See https://aka.ms/new-console-template for more information
using Microsoft.EntityFrameworkCore;

Console.WriteLine("Hello Entity Framework");

public class Pet
{
    public int Id { get; set; }
    public string Name { get; set; } = string.Empty;
}

public class Cat : Pet
{
    public List<Toys> Toys { get; set; } = new();
}

public class Dog : Pet
{
    public List<Food> PreferedFood { get; set; } = new();
}

public class PetContext : DbContext
{
    public DbSet<Dog> Dogs => Set<Dog>();
    public DbSet<Cat> Cats => Set<Cat>();
    
    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        optionsBuilder.UseSqlServer("Server=localhost,1433; Database=ef-inheritance; User=sa; Password=huk@r2Xmen99");
    }
}
```

Avec seulement ce la on peut lancer sa première `Migrations` :

```bash
✨ InheritanceEntityFramework : dotnet ef migrations add FirstMigration
Build started...
Build succeeded.
Done. To undo this action, use 'ef migrations remove'

✨ InheritanceEntityFramework : dotnet ef database update                                 
Build started...
Build succeeded.
Applying migration '20220502130453_FirstMigration'.
Done.
```

