# 07 Unit Of Work

## `IUnitOfWork`

Dans le dossier `IRepository` on va créer une nouvelle interface `IUnitOfWork.cs`

```cs
namespace IRepository
{
    public interface IUnitOfWork : IDisposable
    {
        IGenericRepository<Country> Countries { get; }
        IGenericRepository<Hotel> Hotels { get; }
        
        Task Save();
    }
}
```

Les methodes de `GenericRepository` mettent les changement de côté (`staged`), il faut en plus une méthode `Save` pour appliquer concrétement les changements en base de données.



## Implémentation : `UnitOfWork`

Dans le dossier `Repository` on va créer la classe `UnitOfWork.cs`.

On doit créer une référence vers `DataContext`.

On va créer aussi des champs privées de `_countries` et `_hostels`.

`??=` n'assigne que si le membre de gauche est `null`.

```cs
using System.Threading.Tasks;
using Data;
using IRepository;

namespace Repository
{
    public class UnitOfWork : IUnitOfWork
    {
        private readonly DataContext _context;
        private IGenericRepository<Country> _countries;
        private IGenericrepository<Hotel> _hotels;
        
        public UnitOfWork(DataContext context)
        {
            _context = context;
        }
        
        public IGenericRepository<Country> Countries 
            => _countries ??= new GenericRepository<Country>(_context);

        public IGenericRepository<Hotel> Hotels 
            => _hotels ??= new GenericRepository<Hotel>(_context);

        public void Dispose()
        {
            _context.Dispose();
            GC.SuppressFinalize(this);
        }

        public async Task Save()
        {
            await _context.SaveChangesAsync();
        }
    }
}
```

`Dispose` est avant tout pour le `Garbage Collector` pour libérer de la mémoire.

